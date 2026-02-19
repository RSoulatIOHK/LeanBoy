/*
 * sdl_frontend.c – SDL2 display shim for LeanBoy.
 * All functions use Lean's C FFI calling convention.
 *
 * Debug output is controlled by the LEANBOY_DEBUG environment variable
 * (same flag as the Lean side).  Set to 1 or higher to enable.
 */

#include <lean/lean.h>
#include <SDL2/SDL.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define GB_W  160
#define GB_H  144
#define SCALE 3

static SDL_Window   *g_window   = NULL;
static SDL_Renderer *g_renderer = NULL;
static SDL_Texture  *g_texture  = NULL;

/* debug level – read from LEANBOY_DEBUG once at init */
static int g_debug = 0;

#define DBG(level, ...) \
    do { if (g_debug >= (level)) { fprintf(stderr, "[SDL] " __VA_ARGS__); fputc('\n', stderr); } } while(0)

/* lean_sdl_init : IO Int32 */
lean_obj_res lean_sdl_init(lean_obj_arg world) {
    const char *env = getenv("LEANBOY_DEBUG");
    if (env) g_debug = atoi(env);

    DBG(1, "init  scale=%d  window=%dx%d  debug=%d",
        SCALE, GB_W * SCALE, GB_H * SCALE, g_debug);

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        DBG(1, "SDL_Init failed: %s", SDL_GetError());
        return lean_io_result_mk_ok(lean_box(0xFFFFFFFF));
    }
    g_window = SDL_CreateWindow("LeanBoy",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        GB_W * SCALE, GB_H * SCALE, SDL_WINDOW_SHOWN);
    if (!g_window) {
        DBG(1, "SDL_CreateWindow failed: %s", SDL_GetError());
        SDL_Quit();
        return lean_io_result_mk_ok(lean_box(0xFFFFFFFF));
    }
    g_renderer = SDL_CreateRenderer(g_window, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!g_renderer) {
        DBG(1, "SDL_CreateRenderer failed: %s", SDL_GetError());
        SDL_DestroyWindow(g_window); SDL_Quit();
        return lean_io_result_mk_ok(lean_box(0xFFFFFFFF));
    }
    g_texture = SDL_CreateTexture(g_renderer,
        SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STREAMING, GB_W, GB_H);
    if (!g_texture) {
        DBG(1, "SDL_CreateTexture failed: %s", SDL_GetError());
        SDL_DestroyRenderer(g_renderer); SDL_DestroyWindow(g_window); SDL_Quit();
        return lean_io_result_mk_ok(lean_box(0xFFFFFFFF));
    }
    DBG(1, "init OK");
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_sdl_destroy : IO Unit */
lean_obj_res lean_sdl_destroy(lean_obj_arg world) {
    DBG(1, "destroy");
    if (g_texture)  { SDL_DestroyTexture(g_texture);   g_texture  = NULL; }
    if (g_renderer) { SDL_DestroyRenderer(g_renderer); g_renderer = NULL; }
    if (g_window)   { SDL_DestroyWindow(g_window);     g_window   = NULL; }
    SDL_Quit();
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_sdl_present_frame : ByteArray -> IO Unit */
lean_obj_res lean_sdl_present_frame(lean_obj_arg arr, lean_obj_arg world) {
    static uint64_t s_frame_count = 0;
    size_t size = lean_sarray_size(arr);
    const uint8_t *pixels = lean_sarray_cptr(arr);

    if (!g_texture) {
        DBG(1, "present_frame: no texture");
        return lean_io_result_mk_ok(lean_box(0));
    }
    if (size < (size_t)(GB_W * GB_H)) {
        fprintf(stderr, "[SDL] present_frame: buffer too small! got=%zu expected=%d\n",
                size, GB_W * GB_H);
        return lean_io_result_mk_ok(lean_box(0));
    }

    s_frame_count++;

    /* At level 2: sample some pixel values for visual diagnostics */
    if (g_debug >= 2) {
        fprintf(stderr,
            "[SDL] frame %llu  px(0,0)=%u px(0,80)=%u px(72,0)=%u px(72,80)=%u\n",
            (unsigned long long)s_frame_count,
            pixels[0],
            pixels[80],
            pixels[72 * GB_W],
            pixels[72 * GB_W + 80]);
    } else if (g_debug >= 1 && s_frame_count % 60 == 0) {
        fprintf(stderr, "[SDL] frame %llu presented\n",
                (unsigned long long)s_frame_count);
    }

    /* Convert grayscale to RGB24 */
    uint8_t rgb[GB_W * GB_H * 3];
    for (int i = 0; i < GB_W * GB_H; i++) {
        uint8_t g = pixels[i];
        rgb[i*3+0] = g; rgb[i*3+1] = g; rgb[i*3+2] = g;
    }
    SDL_UpdateTexture(g_texture, NULL, rgb, GB_W * 3);
    SDL_RenderClear(g_renderer);
    SDL_RenderCopy(g_renderer, g_texture, NULL, NULL);
    SDL_RenderPresent(g_renderer);
    return lean_io_result_mk_ok(lean_box(0));
}

/* lean_sdl_poll_events : IO UInt32 */
lean_obj_res lean_sdl_poll_events(lean_obj_arg world) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) {
            DBG(1, "SDL_QUIT received");
            return lean_io_result_mk_ok(lean_box(1));
        }
        if (e.type == SDL_KEYDOWN && e.key.keysym.sym == SDLK_ESCAPE) {
            DBG(1, "ESC pressed");
            return lean_io_result_mk_ok(lean_box(1));
        }
    }
    const uint8_t *ks = SDL_GetKeyboardState(NULL);
    uint32_t mask = 0;
    if (ks[SDL_SCANCODE_RIGHT])     mask |= (1u << 1);
    if (ks[SDL_SCANCODE_LEFT])      mask |= (1u << 2);
    if (ks[SDL_SCANCODE_UP])        mask |= (1u << 3);
    if (ks[SDL_SCANCODE_DOWN])      mask |= (1u << 4);
    if (ks[SDL_SCANCODE_Z])         mask |= (1u << 5);
    if (ks[SDL_SCANCODE_X])         mask |= (1u << 6);
    if (ks[SDL_SCANCODE_BACKSPACE]) mask |= (1u << 7);
    if (ks[SDL_SCANCODE_RETURN])    mask |= (1u << 8);
    return lean_io_result_mk_ok(lean_box(mask));
}
