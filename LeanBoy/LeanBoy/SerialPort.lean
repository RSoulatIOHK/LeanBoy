/-!
# Serial Port

Minimal implementation of the Game Boy serial port (0xFF01–0xFF02).
Primarily useful for capturing output from Blargg test ROMs, which
print their results via the serial port.
-/

namespace LeanBoy

structure SerialPort where
  sb : UInt8 := 0x00   -- 0xFF01: serial data byte
  sc : UInt8 := 0x7E   -- 0xFF02: serial control
  deriving Repr

namespace SerialPort

def readByte (sp : SerialPort) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF01 => sp.sb
  | 0xFF02 => sp.sc ||| 0x7E
  | _      => 0xFF

-- When a write to SC with bit 7 set occurs, the byte in SB is "sent".
-- We print it to stdout so Blargg tests can be observed.
def writeByte (sp : SerialPort) (addr : UInt16) (v : UInt8) (printOutput : Bool) :
    IO SerialPort := do
  match addr with
  | 0xFF01 => return { sp with sb := v }
  | 0xFF02 =>
    let sp' := { sp with sc := v }
    if printOutput && v &&& 0x80 != 0 then
      IO.print (Char.ofNat sp.sb.toNat)
    return sp'
  | _ => return sp

end SerialPort

end LeanBoy
