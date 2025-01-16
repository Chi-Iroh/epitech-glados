module ByteCode where


pushVal :: Int
pushVal = 0x00

pushReg :: Int
pushReg = 0x01

pop :: Int
pop = 0x10

test :: Int
test = 0x20

jt :: Int
jt = 0x30

jf :: Int
jf = 0x40

call :: Int
call = 0x50

retVal :: Int
retVal = 0x60

retReg :: Int
retReg = 0x61

movVal :: Int
movVal = 0x70

movReg :: Int
movReg = 0x71

outVal :: Int
outVal = 0x80

outReg :: Int
outReg = 0x81