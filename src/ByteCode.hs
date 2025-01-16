module ByteCode where


pushVal :: Int
pushVal = 0x00

pushReg :: Int
pushReg = 0x01

pop :: Int
pop = 0x10

construct :: Int
construct = 0x20

test :: Int
test = 0x30

jt :: Int
jt = 0x40

jf :: Int
jf = 0x50

call :: Int
call = 0x60

retVal :: Int
retVal = 0x70

retReg :: Int
retReg = 0x71

movVal :: Int
movVal = 0x80

movReg :: Int
movReg = 0x81

outVal :: Int
outVal = 0x90

outReg :: Int
outReg = 0x91