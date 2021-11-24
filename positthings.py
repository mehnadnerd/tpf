#!/usr/local/bin/python3

import softposit as sp
import softfloat as sf

# p = sp.posit32(1.0)
# p8 = sp.posit8(-1.0)
# p16 = sp.posit16(-1.0)
# p.toBinary()
# p8.toBinary()
# p16.toBinary()
# print(p)
#
# pn = -p
# pn.toBinary()
# print(pn)

#b = [0xc0000000, 0xae000000, 0xa2000000, 0x9ff00000, 0xd8000000]
b = [0xc000, 0xae00, 0xa200, 0x9ff0, 0xd800]
#b = [0x4000, 0x5200, 0x5e00, 0x6010, 0x6408]
p = [sp.posit32(bits=a * 0x10000) for a in b]
for i in range(len(b)):
    print(str(p[i]))
