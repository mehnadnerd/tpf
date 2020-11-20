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

# this python thing has negative numbers wrong....

p2 = sp.posit32(bits=0x57123456)
p2.toBinary()
print(p2)
