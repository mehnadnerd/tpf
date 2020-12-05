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

b1 = [0x48000000,
      0x44000000,
      0x47000000,
      0x70000000,
      0x47000000
      ]

b2 = [0x28000000,
      0x24000000,
      0x27000000,
      0x38000000,
      0x27000000
      ]

p = [sp.posit32(bits=a) for a in b1]
p2 = [sp.posit32(bits=a) for a in b2]

accum = sp.posit32(bits=0)
for i in range(len(p)):
    print(str(p[i]) + " * " + str(p2[i]))
    accum += p[i] * p2[i]
print(accum)  # note: not at infinite precision
print(accum.toHex())  # note: not at infinite precision

waitline = "		@(posedge clock); #1;"
line1 = "		io_in_data1 = 32'h{0}; // {1}"
line2 = "		io_in_data2 = 32'h{0}; // {1}"

verilog = False

if verilog:
    for i in range(len(p)):
        print(line1.format(hex(b1[i]), str( p[i])))
        print(line2.format(hex(b2[i]), str(p2[i])))
        print(waitline)