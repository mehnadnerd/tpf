// See README.md for license details.

package tpf

import chisel3._



object Util {
  def clz(x: Bits): SInt = {
    // horrifically unoptimised, please synthesis make this good
    // TODO: what happens when none?
    chisel3.util.PriorityMux(x, (0 until x.getWidth) map (_.S(chisel3.util.signedBitLength(x.getWidth).W)))
  }
}