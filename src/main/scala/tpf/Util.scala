// See README.md for license details.

package tpf

import chisel3._
import chisel3.util._



object Util {
  def clz(x: Bits): SInt = {
    // horrifically unoptimised, please synthesis make this good
    // TODO: what happens when none?
    val tmp = Cat(1.U(1.W), x)
    chisel3.util.PriorityMux(tmp, (0 until tmp.getWidth) map (_.S(chisel3.util.signedBitLength(tmp.getWidth).W)))
  }
}