// See README.md for license details.

package tpf

import chisel3._
import chisel3.util._
import firrtl.transforms.DontTouchAnnotation

class Posit(val size: Int = 32, val es: Int = 2) extends Bundle {
  val bits = UInt(size.W)

  // derived things
  // regime = run length encoding, signed, max bits = +-log2(size-1)
  // exponent, max bits = es
  // fraction, max bits = size - 1 - 2 - es
}

class DecodedPosit(val size: Int = 32, val es: Int = 2) extends Bundle {
  val sign = Bool()
  val regime = SInt(signedBitLength(size - 1).W)
  val exp = UInt(es.W)
  val frac = UInt((size - 1 - 2 - es).W)
}

object DecodedPosit {
  def apply(p: Posit): DecodedPosit = {
    val dp = Wire(new DecodedPosit(p.size, p.es))
    dp.sign := p.bits(p.size - 1)
    // TODO: what are we taking the two's complemetns of for negative numbers?
    val rest = Mux(dp.sign, Cat(~p.bits(p.size - 2, 0)) +% 1.U, p.bits(p.size - 2, 0))
    val loz = p.bits(p.size - 2) // leading one or zero. if one, regime positive, zero regime negative
    val rrest = chisel3.util.Reverse(rest)

    //TODO: need to setup clz properly, it starts from bit 0
    val clz = Util.clz( rrest)
    val clo = Util.clz(~rrest)
    val start = Wire(UInt(unsignedBitLength(p.size - 1).W))
    printf("%b %x", p.bits, start)

    when (loz) {
      // leading digit one
      dp.regime := clo -& 1.S
      start := clo.asUInt() +& 1.U
    } .otherwise {
      dp.regime := 0.S - clz
      start := clz.asUInt() +& 1.U
    }

    val zrrest: UInt = Cat(0.U(p.size.W), rrest)
    val expbits = (zrrest >> start)(p.es - 1, 0)
    dp.exp := Reverse(expbits)
    val fracbits = (zrrest >> (start +& p.es.U))(p.size - 1 - 2 - p.es - 1, 0)
    dp.frac := Reverse(fracbits)


    dp
  }
}

/**
  * Dot product calculator. Takes in two posits, and accumulates them in a quire.
  *
  */
class TPF(size: Int = 32, es: Int = 2, quire: Int = 1024) extends Module {
  val io = IO(new Bundle {
    val in_valid      = Input(Bool())
    val in_data1      = Input(UInt(size.W))
    val in_data2      = Input(UInt(size.W))
    val in_done       = Input(Bool())
    val out_valid     = Output(Bool())
    val out_data      = Output(UInt(size.W))
  })
  // size = 16, quire = 128
  // size = 32, quire = 1024
  // size = 64, quire = 4096

  val x = Reg(new Posit(size, es))
  val y = Reg(new Posit(size, es))
  val xyvalid = RegNext(io.in_valid)
  when (io.in_valid) {
    x.bits := io.in_data1
    y.bits := io.in_data2
  }

  val x_dec = Reg(new DecodedPosit(size, es))
  val y_dec = Reg(new DecodedPosit(size, es))
  x_dec := DecodedPosit(x)
  y_dec := DecodedPosit(y)

  // for now, just decode
  dontTouch(x_dec)
  dontTouch(y_dec)

  io.out_valid := DontCare
  io.out_data := DontCare
}
