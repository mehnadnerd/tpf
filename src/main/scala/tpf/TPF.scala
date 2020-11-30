// See README.md for license details.

package tpf

import chisel3._
import chisel3.util._

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
  val isZero = Bool()
  val isInf = Bool()
  val isSpe = isZero | isInf
}

object DecodedPosit {
  // sign * useed ^ regime * 2 ^ exp * 1.frac
  // used = 2^(2^es)
  def apply(p: Posit): DecodedPosit = {
    val dp = Wire(new DecodedPosit(p.size, p.es))
    dp.sign := p.bits(p.size - 1)
    // TODO: what are we taking the two's complemetns of for negative numbers?
    val rest = Mux(dp.sign, Cat(~p.bits(p.size - 2, 0)) +% 1.U, p.bits(p.size - 2, 0))
    val loz = p.bits(p.size - 2) // leading one or zero. if one, regime positive, zero regime negative
    val rrest = chisel3.util.Reverse(rest)

    val clz = Util.clz(rrest)
    val clo = Util.clz(~rrest)
    val start = Wire(UInt(unsignedBitLength(p.size - 1).W))
    printf("%b %x", p.bits, start)

    when(loz) {
      // leading digit one
      dp.regime := clo -& 1.S
      start := clo.asUInt() +& 1.U
    }.otherwise {
      dp.regime := 0.S - clz
      start := clz.asUInt() +& 1.U
    }

    val zrrest: UInt = Cat(0.U(p.size.W), rrest)
    val expbits = (zrrest >> start) (p.es - 1, 0)
    dp.exp := Reverse(expbits)
    val fracbits = (zrrest >> (start +& p.es.U)) (p.size - 1 - 2 - p.es - 1, 0)
    dp.frac := Reverse(fracbits)

    dp.isZero := false.B
    dp.isInf := false.B
    when (p.bits(p.size - 2, 0) === 0.U) {
      // special
      dp.isZero := ~p.bits(p.size - 1)
      dp.isInf := p.bits(p.size - 1)
    }

    dp
  }
}

/**
  * Dot product calculator. Takes in two posits, and accumulates them in a quire.
  *
  */
class TPF(size: Int = 32, es: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in_ready = Output(Bool())
    val in_valid = Input(Bool())
    val in_data1 = Input(UInt(size.W))
    val in_data2 = Input(UInt(size.W))
    val in_done = Input(Bool())
    val out_valid = Output(Bool())
    val out_data = Output(UInt(size.W))
  })

  val in_readyctr = Counter(4)
  in_readyctr.inc() // always inc
  io.in_ready := in_readyctr.value === 0.U // only every 4 cycles, so have enough time to accum

  val x = Reg(new Posit(size, es))
  val y = Reg(new Posit(size, es))
  val xyvalid = RegNext(io.in_valid && io.in_ready)
  when(io.in_valid && io.in_ready) {
    x.bits := io.in_data1
    y.bits := io.in_data2
  }

  // stage 1: decode
  val decvalid = RegNext(xyvalid)
  val x_dec = Reg(new DecodedPosit(size, es))
  val y_dec = Reg(new DecodedPosit(size, es))
  x_dec := DecodedPosit(x)
  y_dec := DecodedPosit(y)

  // for now, just decode
  dontTouch(x_dec)
  dontTouch(y_dec)

  // stage 2: multiply
  val mulvalid = RegNext(decvalid)
  val frac_prod = RegNext(Cat(1.U, x_dec.frac) * Cat(1.U, y_dec.frac)) // the 1.U are implicit one
  val regime_sum = RegNext(x_dec.regime +& y_dec.regime)
  val exp_sum = RegNext(x_dec.exp +& y_dec.exp)
  val sign_xor = RegNext(x_dec.sign ^ y_dec.sign)
  val mulzero = RegNext(x_dec.isZero | y_dec.isZero)
  val mulinf = RegNext(x_dec.isInf | y_dec.isInf) // could have both zero and inf, undefined output

  // stage 3: fixup
  // need to check whether exp overflows into regime
  // also need to see whether frac causes change in exp
  // also round frac -- careful not to double round
  val fracprodwidth = 2 * (1 + (size - 1 - 2 - es))
  val left_bits = frac_prod(fracprodwidth - 1, fracprodwidth - 2)
  // top 2 bits, to the left of the binary point
  val right_bits = frac_prod(fracprodwidth - 3, 0) // bits to right of binary point

  val exp_carried = exp_sum +& left_bits(1)
  //val left_bits_carried = Wire(UInt(1.W)) // not needed, always one
  val right_bits_carried = Wire(UInt())
  when (left_bits(1)) {
    //left_bits_carried := left_bits(1)
    right_bits_carried := Cat(left_bits(0), right_bits)
  } .otherwise {
    //left_bits_carried := left_bits(0)
    right_bits_carried := Cat(right_bits, 0.U(1.W))
  }

  val regime_adj = Wire(SInt())
  val exp_adj = Wire(UInt(es.W))
  val max_exp = 3.U // TODO: not generic on es
  when (exp_carried >= max_exp) {
    // can't be >= 2 * max_exp, so this is safe
    assert(exp_carried - max_exp < max_exp)
    regime_adj := regime_sum +& 1.S
    exp_adj := exp_carried - max_exp
  } .otherwise {
    regime_adj := regime_sum
    exp_adj := exp_carried
  }

  val mulout_valid = RegNext(mulvalid)
  val mulout_sign = RegNext(sign_xor)
  val mulout_regime = RegNext(regime_adj)
  val mulout_exp = RegNext(exp_adj)
  val mulout_frac = RegNext(right_bits_carried)
  val mulout_zero = RegNext(mulzero)
  val mulout_inf = RegNext(mulinf)

  dontTouch(mulout_sign)
  dontTouch(mulout_regime)
  dontTouch(mulout_exp)
  dontTouch(mulout_frac)
  dontTouch(mulout_zero)
  dontTouch(mulout_inf)

  // not doing normalising here
  // stage 4: add
  // used ^ (4n - 8)
  // used = 2^(2^es)
  // quire size = 2^2^es^(4 * size - 8) + 2
  val minregime = (size - 1) // actual is negative, but we would subtract it
  assert(mulout_regime +& minregime.S >= 0.S)
  val biasedregime = (mulout_regime +& minregime.S).asUInt() // is positive at this point
  val expanded_exp = Cat(biasedregime, mulout_exp) // note: biased
  // max regime is 30,

  val quiresize = 2 * es * (4 * size - 8) + 2 // TODO: need more?
  val quiresize2 = (1 + (size - 1 - 2 - es)) + (1 << 2)
  val quire = RegInit(0.U(quiresize.W))
  val quireInf = RegInit(0.U)

  val fracaligned = Wire(UInt(1.W))
  val frac = Cat(1.U, mulout_frac)

  // regime are basically extra exp bits here (actually before as well)


  io.out_valid := DontCare
  io.out_data := DontCare
}
