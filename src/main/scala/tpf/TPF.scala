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

object Posit {
  def apply(dp: DecodedPosit): Posit = {
    val p = Wire(new Posit(dp.size, dp.es))
    val expfrac = Cat(dp.exp, dp.frac)
    val regime = Wire(UInt((dp.size - 1).W))
    val regimebits = Wire(UInt(unsignedBitLength(dp.size - 1).W))
    val regimeexpfrac = Wire(UInt((dp.size - 1).W))
    when(dp.regime >= 0.S) {
      regimebits := dp.regime.asUInt()
      regime := (Cat(1.U(1.W), 0.U((dp.size - 2).W)).asSInt() >> (dp.regime).asUInt()).asUInt()
    }.otherwise {
      regimebits := (0.S - dp.regime).asUInt()
      regime := Cat(1.U(1.W), 0.U((dp.size - 2).W)).asUInt() >> (0.S - dp.regime).asUInt()
    }

    regimeexpfrac := regime | Cat(expfrac >> regimebits)
    p.bits := Cat(dp.sign, regimeexpfrac)
    p
  }
}

class DecodedPosit(val size: Int = 32, val es: Int = 2) extends Bundle {
  val sign = Bool()
  val regime = SInt(signedBitLength(size - 1).W)
  val exp = UInt(es.W)
  val frac = UInt((size - 1 - 2 - es).W)
  val isZero = Bool()
  val isInf = Bool()

  val fracsize: Int = size - 1 - 2 - es
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
    val start = Wire(UInt(unsignedBitLength(p.size).W))
    //printf("%b clz: %x clo: %x", p.bits, clz, clo)
    //dontTouch(clz)
    //dontTouch(clo)

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
    when(p.bits(p.size - 2, 0) === 0.U) {
      // special
      dp.isZero := ~p.bits(p.size - 1)
      dp.isInf := p.bits(p.size - 1)
    }

    dp
  }

  implicit class AddMethodsToDecodedPosit(target: DecodedPosit) {
    def isSpe: Bool = {
      target.isZero | target.isInf
    }

    def regime_biased: UInt = {
      assert(target.regime +& (target.size - 2).S >= 0.S)
      (target.regime +& (target.size - 2).S).asUInt()
    } // max value is 2 * (width - 2) - 1 => 2 * width - 5

    def regime_exp: UInt = {
      Cat(target.regime_biased, target.exp)
    } // max value is maxregime << es + maxexp
    // 2 * width * 2^es - 5 * 2^es + 2^es - 1
    // 2^es * (2 * width - 4) - 1
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

  val in_readyctr = Counter(1)
  in_readyctr.inc() // always inc
  io.in_ready := in_readyctr.value === 0.U // only every 4 cycles, so have enough time to accum

  val x = Reg(new Posit(size, es))
  val y = Reg(new Posit(size, es))
  val xyvalid = RegNext(io.in_valid && io.in_ready, init = false.B)
  when(io.in_valid && io.in_ready) {
    x.bits := io.in_data1
    y.bits := io.in_data2
  }

  // stage 1: decode
  val decvalid = RegNext(xyvalid, init = false.B)
  val x_dec = Reg(new DecodedPosit(size, es))
  val y_dec = Reg(new DecodedPosit(size, es))
  x_dec := DecodedPosit(x)
  y_dec := DecodedPosit(y)

  // for now, just decode
  dontTouch(x_dec)
  dontTouch(y_dec)

  // stage 2: multiply
  val mulvalid = RegNext(decvalid, init = false.B)
  val frac_prod = RegNext(Cat(1.U, x_dec.frac) * Cat(1.U, y_dec.frac)) // the 1.U are implicit one
  val regimeexp_sum = RegNext(x_dec.regime_exp +& y_dec.regime_exp) // note: biased
  // max value is 2 * (2^es * (2 * width - 2) - 1)
  val sign_xor = RegNext(x_dec.sign ^ y_dec.sign)
  val mulzero = RegNext(x_dec.isZero | y_dec.isZero)
  val mulinf = RegNext(x_dec.isInf | y_dec.isInf) // could have both zero and inf, undefined output

  // stage 3: fixup
  // need to check whether exp overflows into regime/exp
  val fracprodwidth = 2 * (1 + (size - 1 - 2 - es))
  val left_bits = frac_prod(fracprodwidth - 1, fracprodwidth - 2)
  // top 2 bits, to the left of the binary point
  val right_bits = frac_prod(fracprodwidth - 3, 0) // bits to right of binary point

  val regimeexp_carried = regimeexp_sum +& left_bits(1)
  // max value is 2 * (2^es * (2 * width - 2) - 1) + 1
  val right_bits_carried = Wire(UInt())
  when(left_bits(1)) {
    right_bits_carried := Cat(left_bits(0), right_bits)
  }.otherwise {
    right_bits_carried := Cat(right_bits, 0.U(1.W))
  }
  val frac_sint = Wire(SInt())
  frac_sint := Cat(1.U(2.W), right_bits_carried).asSInt()

  val frac_signed = Wire(SInt())
  frac_signed := Mux(sign_xor, 0.S -& frac_sint, frac_sint)

  val mulout_valid = RegNext(mulvalid, init = false.B)
  val mulout_regimeexp = RegNext(regimeexp_carried)
  val mulout_frac = RegNext(frac_signed)
  val mulout_inf = RegNext(mulinf)

  dontTouch(mulout_regimeexp)
  dontTouch(mulout_frac)
  dontTouch(mulout_inf)

  val quiresize = 2 * es * (4 * size - 8) + 2 // looks right size
  val bitscanignore = 2 * (size - 1 - 2 - es) + 1 // these bits are always zero in fracaligned, so we don't have them
  val quire = RegInit(0.S(quiresize.W))
  //val quireInf = RegInit(0.U) // Disabled
  dontTouch(quire)
  //dontTouch(quireInf)

  // stage 4: align
  val fracaligned = Reg(SInt(quiresize.W))
  val alignvalid = RegNext(mulout_valid, init = false.B)
  //val frac = Cat(1.U, mulout_frac) // done earlier to make signed
  fracaligned := (mulout_frac << mulout_regimeexp) >> bitscanignore
  dontTouch(fracaligned)

  // stage 5? add - align and this may need to take more cycles
  val sum = Wire(SInt())
  sum := quire +& fracaligned
  when(alignvalid) {
    //quireInf := quireInf | sum(quiresize) // if top bit is set, we overflow
    quire := sum
  }

  // stage x: abs, sign
  val done = RegNext(RegNext(RegNext(RegNext(RegNext(RegNext(io.in_done),
    init = false.B), init = false.B), init = false.B), init = false.B), init = false.B) // delayed by 6 cycles
  val quireAbs = RegInit(0.S(quiresize.W))
  quireAbs := Mux(quire(quiresize - 1), 0.S - quire, quire)
  val quireSign = RegNext(quire(quiresize - 1))

  // stage x+1: shift out

  val quireAbsReverse = Reverse(quireAbs.asUInt())
  val firstOne = Util.clz(quireAbsReverse).asUInt()
  val fracbits = (quireAbsReverse >> firstOne) (x_dec.fracsize + 1, 1)
  // start with 1 to ignore implicit 1
  // add 1 so have extra bits for rounding

  val moreBits = (quireAbsReverse >> firstOne)(quiresize - 1, x_dec.fracsize + 2).orR

  val regimeexpbiased = (quiresize - 1).U - firstOne
  val exp = regimeexpbiased(es - 1, 0)
  val regimeraw = Cat(0.U(1.W), regimeexpbiased >> es).asSInt() -& (2 * size - 4).S

  val shiftValid = RegNext(done, init = false.B)
  val shiftFrac = RegNext(Reverse(fracbits))
  val shiftExp = RegNext(exp)
  val shiftRegime = RegNext(regimeraw)
  val shiftZero = RegNext(quireAbs === 0.S)
  val shiftSign = RegNext(quireSign)
  val shiftMore = RegNext(moreBits)

  // stage x+2: round and cap, encode

  val roundValid = RegNext(shiftValid, init = false.B)

  val regimecapped = Mux(shiftRegime > (size - 2).S, (size - 2).S,
    Mux(shiftRegime < (-(size - 2)).S, (-(size - 2)).S, shiftRegime))
  val expfrac = Wire(UInt((size - 0).W))
  expfrac := Cat(shiftExp, shiftFrac) // note: last bit is only for rounding
  val regimeencoded = Wire(UInt((size - 1).W))
  val regimebitlength = Wire(UInt(unsignedBitLength(size - 1).W)) // n.b. not actual bit length
  val regimeexpfrac = Wire(UInt((size - 1).W))
  val absregimecapped = Mux(shiftRegime >= 0.S, regimecapped.asUInt(), (0.S - regimecapped).asUInt())
  regimebitlength := absregimecapped
  when(shiftRegime >= 0.S) {
    regimeencoded := (Cat(1.U(1.W), 0.U((size - 2).W)).asSInt() >> (regimecapped).asUInt()).asUInt()
  }.otherwise {
    regimeencoded := Cat(1.U(1.W), 0.U((size - 2).W)).asUInt() >> (0.S - regimecapped).asUInt()
  }
  val expfracMore = (Reverse(expfrac) << regimebitlength)(2 * size - 2, size).orR
  val moreMore = shiftMore | expfracMore
  val expfracshifted = expfrac >> regimebitlength
  val nextbit = expfracshifted(0)
  regimeexpfrac := regimeencoded | expfracshifted(size - 1, 1) // last bit is nextbit, so don't take it
  val regimeexpfracrounded = Mux(nextbit & (moreMore | regimeexpfrac(0)) & !regimeexpfrac.andR,
    regimeexpfrac +% 1.U ,regimeexpfrac)
  val refr = regimeexpfracrounded

  val signedrefr = Cat(shiftSign, Mux(shiftSign, Cat(~refr) +% 1.U, refr))
  // stage x+3: output
  val outlatch = RegNext(signedrefr)
  val outvalid = RegNext(roundValid, init = false.B)
  io.out_valid := outvalid
  io.out_data := outlatch

  val debugp = Wire(new Posit(size, es))
  debugp.bits := outlatch
  val debughelp = DecodedPosit(debugp)
  dontTouch(debughelp)
}
