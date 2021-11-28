// See README.md for license details.

package tpf

import chisel3._
import chisel3.util._

class Float(val size: Int = 32, val expBits: Int = 8) extends Bundle {
  val bits = UInt(size.W)
}

object Float {
  def apply(dp: DecodedFloat): Float = {
    val p = Wire(new Float(dp.size, dp.expBits))
    // only used for debug so not done yet, hard part is denormals
    //    val expfrac = Cat(dp.exp, dp.frac)
    //    val regime = Wire(UInt((dp.size - 1).W))
    //    val regimebits = Wire(UInt(unsignedBitLength(dp.size - 1).W))
    //    val expfrac = Wire(UInt((dp.size - 1).W))
    //
    //    when(dp.regime >= 0.S) {
    //      regimebits := dp.regime.asUInt()
    //      regime := (Cat(1.U(1.W), 0.U((dp.size - 2).W)).asSInt() >> (dp.regime).asUInt()).asUInt()
    //    }.otherwise {
    //      regimebits := (0.S - dp.regime).asUInt()
    //      regime := Cat(1.U(1.W), 0.U((dp.size - 2).W)).asUInt() >> (0.S - dp.regime).asUInt()
    //    }
    //
    //    expfrac := regime | Cat(expfrac >> regimebits)
    //    p.bits := Cat(dp.sign, expfrac)
    p
  }
}

class DecodedFloat(val size: Int = 32, val expBits: Int = 8) extends Bundle {
  val sign = Bool()
  val exp = UInt(log2Up((1 << expBits) + (size - expBits - 1)).W) // NOTE: is biased, both with the IEEE and denormaloffset
  val frac = UInt((size - 1 - expBits).W)
  val isZero = Bool()
  val isInf = Bool()

  val fracsize: Int = size - 1 - expBits
  val ieeebias: Int = (1 << (expBits - 1)) - 1
  val denormbias: Int = fracsize - 1 // TODO: check??
  val maxexpdist: Int = (((1 << expBits) - 1) + denormbias)
}

object DecodedFloat {
  // sign * useed ^ regime * 2 ^ exp * 1.frac
  // used = 2^(2^expBits)
  def apply(p: Float): DecodedFloat = {
    val dp = Wire(new DecodedFloat(p.size, p.expBits))
    dp.sign := p.bits(p.size - 1)
    val rawexp = p.bits(p.size - 2, dp.fracsize) // todo check rawexp ad rawfrac
    val rawfrac = p.bits(dp.fracsize - 1, 0)

    // non-denormal
    val normalexp = rawexp +& dp.denormbias.U
    val normalfrac = rawfrac
    // denormal, not fun
    val nlz = Util.clz(Reverse(rawfrac)).asUInt() // asuint is safe b/c always >= 0
    val denormalexp = dp.denormbias.U -& nlz
    val denormalfrac = rawfrac << (nlz.asUInt() + 1.U)

    when(rawexp === 0.U) {
      dp.exp := denormalexp
      dp.frac := denormalfrac
    }.otherwise {
      dp.exp := normalexp
      dp.frac := normalfrac
    }
    dp.isZero := false.B
    dp.isInf := false.B
    when(rawexp === 0.U && rawfrac === 0.U) {
      // special, zero
      dp.isZero := true.B
    }
    when(rawexp.andR()) {
      // infinity or NaN, we just call NNaN inf
      dp.isInf := true.B
    }

    dp
  }

  implicit class AddMethodsToDecodedFloat(target: DecodedFloat) {
    def isSpe: Bool = {
      target.isZero | target.isInf
    }
  }

}

/**
  * Dot product calculator. Takes in two posits, and accumulates them in a quire.
  *
  */
class TIF(size: Int = 32, expBits: Int = 2) extends Module {
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

  val x = Reg(new Float(size, expBits))
  val y = Reg(new Float(size, expBits))
  val xyvalid = RegNext(io.in_valid && io.in_ready, init = false.B)
  when(io.in_valid && io.in_ready) {
    x.bits := io.in_data1
    y.bits := io.in_data2
  }

  // stage 1: decode
  val decvalid = RegNext(xyvalid, init = false.B)
  val x_dec = Reg(new DecodedFloat(size, expBits))
  val y_dec = Reg(new DecodedFloat(size, expBits))
  x_dec := DecodedFloat(x)
  y_dec := DecodedFloat(y)

  // for now, just decode
  dontTouch(x_dec)
  dontTouch(y_dec)
  cover(decvalid, "COVER decvalid")


  // stage 2: multiply
  val mulvalid = RegNext(decvalid, init = false.B)
  val frac_prod = RegNext(Cat(1.U, x_dec.frac) * Cat(1.U, y_dec.frac)) // the 1.U are implicit one
  val exp_sum = RegNext(x_dec.exp +& y_dec.exp)
  val sign_xor = RegNext(x_dec.sign ^ y_dec.sign)
  cover(mulvalid & sign_xor, "COVER mul negative")
  cover(mulvalid & !sign_xor, "COVER mul positive")
  val mulzero = RegNext(x_dec.isZero | y_dec.isZero)
  cover(mulvalid & mulzero, "COVER mul zero")
  val mulinf = RegNext(x_dec.isInf | y_dec.isInf) // could have both zero and inf, undefined output

  // stage 3: fixup
  // need to check whether frac overflows into exp
  val fracprodwidth = 2 * (1 + (size - 1 - expBits))
  val left_bits = frac_prod(fracprodwidth - 1, fracprodwidth - 2)
  // top 2 bits, to the left of the binary point
  val right_bits = frac_prod(fracprodwidth - 3, 0) // bits to right of binary point

  val exp_carried = exp_sum +& left_bits(1)
  val right_bits_carried = Wire(UInt())
  when(left_bits(1)) {
    right_bits_carried := Cat(left_bits(0), right_bits)
  }.otherwise {
    right_bits_carried := Cat(right_bits, 0.U(1.W))
  }
  cover(mulvalid & left_bits(1), "COVER leftbits1 true") // indicates carry into exp
  cover(mulvalid & !left_bits(1), "COVER leftbits1 false")
  val frac_sint = Wire(SInt())
  frac_sint := Cat(1.U(2.W), right_bits_carried).asSInt()

  val frac_signed = Wire(SInt())
  frac_signed := Mux(sign_xor, 0.S -& frac_sint, frac_sint)

  val mulout_valid = RegNext(mulvalid, init = false.B)
  val mulout_exp = RegNext(exp_carried)
  val mulout_frac = RegNext(frac_signed)
  val mulout_inf = RegNext(mulinf)
  val mulout_zero = RegNext(mulzero)

  dontTouch(mulout_exp)
  dontTouch(mulout_frac)
  dontTouch(mulout_inf)
  dontTouch(mulout_zero)

  // min is exp
  val quiresize = x_dec.maxexpdist + y_dec.maxexpdist + 3 // looks right size
  val bitscanignore = 1 + x_dec.denormbias + y_dec.denormbias // these bits are always zero in fracaligned, so we don't have them
  // this is because at the smallest number, it is denormal and so these are all zero

  val quire = RegInit(0.S(quiresize.W))
  //val quireInf = RegInit(0.U) // Disabled
  dontTouch(quire)
  //dontTouch(quireInf)

  // stage 4: align
  val fracaligned = Reg(SInt(quiresize.W))
  val alignvalid = RegNext(mulout_valid, init = false.B)
  //val frac = Cat(1.U, mulout_frac) // done earlier to make signed
  fracaligned := Mux(mulout_zero, 0.S, (mulout_frac << mulout_exp) >> bitscanignore)
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
  val fracbits = (quireAbsReverse >> firstOne) (x_dec.fracsize + 3, 1)
  // start with 1 to ignore implicit 1
  // add 3 so have extra bits for rounding

  val moreBits = (quireAbsReverse >> firstOne) (quiresize - 1, x_dec.fracsize + 4)

  val expraw = (quiresize - 1).U - firstOne // both ieee and deormal biased

  val shiftValid = RegNext(done, init = false.B)
  val shiftFrac = RegNext(Reverse(fracbits))
  val shiftExp = RegNext(expraw)
  val shiftZero = RegNext(quireAbs === 0.S)
  val shiftSign = RegNext(quireSign)
  val shiftMore = RegNext(moreBits)

  // stage x+2: round and cap, encode

  val roundValid = RegNext(shiftValid, init = false.B)

  // TODO: handle denormal output, rounding
  val biased_exp = shiftExp -& (x_dec.ieeebias + x_dec.denormbias + 2).U // biased with only one ieee and denorm bias // no idea where the 2 comes from
  val flushable = shiftExp < (x_dec.ieeebias + x_dec.denormbias + 2).U
  val shiftIsNormal = biased_exp > x_dec.denormbias.U // TODO check inclusive

  val unroundedExp = Wire(UInt(x_dec.expBits.W))
  val unroundedFrac = Wire(UInt(x_dec.fracsize.W))
  val unroundedExtra = Wire(UInt(3.W))

  val denormalFracExt = Cat(1.U(1.W), shiftFrac)
  val denormalFracShift = (denormalFracExt >> (x_dec.denormbias.U - biased_exp)) // TODO chceck

  when(shiftIsNormal) {
    unroundedExp := biased_exp - x_dec.denormbias.U
    unroundedFrac := shiftFrac(x_dec.fracsize + 2, 3)
    unroundedExtra := shiftFrac(2, 0)
  }.otherwise {
    // denormal
    unroundedExp := 0.U
    unroundedFrac := denormalFracShift(x_dec.fracsize + 2, 3) // TODO: idk if correct
    unroundedExtra := denormalFracShift(2, 0)
  }

  val roundedExp = Wire(UInt())
  val roundedFrac = Wire(UInt())
  when(unroundedExtra(2)) {
    // round up
    roundedFrac := unroundedFrac +% 1.U // will handle the carry fine
    when(unroundedFrac.andR()) {
      roundedExp := unroundedExp +% 1.U // overflow to inf is not hadled
    }.otherwise {
      roundedExp := unroundedExp
    }
  }.otherwise {
    // don't round up
    roundedExp := unroundedExp
    roundedFrac := unroundedFrac
  }

  when(shiftZero || flushable) {
    roundedExp := 0.U
    roundedFrac := 0.U
  }

  val signedrefr = Cat(shiftSign, roundedExp, roundedFrac)
  // stage x+3: output
  val outlatch = RegNext(signedrefr)
  val outvalid = RegNext(roundValid, init = false.B)
  io.out_valid := outvalid
  io.out_data := outlatch

  val debugp = Wire(new Float(size, expBits))
  debugp.bits := outlatch
  val debughelp = DecodedFloat(debugp)
  dontTouch(debughelp)
}
