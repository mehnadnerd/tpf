

package tpf

import chisel3._


object Main extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new TPF(size = 16, es = 2))
}

object IEEEMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new TIF(size = 16, expBits = 8))
}