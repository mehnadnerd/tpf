

package tpf

import chisel3._


object Main extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new TPF(size = 32, es = 2))
}