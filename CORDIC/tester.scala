import chisel3._
import chisel3.stage.ChiselStage
import dsptools._

class CORDICtester(c: CORDIC) extends DspTester(c) {

  val taps = 100
  poke(c.io.PHASE, 3.14/4)
  step(100)
  peek(c.io.REAL)
  peek(c.io.IMAG)

}

object chenTesterMain extends App {
   
   (new ChiselStage).emitVerilog(new CORDIC(16))
   dsptools.Driver.execute (() => new CORDIC(16)) {
       c => new CORDICtester(c)
    }
}
