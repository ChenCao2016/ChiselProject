import chisel3._
import chisel3.stage.ChiselStage
import chisel3.experimental.FixedPoint
import dsptools._
import math._
import scala.collection.mutable.ListBuffer
import scala.math


class CORDIC (width: Int) extends Module{
  val io = IO(new Bundle {
    val PHASE = Input(FixedPoint(width.W,(width-3).BP))
    val REAL  = Output(FixedPoint(width.W,(width-2).BP))
    val IMAG  = Output(FixedPoint(width.W,(width-2).BP))
  })

  val taps = 18
  val taps_x = Seq(RegInit(1.F(width.W,(width-2).BP))) ++ Seq.fill(taps-1)(RegInit(0.F(width.W,(width-2).BP)))
  val taps_y = Seq.fill(taps)(RegInit(0.F(width.W,(width-2).BP)))
  val p = Seq.fill(taps)(RegInit(0.F(width.W,(width-2).BP)))

  for (i <- 1 to (taps-1) ) {
    when (p(i-1) < io.PHASE) {
        taps_x(i) := taps_x(i-1) - (taps_y(i-1) >> (i-1))
        taps_y(i) := taps_y(i-1) + (taps_x(i-1) >> (i-1))
        if (i < (taps - 1)) {
          p(i) := p(i-1) + atan(pow(2,-(i-1))).F(width.W,(width-3).BP)
        } 
    }
    .otherwise {
        taps_x(i) := taps_x(i-1) + (taps_y(i-1) >> (i-1))
        taps_y(i) := taps_y(i-1) - (taps_x(i-1) >> (i-1))
        if (i < (taps - 1)) {
          p(i) := p(i-1) - atan(pow(2,-(i-1))).F(width.W,(width-3).BP)
        }
    }
  }

  io.REAL := taps_x(taps-1)
  io.IMAG := taps_y(taps-1)
}
