import chisel3._
import chisel3.stage.ChiselStage
import chisel3.experimental.FixedPoint
import dsptools._
import math._
import scala.collection.mutable.ListBuffer
import scala.math


class castExample (width:Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(FixedPoint(width.W,(width-2).BP))
    val y = Output(FixedPoint(width.W,(width-2).BP))
    val y1 = Output(FixedPoint((width-1).W,(width-2).BP))
    val y2 = Output(FixedPoint((width-1).W,(width-3).BP))
    val y3 = Output(FixedPoint((width-2).W,(width-3).BP))
  })
    io.y := io.x
    io.y1 := io.x
    io.y2 := io.x
    io.y3 := io.x
}

class shiftRegister (width:Int, F:Int, tapsN:Int) extends Module {
  val io = IO(new Bundle {
    val IN = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val OUT  = Output(Vec(2,FixedPoint(width.W,F.BP)))
  })
  
  val tapsI = RegInit(VecInit(Seq.fill(tapsN)(0.F(width.W,F.BP))))
  val tapsQ = RegInit(VecInit(Seq.fill(tapsN)(0.F(width.W,F.BP))))

  for (i <-1 to (tapsN-1)) {
    tapsI(i) := tapsI(i-1)
    tapsQ(i) := tapsQ(i-1)
  }

  tapsI(0) := io.IN(0)
  tapsQ(0) := io.IN(1)
  io.OUT(0) := tapsI(tapsN-1)
  io.OUT(1) := tapsQ(tapsN-1)
}

class Radix2 (width:Int,F:Int) extends Module {

  val io = IO(new Bundle {
    val x = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val x_Nd2 = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val y = Output(Vec(2,FixedPoint((width+1).W,F.BP)))
    val y_d2 = Output(Vec(2,FixedPoint((width+1).W,F.BP)))
  })

  io.y(0) := io.x(0) +& io.x_Nd2(0)
  io.y(1) := io.x(1) +& io.x_Nd2(1)
  io.y_d2(0) := io.x(0) -& io.x_Nd2(0)
  io.y_d2(1) := io.x(1) -& io.x_Nd2(1)

}

class Wmultipler (width:Int, F:Int, Nb:Int) extends Module {
  val io = IO(new Bundle {
    val x  = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val y  = Output(Vec(2,FixedPoint(width.W,F.BP)))
  })

  val N = 1 << Nb

  val cListBufferReal = new ListBuffer[registerValue]()
  val cListBufferImag = new ListBuffer[registerValue]()

  cListBufferReal += Module(new registerValue(width, cos(2*Pi*0/(N*2))))
  cListBufferImag += Module(new registerValue(width, -sin(2*Pi*0/(N*2))))
  for (i <- Range(N-1,0,-1)) {
    cListBufferReal += Module(new registerValue(width, cos(2*Pi*i/(N*2))))
    cListBufferImag += Module(new registerValue(width, -sin(2*Pi*i/(N*2))))
  }
  val cListReal = cListBufferReal.toList
  val cListImag = cListBufferImag.toList

  cListReal(0).io.x := cListReal(N-1).io.y
  cListImag(0).io.x := cListImag(N-1).io.y
  for (i <- Range(1,N,1)) {
    cListReal(i).io.x := cListReal(i-1).io.y
    cListImag(i).io.x := cListImag(i-1).io.y
  }  

  io.y(0) := io.x(0)*cListReal(0).io.y -& io.x(1)*cListImag(0).io.y
  io.y(1) := io.x(0)*cListImag(0).io.y +& io.x(1)*cListReal(0).io.y
}


class registerValue (width:Int, init: Double) extends Module {
  val io = IO(new Bundle {
    val x  = Input(FixedPoint(width.W,(width-2).BP))
    val y  = Output(FixedPoint((width+1).W,(width-2).BP))
  })

  val r = RegInit(init.F(width.W,(width-2).BP))
  
  r := io.x
  io.y := r
}

class counter (Nb:Int) extends Module {
  val io = IO(new Bundle {
      val c   = Output(UInt(Nb.W))
      val b   = Output(UInt(1.W))
  })

  val count = RegInit(0.U(Nb.W))
  count := count + 1.U
  when (count(Nb-1) === 1.U){
    io.b := 1.U
  }
  .otherwise {
    io.b := 0.U
  }

  io.c := count
}

class R2SDFunit (width:Int, F:Int, Nb:Int) extends Module {
  val io = IO(new Bundle{
    val x = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val y = Output(Vec(2,FixedPoint((width+1).W,F.BP)))
  })

  val counterUnit = Module(new counter(Nb+1))
  val shiftRegisterUnit = Module(new shiftRegister(width+1,F,1<<Nb))
  val Radix2Unit = Module(new Radix2(width,F))
  val WmultiplerUnit = Module(new Wmultipler(width+1,F,Nb))

  
  Radix2Unit.io.x_Nd2 := io.x
  shiftRegisterUnit.io.IN := Radix2Unit.io.y
  WmultiplerUnit.io.x := Radix2Unit.io.y_d2
  when (counterUnit.io.b === 1.U){
    Radix2Unit.io.x := shiftRegisterUnit.io.OUT
    io.y := WmultiplerUnit.io.y
  }
  .otherwise{
    Radix2Unit.io.x := VecInit(Seq.fill(2)(0.F(width.W,F.BP)))
    io.y := shiftRegisterUnit.io.OUT
  }

}

class R2SDF (width:Int, F:Int, stage:Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(Vec(2,FixedPoint(width.W,F.BP)))
    val y = Output(Vec(2,FixedPoint((width+stage).W,F.BP)))
  })

  val unitListBuffer = new ListBuffer[R2SDFunit]()
  for (i <- Range(0, stage, 1)) {
    unitListBuffer += Module(new R2SDFunit(width+(stage-1-i),F,i))
  }
  val unitList = unitListBuffer.toList

  for (i <- Range(0, stage, 1)) {
    if (i < stage - 1){
      unitList(i).io.x := unitList(i+1).io.y
    }
    if (i > 0){
      unitList(i-1).io.x := unitList(i).io.y
    }
  }

  unitList(stage-1).io.x := io.x
  io.y := unitList(0).io.y
}