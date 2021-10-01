
import users._
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.experimental.FixedPoint
import math._
import dsptools._

class castExampleTester (c: castExample) extends DspTester(c) {
  
  poke(c.io.x,0.1)
  peek(c.io.y)
  peek(c.io.y1)
  peek(c.io.y2)
  peek(c.io.y3)

  poke(c.io.x,1.1)
  peek(c.io.y)
  peek(c.io.y1)
  peek(c.io.y2)
  peek(c.io.y3)
}

class shiftRegisterTester (c: shiftRegister) extends DspTester(c) {

  for (i <- 0 to 30) {
    poke(c.io.IN(0),i)
    poke(c.io.IN(1),i+1)
    step(1)
    peek(c.io.OUT)
  }

}

class Radix2Tester (c: Radix2) extends DspTester(c) {

  // poke(c.io.x(0),1.12)
  // poke(c.io.x(1),0.34)
  // poke(c.io.x_Nd2(0),1.0)
  // poke(c.io.x_Nd2(1),-0.1)

  poke(c.io.x(0),1)
  poke(c.io.x(1),1)
  poke(c.io.x_Nd2(0),1)
  poke(c.io.x_Nd2(1),1)

  peek(c.io.y(0))
  peek(c.io.y(1))

  peek(c.io.y_d2(0))
  peek(c.io.y_d2(1))
}

class WmultiplerTester (c: Wmultipler) extends DspTester(c) {

  poke(c.io.x(0),1)
  poke(c.io.x(1),0)

  for (i <- 0 to 30) {
    peek(c.io.y(0))
    peek(c.io.y(1))
    step(1)
  }

}

class complexMultiplerTester (c: complexMultipler) extends DspTester(c) {

  poke(c.io.x1(0),-1.9999999)
  poke(c.io.x1(1),-1.9999999)

  poke(c.io.x2(0),cos(3.14/4))
  poke(c.io.x2(1),sin(3.14/4))

  peek(c.io.y(0))
  peek(c.io.y(1))
}

class counterTester (c: counter) extends DspTester(c) {
  for (i <- 0 to 30) {
    peek(c.io.c)
    peek(c.io.b)
    step(1)
  }
}

class R2SDFunitTester (c: R2SDFunit) extends DspTester(c) {
  for (i <- 0 to 30) {

    poke(c.io.x(0),1)
    poke(c.io.x(1),0)
    peek(c.io.y(0))
    peek(c.io.y(1))
    step(1)
  }  
}

class R2SDFTester (c: R2SDF) extends DspTester(c) {


  // val data = List(
  // List(0.70710678,0.0        ),
  // List(0.70710678,0.0        ),
  // List(0.70710678,0.70710678),
  // List(0.70710678,0.70710678),
  // )

  // val data = List(
  // List(-0.4531382 , 0.61934277,-0.06794489,-0.5658579 ),
  // List( 0.79135788, 0.45765447,-0.02684513, 0.82450278),
  // List(-0.46759823,-0.75204162,-0.57456795,-0.018345  ),
  // List( 2.04667   ,-0.36699766,-0.51764449, 2.00340368),
  // )

  // val data = List(
  // List( 0.00166195,-0.50966312,-0.15902967, 0.16188351,-0.35970352,-0.04666767,
  //   0.48343236, 0.73817796),
  // List(-0.52442272,-0.23648586,-0.60518468,-0.63088718,-0.48153894, 0.8178476 ,
  //   0.42679782, 0.67460603),
  // List( 0.3100918 ,-2.25915198,-0.14480136,-0.35541596,-0.37736957, 0.91791794,
  // -1.22008714, 3.1421119 ),
  // List(-0.55926793, 1.51206396, 0.62881745,-0.12805365,-1.80942911,-0.31290745,
  // -2.28396707,-1.24263798),
  // )

  // val data = List(
  // List(0,0,0,0,0,0,0,0),
  // List(0,0,0,0,0,0,0,1),
  // )

  val data = List(
    //in
    List(-0.15556784,-0.24538934, 0.93589853,-0.58342623, 0.11567584, 0.6360381 ,
    0.79911556, 0.11633233),
    List(-0.14136551,-0.837158  , 0.28756459,-0.81216614, 0.16753278,-0.10309104,
    -0.45321498, 0.19268019),
    //out
    List( 1.61867695,-0.88852078,-2.09566917,-2.11316133, 1.77156723, 1.82759255,
    -1.45414299, 0.08911482),
    List(-1.69921811, 0.86385602,-0.665925  , 0.75448211, 1.42025187,-1.75521854,
    1.04956031,-1.09871274),
    )


  val stage = 3         //stage of butterfly
  val N = 1 << stage

  val real = Array.fill[Double](N)(0)
  val imag = Array.fill[Double](N)(0)

  for (i <- Range(0,N-1)) {
    poke(c.io.x(0),data(0)(i))
    poke(c.io.x(1),data(1)(i))
    step(1)
  }

  poke(c.io.x(0),data(0)(N-1))
  poke(c.io.x(1),data(1)(N-1))
  real(0) = peek(c.io.y(0))
  imag(0) = peek(c.io.y(1))

  for (i <- Range(1,N)) {
    step(1)
    // poke(c.io.x(0),0)    //this will be the consecutive block in
    // poke(c.io.x(1),0)
    real(i) = peek(c.io.y(0))
    imag(i) = peek(c.io.y(1))
  }

  //re-order the output
  val order = Array.fill[Int](N)(0)

  for (i <- Range(0,stage)){
    for (j <- Range(0,1<<i)){
      for (m <- Range(0,N>>i)){
        if ((m + j*(N>>i)) >= ((N>>(i+1)) + j*(N>>i))) {
          order(m + j*(N>>i)) = order(m + j*(N>>i)) + (1<<i)
        }
      }
      //println(order.mkString(" "))
    }
  }

  val reoderR = Array.fill[Double](N)(0)
  val reoderI = Array.fill[Double](N)(0)

  for (i <- Range(0,N)) {
      val index = N-1-i
      reoderR(order(i)) = real(index)
      reoderI(order(i)) = imag(index)
  }


  //result
  println("FFT output:")
  for (i <- Range(0,N)) {
    println(reoderR(i).toString + "," + reoderI(i).toString)
  }

  println("error:")
  for (i <- Range(0,N)) {
    println((reoderR(i)-data(2)(i)).toString + "," + (reoderI(i)-data(3)(i)).toString)
  }  

}

object tester extends App {
   
   // (new ChiselStage).emitVerilog(new shiftRegister(32,1))
   // dsptools.Driver.execute (() => new shiftRegister(32,1)) {
   //    c => new shiftRegisterTester(c)
   // }

   // (new ChiselStage).emitVerilog(new Radix2(32))
   // dsptools.Driver.execute (() => new Radix2(32)) {
   //    c => new Radix2Tester(c)
   // }

   // (new ChiselStage).emitVerilog(new counter(4))
   // dsptools.Driver.execute (() => new counter(4)) {
   //    c => new counterTester(c)
   // }

   // (new ChiselStage).emitVerilog(new Wmultipler(32,1))
   // dsptools.Driver.execute (() => new Wmultipler(32,0)) {
   //    c => new WmultiplerTester(c)
   // }

   // (new ChiselStage).emitVerilog(new R2SDFunit(32,0))
   // dsptools.Driver.execute (() => new R2SDFunit(32,30,0)) {
   //    c => new R2SDFunitTester(c)
   // }

    (new ChiselStage).emitVerilog(new R2SDF(32,30,3))
    dsptools.Driver.execute (() => new R2SDF(32,30,3)) {
        c => new R2SDFTester(c)
    }
}
