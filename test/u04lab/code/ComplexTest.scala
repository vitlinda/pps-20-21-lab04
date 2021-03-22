package u04lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class ComplexTest {

  @Test
  def testComplexSum(): Unit = {
    val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
    val c = a(0) + a(1) + a(2)

    assertEquals(ComplexImpl(18.0, 21.0), c)
    assertEquals(18.0, c.re)
    assertEquals(21.0, c.im)
  }

//  val c2 = a(0) * a(1)
//  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}
