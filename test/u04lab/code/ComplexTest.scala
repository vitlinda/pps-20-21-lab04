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

  @Test
  def testComplexProduct(): Unit = {
    val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
    val c2 = a(0) * a(1)

    assertEquals(ComplexImpl(-10.0, 30.0), c2)
    assertEquals(-10.0, c2.re)
    assertEquals(30.0, c2.im)
  }
}
