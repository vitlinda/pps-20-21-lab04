package u04lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import u04lab.code.Lists.List._

class StudentTest {

  @Test def studentTest: Unit = {
    val cPPS = Course("PPS","Viroli")
    val cPCD = Course("PCD","Ricci")
    val cSDR = Course("SDR","D'Angelo")
    val s1 = Student("mario",2015)
    val s2 = Student("gino",2016)
    val s3 = Student("rino") //defaults to 2017

    s1.enrolling(cPPS)
    s1.enrolling(cPCD)
    s2.enrolling(cPPS)
    s3.enrolling(cPPS)
    s3.enrolling(cPCD)
    s3.enrolling(cSDR)
    assertEquals(Cons("PCD", Cons("PPS", Nil())), s1.courses)
    assertEquals(Cons("PPS", Nil()), s2.courses)
    assertEquals(Cons("SDR", Cons("PCD", Cons("PPS", Nil()))), s3.courses)
  }

  @Test def hasTeacherTest: Unit = {
    val s1 = Student("mario",2015)
    val cPPS = Course("PPS","Viroli")
    s1.enrolling(cPPS)
    assertFalse(s1.hasTeacher("Ricci"))
    assertFalse(s1.hasTeacher("Viroli"))
  }

}
