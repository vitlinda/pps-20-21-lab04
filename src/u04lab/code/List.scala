package u04lab.code

import scala.annotation.tailrec

object VarArgListTest extends App {
  import Lists._
  import Optionals._
  object VarArgList{
    def apply[E](elems: E*): List[E] = {
      var list: List[E] = List.Nil()
      elems.foreach(e => list = List.append(list, List.Cons(e, List.Nil())))
      list
    }
  }

  object sameTeacher{
    def unapply(l: List[Course]): scala.Option[String] = {
      val head = Option.get(List.head(l)).teacher
      if (List.forAll(l)(e => e.teacher == head)) scala.Some(head) else scala.None
    }
  }

  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val cOOP = Course("OOP", "Viroli")
  
  val l1 = VarArgList(cPPS, cPCD, cSDR)
  val l2 = VarArgList(cPPS, cOOP)

  l1 match {
    case sameTeacher(t) => println(s"$l1 have same teacher $t")
    case _ => println(s"$l1 have different teachers")
  }

  l2 match {
    case sameTeacher(t) => println(s"$l2 have same teacher $t")
    case _ => println(s"$l2 have different teachers")
  }
}


object Lists extends App {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def nil[A]: List[A] = Nil() // smart constructor

    def forAll[A](l: List[A])(pred: A => Boolean): Boolean = length(filter(l)(pred)) == length(l)

    def head[A](l: List[A]): Optionals.Option[A] = l match {
      case Cons(h, _) => Optionals.Option.Some(h)
      case _ => Optionals.Option.None[A]
    }

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case _ if n<=0 || l==Nil() => l
      case Cons(h,t) => drop(t,n-1)
    }

    def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Cons(h,t) => Cons(f(h), map(t)(f))
      case Nil() => Nil()
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h,t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h,t) => filter(t)(f)
      case Nil() => Nil()
    }

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(h,t) => append(f(h),flatMap(t)(f))
      case Nil() => Nil()
    }

    @tailrec
    def foldLeft[A,B](l: List[A])(acc: B)(f: (B,A)=>B): B = l match {
      case Cons(h,t) => foldLeft(t)(f(acc,h))(f)
      case Nil() => acc
    }

    def foldRightNonTailRec[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B = l match {
      case Cons(h,t) => f(h, foldRightNonTailRec(t)(acc)(f))
      case Nil() => acc
    }

    def reverse[A](l: List[A]) : List[A] =
      foldLeft(l)(nil[A])((acc,elem) => Cons(elem,acc))

    def foldRightViaFoldleft[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B =
      foldLeft(reverse(l))(acc)((acc,elem) => f(elem,acc))

    def foldRight[A,B](l: List[A])(acc: B)(f: (A,B)=>B): B =
      foldRightViaFoldleft(l)(acc)(f)

    def contains[A](l: List[A])(elem: A): Boolean = {
      filter(l)(e => e == elem) != Nil()
    }

    def toStream[A](l: List[A]): Streams.Stream[A] = l match {
      case Cons(h, tail) => Streams.Stream.cons(h, toStream(tail))
      case Nil() => Streams.Stream.empty()
    }

//    def filterByFlatmap[A](l: List[A])(f: A => Boolean): List[A] = ???

//    def appendByFold[A](l1: List[A], l2: List[A]): List[A] = ???

    def length(l: List[_]): Int = l match {
      case Cons(h, t) => 1 + length(t)
      case _ => 0
    }
  }

  // Note "List." qualification
  println(List.sum(List.Cons(10, List.Cons(20, List.Cons(30, List.Nil()))))) // 60
  import List._
  println(append(Cons("10", Nil()), Cons("1", Cons("2", Nil())))) // "10","1","2"

  println(drop(Cons(10, Cons(20, Cons(30, Nil()))),2)) // Cons(30, Nil())
  println(drop(Cons(10, Cons(20, Cons(30, Nil()))),5)) // Nil()
  println(drop(Nil(), 5)) // Nil()

  println(map(Cons(10, Cons(20, Nil())))(_+1))       // Cons(11, Cons(21, Nil()))
  println(map(Cons(10, Cons(20, Nil())))(":"+_+":")) // Cons(":10:", Cons(":20:",Nil()))
  println(filter(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
  println(filter(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))

  val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_-_)) // -16
  println(reverse(lst)) // Cons(5,Cons(1,Cons(7,Cons(3,Nil()))))
  println(foldRightNonTailRec(lst)(0)(_-_)) // -8
  println(foldRightViaFoldleft(lst)(0)(_-_)) // -8

  // EXERCISES:
//  println(filterByFlatmap(Cons(10, Cons(20, Nil())))(_>15)) // Cons(20, Nil())
//  println(filterByFlatmap(Cons("a", Cons("bb", Cons("ccc", Nil()))))( _.length <=2)) // Cons("a",Cons("bb", Nil()))
//  println(appendByFold(Cons(3,Cons(7,Nil())), Cons(1,Cons(5,Nil())))) // Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
  println(length(Nil())) // 0
  println(length(Cons(3,Cons(7,Cons(1,Cons(5, Nil())))))) // 4


  println(List.forAll(List.Cons(1, List.Cons(1, List.Cons(1, List.Nil()))))(l => l == 1)) //true
  println(List.forAll(List.Cons(1, List.Cons(2, List.Cons(1, List.Nil()))))(l => l == 1)) //false
}