package u04lab.code

import Optionals._
import Lists._
import Lists.List._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5, _ + 2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11, List.Nil())))), pi.allSoFar()); // elementi già prodotti
    for (_ <- 0 until 10) {
      pi.next(); // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test
  def testRandom() {
    val pi = factory.randomBooleans(4);
    val b1 = pi.next()
    val b2 = pi.next()
    val b3 = pi.next()
    val b4 = pi.next()
    assertEquals(Option.empty, pi.next()); // ne ho già prodotti 4, quindi il prossimo è un opzionale vuoto
    assertEquals(List.Cons(Option.get(b1), List.Cons(Option.get(b2), List.Cons(Option.get(b3), List.Cons(Option.get(b4), List.Nil())))), pi.allSoFar()) // ho prodotto proprio b1,b2,b3,b4
  }

  @Test
  def testFromList() {
    val pi = factory.fromList(List.Cons("a", List.Cons("b", List.Cons("c", List.Cons("d", List.Nil()))))) // pi produce a,b,c
    assertEquals(pi.next(), Option.of("a"))
    assertEquals(pi.next(), Option.of("b"))
    assertEquals(pi.allSoFar(), List.Cons("a", List.Cons("b", List.Nil()))) // fin qui a,b
    assertEquals(pi.next(), Option.of("c"))
    assertEquals(pi.allSoFar(), List.Cons("a", List.Cons("b", List.Cons("c", List.Nil()))))// fin qui a,b,c
  }

  @Test
  def testReversedOnList() {
    val pi = factory.fromList(List.Cons("a", List.Cons("b", List.Cons("c", List.Nil()))));
    assertEquals(pi.next(),Option.of("a"));
    assertEquals(pi.next(),Option.of("b"));
    val pi2 = pi.reversed(); //pi2 itera su b,a
    assertEquals(pi.next(),Option.of("c")); // c viene prodotto da pi normalmente

    assertEquals(pi2.next(),Option.of("b"));
    assertEquals(pi2.next(),Option.of("a"));
    assertEquals(pi2.allSoFar(), List.Cons("b", List.Cons("a", List.Nil()))); // pi2 ha prodotto b,a
  }

  @Test
  def testReversedOnIncremental() {
    val pi = factory.incremental(0, _+1); // 0,1,2,3,...
    assertEquals(pi.next(),Option.of(0));
    assertEquals(pi.next(),Option.of(1));
    assertEquals(pi.next(),Option.of(2));
    assertEquals(pi.next(),Option.of(3));
    val pi2 = pi.reversed(); // pi2 itera su 3,2,1,0
    assertEquals(pi2.next(),Option.of(3));
    assertEquals(pi2.next(),Option.of(2));
    val pi3 = pi2.reversed(); // pi2 ha prodotto 3,2 in passato, quindi pi3 itera su 2,3
    assertEquals(pi3.next(),Option.of(2));
    assertEquals(pi3.next(),Option.of(3));
  }


}