package u04lab.code

import Optionals._
import Lists._
import Streams._

trait PowerIterator[A] {
  def next(): Option[A]

  def allSoFar(): List[A]

  def reversed(): PowerIterator[A]
}

case class PowerIteratorImpl[A](private val stream: Stream[A]) extends PowerIterator[A] {
  private val streamIterator: StreamIterator[A] = StreamIterator(stream)
  private var list: List[A] = List.Nil()

  override def next(): Option[A] = {
    val nextValue = streamIterator.next
    list = List.append(list, Stream.toList(Stream.take(Stream.generate(Option.get(nextValue)))(1)))
    nextValue
  }

  override def allSoFar(): List[A] = list

  override def reversed(): PowerIterator[A] = PowerIteratorImpl(List.toStream(List.reverse(list)))

}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]

  def fromList[A](list: List[A])

  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIteratorImpl(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): Unit = PowerIteratorImpl(List.toStream(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] =
    PowerIteratorImpl(Stream.take(Stream.generate(Math.random() < 0.25))(size))

}
