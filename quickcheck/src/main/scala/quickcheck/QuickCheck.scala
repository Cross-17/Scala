package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("4") = forAll{
    (h1:H ,h2:H) =>
      val min1 = if (isEmpty(h1)) 0 else findMin(h1)
      val min2 = if (isEmpty(h2)) 0 else findMin(h2)
      val newmin = findMin(meld(h1,h2))
      newmin == min1 || newmin == min2
  }

  property("3") = forAll{
    (h:H) =>
      def sorted(min : Int, h : H ):Boolean = {
        isEmpty(h) || min <= findMin(h) && sorted(findMin(h),deleteMin(h))
      }
      sorted(findMin(h),deleteMin(h))
  }

  property("2") = forAll {
    x : Int =>
      val h = insert(x,empty)
      isEmpty(deleteMin(h))
  }

  property("1") = forAll {
    (x : Int , y :Int) =>
      val h = insert(x,insert(y,empty))
      val min = if(x < y) x else y
      findMin(h) == min
  }

  property("sortedSequence1") = forAll { (a: Int, b: Int, c: Int) =>
    val list = List(a, b, c).sorted
    val heap1 = deleteMin(list.foldLeft(empty) { (h, e) => insert(e, h) })
    val heap2 = list.tail.foldLeft(empty) { (h, e) => insert(e, h) }
    heap1 == heap2
  }


}
