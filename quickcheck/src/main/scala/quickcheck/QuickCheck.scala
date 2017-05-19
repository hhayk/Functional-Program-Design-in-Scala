package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back") = forAll { (a: A, b: A) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == math.min(a, b)
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll { (a: A) =>
    val heap = insert(a, empty)
    val deleteHeap = deleteMin(heap)
    isEmpty(deleteHeap)
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)") = forAll { (heap: H) =>
    def sort(acc: List[A], heap: H): List[A] = {
      val x = findMin(heap)
      val xs = deleteMin(heap)
      if (isEmpty(xs)) acc :+ x else sort(acc :+ x, xs)
    }

    val sorted = sort(List(), heap)
    sorted == sorted.sorted
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min3 = findMin(meld(h1, h2))
    min1 == min3 || min2 == min3
  }

  property("min element check") = forAll { (a: A, h: H) =>
    findMin(insert(a, h)) == math.min(a, findMin(h))
  }

  property("min in meld") = {
    val h1 = insert(1, insert(2, insert(3, insert(4, empty))))
    val h2 = insert(5, insert(6, insert(7, insert(8, empty))))
    val melded = meld(h2, h1)
    findMin(deleteMin(melded)) == 2
  }

  property("equal in melds") = forAll { (h1: H, h2: H) =>
    def isEqual(h1: H, h2: H, res: Boolean): Boolean = {
      if (isEmpty(h1)) isEmpty(h2) else res && isEqual(deleteMin(h1), deleteMin(h2), findMin(h1) == findMin(h2))
    }

    val meld1 = meld(h1, h2)
    val meld2 = meld(deleteMin(h1), insert(findMin(h1), h2))
    isEqual(meld1, meld2, true)
  }

  lazy val genMap: Gen[Map[Int, Int]] = oneOf(
    const(Map.empty[Int, Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int, Int]), genMap)
    } yield m.updated(k, v)
  )

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf[H](empty, genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
