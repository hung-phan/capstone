package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin on heap with single value") = forAll { (a: Int) =>
    findMin(insert(a, empty)) == a
  }

  property("findMin on generated heap") = forAll { (h: H) =>
    val min = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(min, h)) == min
  }

  property("insert 2 elements into the heap and findMin") = forAll { (a1: Int, a2: Int) =>
    val min = if (a1 <= a2) a1 else a2

    findMin(insert(a1, insert(a2, empty))) == min
  }

  property("insert element into the heap and then try to remove") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("get sorted elements when continually finding and deleting minimal") = forAll { (h: H) =>
    def getMinSequence(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: getMinSequence(deleteMin(h))
    }

    val minSeq = getMinSequence(h)

    minSeq == minSeq.sorted
  }

  property("melding of any two heaps should return a minimum of one or the other") = forAll { (h1: H, h2: H) =>
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    val min = if (minH1 <= minH2) minH1 else minH2

    findMin(meld(h1, h2)) == min
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    heapEqual(
      meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2))
    )
  }
}
