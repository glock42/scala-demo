package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable
import scala.collection.immutable.Nil

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    lazy val genHeap: Gen[H] = for {
        a <- arbitrary[Int]
        h <- oneOf(Gen.const(empty), genHeap)
    } yield insert(a, h)

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen1") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    }

    property("insert 2 value to empty") = forAll { (a: A, b: A) =>
        val q = insert(b, insert(a, empty))
        val min = if (a > b) b else a
        findMin(q) == min
    }

    property("insert into empty and delete it") = forAll { (i: A) =>
        val q = insert(i, empty)
        deleteMin(q) == empty
    }

    property("q is in order") = forAll { (q: H) =>
        def remove(h: H): List[A] ={
            if (isEmpty(h)) Nil
            else findMin(h) :: remove(deleteMin(h))
        }
        val l = remove(q)
        l == l.sorted
    }

    property("meld") = forAll { (h1: H, h2: H) =>
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        val min = if (min1 > min2) min2 else min1
        val h = meld(h1, h2)
        findMin(h) == min
    }


    // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
    property("meldMinMove") = forAll { (h1: H, h2: H) =>
        def remMin(ts: H, as: List[Int]): List[Int] = {
            if (isEmpty(ts)) as
            else findMin(ts) :: remMin(deleteMin(ts), as)
        }
        val meld1 = meld(h1, h2)
        val min1 = findMin(h1)
        val meld2 = meld(deleteMin(h1), insert(min1, h2))
        val xs1 = remMin(meld1, Nil)
        val xs2 = remMin(meld2, Nil)
        xs1 == xs2
    }
}
