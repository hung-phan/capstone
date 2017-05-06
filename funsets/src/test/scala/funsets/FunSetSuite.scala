package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  * - run the "test" command in the SBT console
  * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    * - test
    * - ignore
    * - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val setOf0To10: Set = (num: Int) => 0 <= num && num <= 10
    val setOf5To15: Set = (num: Int) => 5 <= num && num <= 15
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(setOf0To10, setOf5To15)

      for (num <- 0 to 15) {
        assert(contains(s, num), "Union " + num)
      }

      assert(!contains(s, -1), "Union -1")
      assert(!contains(s, 16), "Union 16")
    }
  }

  test("intersect contains only 5 to 10") {
    new TestSets {
      var s = intersect(setOf0To10, setOf5To15)

      for (num <- 0 to 4) {
        assert(!contains(s, num), "Intersect " + num)
      }

      for (num <- 11 to 15) {
        assert(!contains(s, num), "Intersect " + num)
      }

      for (num <- 5 to 10) {
        assert(contains(s, num), "Intersect " + num)
      }
    }
  }

  test("diff contains only 0 to 4") {
    new TestSets {
      var s = diff(setOf0To10, setOf5To15)

      for (num <- 0 to 4) {
        assert(contains(s, num), "Diff " + num)
      }

      for (num <- 5 to 15) {
        assert(!contains(s, num), "Diff " + num)
      }
    }
  }

  test("filter contains only even number from 0 to 10") {
    new TestSets {
      def divisibleBy2(num: Int): Boolean = num % 2 == 0

      var s = filter(setOf0To10, divisibleBy2)

      for (num <- 0 to 10) {
        if (divisibleBy2(num)) {
          assert(contains(s, num), "Filter " + num)
        } else {
          assert(!contains(s, num), "Filter " + num)
        }
      }

      assert(!contains(s, 12), "Filter 12")
    }
  }

  test("forall returns true for a set of 0 to 10 that is smaller than 11") {
    new TestSets {
      assert(forall(setOf0To10, num => num < 11))
    }
  }

  test("forall returns false for a set of 0 to 10 that is smaller than 5") {
    new TestSets {
      assert(!forall(setOf0To10, num => num < 5))
    }
  }

  test("exists returns true for a set of 0 to 10 that has a number divisible by 5 and doesn't equal to 0") {
    new TestSets {
      def divisibleBy5(num: Int): Boolean = num != 0 && num % 5 == 0

      assert(exists(setOf0To10, divisibleBy5))
    }
  }

  test("exists returns false for a set of 0 to 10 that is has number divisible by 12 and doesn't equal to 0") {
    new TestSets {
      def divisibleBy12(num: Int): Boolean = num != 0 && num % 12 == 0

      assert(!exists(setOf0To10, divisibleBy12))
    }
  }

  test("map returns a set of 5 to 15 after apply + 5 to a set of 0 to 10") {
    new TestSets {
      assert(
        forall(map(setOf0To10, num => num + 5), num => 5 <= num && num <= 15)
      )
    }
  }

  test("map returns correct set") {
    new TestSets {
      def unifySet(s: Set, num: Int) = union(s, singletonSet(num))
      val s = List(1, 3, 4, 5, 7, 1000).foldLeft((num: Int) => false)(unifySet)
      val newSet = map(s, num => num - 1)

      assert(FunSets.toString(newSet) === "{0,2,3,4,6,999}")
    }
  }
}
