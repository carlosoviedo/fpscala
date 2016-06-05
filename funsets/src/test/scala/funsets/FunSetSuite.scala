package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
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
   *   val s1 = singletonSet(1)
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
    val s134 = (x: Int) => x == 1 || x == 3 || x == 4
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
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect s(1,2) and s(2,3) == s(2)") {
    val s12 = (x: Int) => x == 1 || x == 2
    val s23 = (x: Int) => x == 2 || x == 3
    val s = intersect(s12, s23)
    assert(contains(s, 2), "Intersect contains 2")
    assert(!contains(s, 1), "Intersect doesn't contain 1")
    assert(!contains(s, 3), "Intersect doesn't contain 3")
  }

  test("diff s(1,2,3) and s(2,4,6) == s(1,3)") {
    val s123 = (x: Int) => x == 1 || x == 2 || x == 3
    val s246 = (x: Int) => x == 2 || x == 4 || x == 6
    val s = diff(s123, s246)
    assert(contains(s, 1), "Diff contains 1")
    assert(!contains(s, 2), "Diff does not contain 2")
    assert(contains(s, 3), "Diff contains 3")
  }

  private def isEven(x: Int) = x % 2 == 0
  private def isOdd(x: Int) = !isEven(x)

  test("filter even(s(1,2,5,8)) == s(2,8)") {
    val s1258 = (x: Int) => x == 1 || x == 2 || x == 5 || x == 8
    val s = filter(s1258, isEven)
    assert(!contains(s, 1), "Filter does not contain 1")
    assert(contains(s, 2), "Filter contains 2")
    assert(!contains(s, 5), "Filter does not contain 5")
    assert(contains(s, 8), "Filter contains 8")
  }

  private def testForAll(s: Set, message: String)(f: Boolean => Boolean) = {
    val allAreOdd = forall(s, isOdd)
    assert(f(allAreOdd), message)
  }

  test("forall odd(s(1,3,5)) == true") {
    val s135 = (x: Int) => x == 1 || x == 3 || x == 5
    testForAll(s135, "All members are odd numbers")(x => x)
  }

  test("forall odd(s(1,3,4)) == false") {
    new TestSets {
      testForAll(s134, "All members are not odd numbers")(x => !x)
    }
  }

  test("exists even(s(1,3,4)) == true") {
    new TestSets {
      assert(exists(s134, isEven), "At least one number is even")
      assert(exists(s134, isOdd), "At least one number is odd")
    }
  }

  test("map of square(s(1,3,4)) = s(1,9,16)") {
    new TestSets {
      val s = map(s134, x => x*x)
      assert(contains(s, 1), "Map result contains 1")
      assert(contains(s, 9), "Map result contains 9")
      assert(contains(s, 16), "Map result contains 16")
      assert(!contains(s, 3), "Map result does not contain 3")
      assert(!contains(s, 4), "Map result does not contain 4")
    }
  }
}
