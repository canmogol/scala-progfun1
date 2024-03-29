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
    val s4 = union(union(union(union(union(s1, s3),singletonSet(4)),singletonSet(5)),singletonSet(7)),singletonSet(1000))
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

	test("intersect of singleton sets ok") {
    new TestSets {
      val inter = intersect(s1, s2)
      assert(!contains(inter, 1), "inter 1")
      assert(!contains(inter, 2), "inter 2") 
      val sameInter = intersect(s1, singletonSet(1))
      assert(contains(sameInter, 1), "same set inter")
		}
	}
  
  test("diff of singleton sets ok") {
    new TestSets {
      val df = diff(s1, s2)
      assert(contains(df, 1), "diff 1")
      assert(contains(df, 2), "diff 2")
      val dfSame = diff(s1, singletonSet(1))
      assert(!contains(dfSame, 1), "diff same")
    }
  }

  test("filter of singleton sets") {
    new TestSets {
      val filterSet = filter(s1, x=>x==1)
      printSet(filterSet)
      assert(contains(filterSet, 1), "singleton set 1")
      assert(!contains(filterSet, 0), "singleton set 1")
      val filteredSet = filter(s4, x=>x<5)
      printSet(s4)
      printSet(filteredSet)
    }
  }

  test("1 exists in of singleton set 1, 2 in 2, 0 not in 3") {
    new TestSets {
      val result1 = exists(s1, x=>x==1)
      assert(result1 == true, "1 singleton 1")
      val result2 = exists(s2, x=>x==2)
      assert(result2 == true, "2 singleton 2")
      val result3 = exists(s3, x=>x==0)
      assert(result3 == false, "0 singleton 3")
    }
  }

  test("map singleton set with func x*2") {
    new TestSets {
      val mappedSetS = map(s1, x=>x-1)
      //printSet(mappedSetS)
      val resultS = exists(mappedSetS, x=>x==0)
      //println(resultS)
      assert(resultS == true, "subtract map") 

      val mappedSet1 = map(s1, x=>x*2)
      //printSet(mappedSet1)
      val result1 = exists(mappedSet1, x=>x==2)
      //println(result1)
      assert(result1 == true, "2 in mapped set 1") 

      val mappedSet2 = map(s2, x=>x*2)
      //printSet(mappedSet2)
      val result2 = exists(mappedSet2, x=>x==4)
      //println(result2)
      assert(result2 == true, "4 in mapped set 2") 

      val mappedSet3 = map(s3, x=>x*3)
      //printSet(mappedSet3)
      val result3 = exists(mappedSet3, x=>x==9)
      //println(result3)
      assert(result3 == true, "9 in mapped set 3") 

      val mappedSet3Fail = map(s3, x=>x*3)
      //printSet(mappedSet3Fail)
      val result3Fail = exists(mappedSet3Fail, x=>x==0)
      //println(result3Fail)
      assert(result3Fail == false, "0 not in mapped set 3") 
    }
  }

  test("All elements in the set are not strictly less than 5") {
    new TestSets {
      // def forall(s: Set, p: Int => Boolean): Boolean
      //printSet(s4)
      assert(forall(s4, x=>x<5)==false, "less than 5")
      assert(forall(s1, x=>x<2)==true, "x < 2 for s1")
      assert(forall(s2, x=>x<3)==true, "x < 3 for s2")
      assert(forall(s2, x=>x<0)==false, "x < 0 not correct for s2")
    }
  }



}
