package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
  }

  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains elements common to both`: Unit = {
    new TestSets {
      val s = intersect(union(s1, s2), s2)

      assert(!contains(s, 1), "intersect 1")
      assert(contains(s, 2), "intersect 2")
      assert(!contains(s, 3), "intersect 3")
    }
  }

  @Test def `difference contains elements in first but not second set`: Unit = {
    new TestSets {
      val s = diff(union(s3, union(s1, s2)), s2)

      assert(contains(s, 1), "diff 1")
      assert(!contains(s, 2), "diff 2")
      assert(contains(s, 3), "diff 3")
    }
  }

  @Test def `filter contains elements matching the function`: Unit = {
    new TestSets {
      val alls = union(s3, union(s1, s2))
      val s = filter(alls, (e: Int) => e > 1)

      assert(!contains(s, 1), "filter 1")
      assert(contains(s, 2), "filter 2")
      assert(contains(s, 3), "filter 3")

      val ss = filter(alls, (e: Int) => e % 2 == 0)

      assert(!contains(ss, 1), "filter 1")
      assert(contains(ss, 2), "filter 2")
      assert(!contains(ss, 1), "filter 3")
    }
  }

  @Test def `forall checks if all elements match a boolean function`: Unit = {
    new TestSets {
      val evens = union(singletonSet(-200), union(singletonSet(20), singletonSet(448)))
      val odds = union(singletonSet(-1), union(singletonSet(45), singletonSet(557)))
      val both = union(s1, union(s2, s3))

      assert(forall(evens, (e: Int) => e % 2 == 0), "evens")
      assert(forall(odds, (e: Int) => e % 2 != 0), "odds")
      assert(!forall(both, (e: Int) => e % 2 ==0), "not even")
    }
  }


  @Test def `exists checks if one element satisfies a condition`: Unit = {
    new TestSets {
      val evens = union(singletonSet(-200), union(singletonSet(20), singletonSet(448)))
      val odds = union(singletonSet(-1), union(singletonSet(45), singletonSet(557)))
      val both = union(s1, union(s2, s3))

      assert(exists(evens, (e: Int) => e == 20), "twenty")
      assert(exists(odds, (e: Int) => e < 0), "negative")
      assert(!exists(evens, (e: Int) => e % 2 !=0), "not even")
    }
  }

  @Test def `map transforms the elements in a set`: Unit = {
    new TestSets {
      val evens = union(singletonSet(-200), union(singletonSet(20), singletonSet(448)))
      val odds = union(singletonSet(-1), union(singletonSet(45), singletonSet(557)))
      val both = union(s1, union(s2, s3))
      val doubled = map(evens, (e: Int) => 2 * e)
      assert(contains(doubled, -400), "double -200")
      assert(contains(doubled, 40), "double 20")
      assert(contains(doubled, 896), "double 448")
    }
  }
  
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
