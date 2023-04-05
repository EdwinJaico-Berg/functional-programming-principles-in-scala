package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val univSet = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))

    val completeSet = createSet(union, 1000)

    def createSet(f: (FunSet, FunSet) => FunSet, bound: Int): FunSet =
      def recur(a: Int): FunSet =
        if a == bound - 1 then (f(singletonSet(a), singletonSet(a+1)))
        else f(singletonSet(a), recur(a + 1))
      recur(-bound)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains the intersecting elements") {
    new TestSets:
      val s = union(s1, s2)
      val t = union(s2, s3)
      val i = intersect(s, t)
      assert(contains(i, 2), "Intersection between {1, 2} {2, 3}")
      assert(!contains(i, 1), "Intersection between {1, 2} {2, 3}")
      assert(!contains(i, 3), "Intersection between {1, 2} {2, 3}")
  }

  test("diff contains the difference of two sets") {
    new TestSets:
      val t = union(s2, union(s4, s6))
      val d = diff(univSet, t)
      assert(contains(d, 1) && contains(d, 3) && contains(d, 5), "Difference contains {1, 3, 5}")
  }

  test("filter returns set containing even numbers") {
    new TestSets:
      val f = filter(univSet, x => x % 2 == 0)
      assert(contains(f, 2) && contains(f, 4) && contains(f, 6), "Contains even numbers")
  }

  test("forall returns true for identity and false for square") {
    new TestSets:
      assert(forall(completeSet, x => x == x), "Identity")
      assert(!forall(completeSet, x => x == x * x), "Square")
  }

  test("exists returns true for set that contains squares") {
    new TestSets:
      assert(exists(completeSet, x => x == x * x), "Square")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
