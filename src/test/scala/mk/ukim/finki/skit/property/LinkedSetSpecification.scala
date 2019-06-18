package mk.ukim.finki.skit.property

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object LinkedSetSpecification extends Properties("LinkedSet") {

  def functionalSetGen[A: Arbitrary]: Gen[LinkedSet[A]] = for {
    as <- Gen.listOf[A](implicitly[Arbitrary[A]].arbitrary)
  } yield LinkedSet(as: _*)

  implicit def functionalSetArbitrary[A: Arbitrary]: Arbitrary[LinkedSet[A]] =
    Arbitrary(functionalSetGen)

  import PersonSpecification.personArbitrary

  property("empty set contains no elements") = forAll { p: Person =>
    val emptySet = LinkedSet.empty[Person]
    !emptySet(p)
  }

  property("set with one element contains the element") = forAll { p: Person =>
    val set = LinkedSet(p)
    set(p)
  }

  property("adding an element to a set makes the new set contain the element") = forAll {
    (old: LinkedSet[Person], p: Person) =>
      val set = old + p
      set(p)
  }

  property("removing an element from a set makes the new set not contain the element") = forAll {
    (old: LinkedSet[Person], p: Person) =>
      val set = (old + p) - p
      !set(p)
  }

  property("creating a set from a list can be turned into the same list again") = forAll {
    list: List[Person] =>
      val sorted = list.sortBy(_.name)
      val set = LinkedSet(sorted: _*)

      set.toList.sortBy(_.name) == sorted
  }

  property("union of two sets contains all elements from both sets") = forAll {
    (s1: LinkedSet[Person], s2: LinkedSet[Person]) =>
      val set = s1 union s2
      val allElements = s1.toList ++ s2.toList

      allElements.forall(set.apply)
  }

  property("intersection of two sets contains only the elements found in both sets") = forAll {
    (s1: LinkedSet[Person], s2: LinkedSet[Person]) =>
      val set = s1 intersection s2

      set.toList.forall(p => s1(p) && s2(p))
  }

  property("diff of two sets contains only the elements found in the first set, but not the second") = forAll {
    (s1: LinkedSet[Person], s2: LinkedSet[Person]) =>
      val set = s1 diff s2

      set.toList.forall(p => s1(p) && !s2(p))
  }

  property("xor of two sets contains only the elements found in either diff, but not the intersection") = forAll {
    (s1: LinkedSet[Person], s2: LinkedSet[Person]) =>
      val set = s1 xor s2

      set.toList.forall(p => (s1(p) && !s2(p)) || (!s1(p) && s2(p)))
  }
}
