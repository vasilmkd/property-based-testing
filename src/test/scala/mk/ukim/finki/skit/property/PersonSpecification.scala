package mk.ukim.finki.skit.property

import org.scalacheck.Gen.oneOf
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

object PersonSpecification extends Properties("Person") {

  val underagePersonGen: Gen[Person] = for {
    name <- Gen.alphaStr
    age  <- Gen.choose(0, 17)
  } yield Person(name, age)

  val adultPersonGen: Gen[Person] = for {
    name <- Gen.alphaStr
    age  <- Gen.choose(18, 120)
  } yield Person(name, age)

  implicit val personArbitrary: Arbitrary[Person] = Arbitrary(oneOf(underagePersonGen, adultPersonGen))

  property("underage persons are not adults") = forAll(underagePersonGen) { p: Person => !p.isAdult }

  property("people of age 18 or older are adults") = forAll(adultPersonGen) { p: Person => p.isAdult }
}
