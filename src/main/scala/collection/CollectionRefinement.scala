package collection

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.{And, _}
import eu.timepit.refined.collection.{Contains, Empty, Forall, MinSize, NonEmpty}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{Greater, Interval, Negative, Positive}
import shapeless.Nat._0
import shapeless.Witness

import scala.language.higherKinds

object CollectionRefinement extends App {
  //list with more than 1 elements
  type NonEmptyListValidator = MinSize[shapeless.nat._1]

  type NonEmptyList = Refined[List[Int], NonEmptyListValidator]

  val nonEmptyList = refineV[NonEmptyListValidator](List(-2, -1, 0, 2))

  println(nonEmptyList)

  //list with at least 1 positive element
  type PositiveElementValidator = Contains[Positive]
  type PositiveElementList[N] = Refined[List[N], PositiveElementValidator]

  implicit val validator: Validate[List[Int], PositiveElementValidator] = Validate.fromPredicate[List[Int], PositiveElementValidator](
    (l: List[Int]) => l.exists(e => e > 0),
    (l: List[Int]) => s"$l contains only negative elements",
    Not(Forall(Not(Equal(Greater(_0)))))
  )

  val positiveElementList = refineV[PositiveElementValidator](List(-1))

  println(positiveElementList)

  //list with all elements either in [10, 100] interval or [-10, -100]

  type NegativeInterval = Interval.Closed[W.`"-100"`.T, W.`"-10"`.T]
  type PositiveInterval = Interval.Closed[W.`10`.T, W.`100`.T]
/*
  type AcceptableInterval = Xor[PositiveInterval, NegativeInterval]

  type ListInAcceptableInterval = Refined[List[Int], AcceptableInterval]

  implicit val listValidator: Validate[List[Int], AcceptableInterval] = Validate.fromPredicate[List[Int], AcceptableInterval](
    (l: List[Int]) => l.forall(i => (i >= 10 && i <= 100) || (i <= -10 && i >= -100)),
    (l: List[Int]) => s"List's elements are not in interval [10, 100] or [-100, -10].",
    Xor(
      And(Greater(Witness("10").value), Not(Greater(Witness("100").value))),
      And(Greater(Witness("-100").value), Not(Greater(Witness("-10").value)))
    )
  )

  val x = refineV[AcceptableInterval](List(10, 20, 30, -10, -20))

  println(x)*/

  //refactored so we dont have to define the validator

  type AcceptableInterval2 = Forall[Or[PositiveInterval, Negative]]
  type ListInAcceptableInterval2 = Refined[String, AcceptableInterval2]

  val z = refineV[AcceptableInterval2](List(10, 20, 30, -10, -20))

  println(z)

  // list whereby all elements satisfy a certain predicate

  case class MyClass(s: String, x: Int)

  type MyClassRestrictionOnS = NonEmpty
  type MyClassRestrictionOnX = Positive

  type MyClassRestrictions = And[MyClassRestrictionOnS, MyClassRestrictionOnX]

  type RefinedMyClass = Refined[MyClass, MyClassRestrictions]

  implicit val myClassValidator: Validate[MyClass, MyClassRestrictions] = Validate.fromPredicate[MyClass, MyClassRestrictions](
    (m: MyClass) => m.s.nonEmpty && m.x >= 0,
    (m: MyClass) => "My class doesnt right boii",
    And(Not(Empty()), Greater(shapeless.nat._0))
  )

  println(refineV[MyClassRestrictions](MyClass("", 0)))
}
