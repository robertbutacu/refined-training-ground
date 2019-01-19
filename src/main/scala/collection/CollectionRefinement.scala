package collection

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.{And, Not}
import eu.timepit.refined.collection.{Contains, Forall, MinSize}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{Greater, Positive}
import shapeless.Nat._0

object CollectionRefinement extends App {
  //list with more than 1 elements
  type NonEmptyListValidator = MinSize[shapeless.nat._1]

  type NonEmptyList = Refined[List[Int], NonEmptyListValidator]

  val nonEmptyList = refineV[NonEmptyListValidator](List(-2, -1, 0, 2))

  println(nonEmptyList)

  //list with at least 1 positive element
  type PositiveElementValidator = Contains[Positive]
  type PositiveElementList[N]      = Refined[List[N], PositiveElementValidator]

  implicit val validator: Validate[List[Int], PositiveElementValidator] = Validate.fromPredicate[List[Int], PositiveElementValidator](
    (l: List[Int]) => l.exists(e => e > 0),
      (l: List[Int]) => s"$l contains only negative elements",
    Not(Forall(Not(Equal(Greater(_0)))))
  )

  val positiveElementList = refineV[PositiveElementValidator](List(-1))

  println(positiveElementList)
}
