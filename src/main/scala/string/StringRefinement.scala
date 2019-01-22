package string

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.{And, AnyOf, Or}
import eu.timepit.refined.collection.{Contains, Exists, NonEmpty, Size}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.LessEqual
import eu.timepit.refined.string.{MatchesRegex, Regex, Uri, Url}

object StringRefinement extends App {
  type Nino = MatchesRegex[W. `"""abc"""` .T]

  val nino = refineV[Nino]("abc")

  // playing around with MatchesRegex
  type ValidPort     = MatchesRegex[W.`"""(Calais)|(Coquelles)|(Dublin)|(Dunkirk)"""`.T]
  type ValidPort2    = Exists[Equal[W.`"Calais"`.T] Or Equal[W.`"Coquelles"`.T] Or Equal[W.`"Dublin"`.T] Or Equal[W.`"Dunkirk"`.T]]
  type DeparturePort = Refined[String, ValidPort]
  type DeparturePort2 = Refined[String, ValidPort2]

  val x: Either[String, DeparturePort] = refineV[ValidPort]("Cal ais")
  lazy val z: Either[String, DeparturePort2] = ???

  println(x)
  // println(z)

  // making sense of the Uri predicate and the And which unites 2 predicates
  // unfortunately, can't mix Contains
  type YoutubeRegex = MatchesRegex[W.`""".*youtube.com.*"""`.T]

  type ValidYoutubeLink = Url And YoutubeRegex

  type YoutubeLink = Refined[String, ValidYoutubeLink]

  val y = refineV[And[Url, YoutubeRegex]]("http://www.youtube.com/watch?=adsfasdfasd")

  println(y)

  // experiment
  type NonEmptyLengthyString = NonEmpty And Size[LessEqual[shapeless.nat._12]]
  type NonEmptyLengthyString2 = NonEmpty And Size[LessEqual[W.`65`.T]]

  type String2 = Refined[String, NonEmptyLengthyString]
  type String3 = Refined[String, NonEmptyLengthyString2]

  println(refineV[NonEmptyLengthyString]("asdfa"))
  println(refineV[NonEmptyLengthyString2]("asdfa"))
}
