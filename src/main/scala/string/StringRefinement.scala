package string

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.string.{MatchesRegex, Regex, Uri, Url}

object StringRefinement extends App {
  type Nino = MatchesRegex[W. `"""abc"""` .T]

  val nino = refineV[Nino]("abc")

  // playing around with MatchesRegex
  type ValidPort     = MatchesRegex[W.`"""(Cal ais)|(Coquelles)|(Dublin)|(Dunkirk)"""`.T]
  type DeparturePort = Refined[String, ValidPort]

  val x: Either[String, DeparturePort] = refineV[ValidPort]("Cal ais")

  println(x)

  // making sense of the Uri predicate and the And which unites 2 predicates
  // unfortunately, can't mix Contains
  type YoutubeRegex = MatchesRegex[W.`""".*youtube.com.*"""`.T]

  type ValidYoutubeLink = Url And YoutubeRegex

  type YoutubeLink = Refined[String, ValidYoutubeLink]

  val y = refineV[And[Url, YoutubeRegex]]("http://www.youtube.com/watch?=adsfasdfasd")

  println(y)
}
