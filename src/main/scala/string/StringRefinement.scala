package string

import eu.timepit.refined._
import eu.timepit.refined.string.MatchesRegex

object StringRefinement extends App {
  type Nino = MatchesRegex[W. `"""abc"""` .T]

  val nino = refineV[Nino]("abc")

  type DeparturePort = MatchesRegex[W.`"""(Cal ais)|(Coquelles)|(Dublin)|(Dunkirk)"""`.T]

  println(refineV[DeparturePort]("Cal ais"))

}
