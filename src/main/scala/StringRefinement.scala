import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import shapeless.{Witness => W}

object StringRefinement extends App {
  type Nino = MatchesRegex[W. `"""abc"""` .T]

  val nino = refineV[Nino]("abc")

  type DeparturePort = MatchesRegex[W.`"(Calais)|(Coquelles)|(Dublin)|(Dunkirk)"`.T]

  println(refineV[DeparturePort]("Coquelles"))
}
