import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.boolean.And
import eu.timepit.refined.numeric._

object Experiments extends App {
  type Fizz     = Divisible[shapeless.nat._3]
  type Buzz     = Divisible[shapeless.nat._5]
  type FizzBuzz = And[Fizz, Buzz]

  type PositiveInt = Int Refined Positive

  def fizzBuzz(upTo: PositiveInt): List[String] = {
    def isOfType[T](i: Int, s: String)(implicit v: Validate[Int, T]): Option[String] = refineV[T](i).map(_ => s).toOption

    (0 to upTo.value).toList.map { e =>
      isOfType[FizzBuzz](e, "FizzBuzz")
        .getOrElse(isOfType[Fizz](e, "Fizz").getOrElse(isOfType[Buzz](e, "Buzz").getOrElse(e.toString)))
    }
  }

  val input: PositiveInt = refineMV[Positive](100)
  println(fizzBuzz(input))
}
