package tadp
import scala.util._

trait Parser[A] extends (String => Try[ParserResult[A]]) {

  def <|>[B](parserB: Parser[B]): Parser[Any] = input =>
    Try(this (input).getOrElse(parserB(input).get))

  def <>[B](parserB: Parser[B]): Parser[(A, B)] = input => Try {
    this.andThen(resultA => {
      val resultB = parserB(resultA.get.output).get
      ParserResult((resultA.get.consumed, resultB.consumed), resultB.output)
    }).apply(input)
  }

  def ~>[B](parserB: Parser[B]): Parser[B] = this (_).flatMap(r => parserB(r.output))

  def <~[B](parserB: Parser[B]): Parser[A] = this (_).flatMap(a => parserB(a.output).map(b => a.copy(output = b.output)))

  def stepBy[B](parserB: Parser[B]): Parser[String] = input => Try {
    var pivot: Try[ParserResult[Any]] = this (input)
    var result = pivot
    while (!result.get.output.isEmpty) {
      result = pivot
      pivot = (parserB <> this) (pivot.get.output)
    }
    ParserResult(input, "")
  }

  def satisfies(condition: A => Boolean): Parser[A] = this (_).flatMap {
    case result if condition(result.consumed) => Success(result)
    case _ => Failure(new ParserError)
  }

  def opt(): Parser[Option[A]] = input => this (input) match {
    case Success(ParserResult(consumed, output)) => Success(ParserResult(Option(consumed), output))
    case Failure(_) => Success(ParserResult(Option.empty, input))
  }

  def * : Parser[List[A]] = input => {
    var consumed = List[A]()
    var output = input

    var result = this (input)
    while (result.isSuccess) {
      consumed = consumed :+ result.get.consumed
      output = result.get.output
      result = this (output)
    }

    Success(ParserResult(consumed, output))
  }

  def + : Parser[List[A]] = input =>
    this.* (input) match {
      case Success(ParserResult(List(), _)) => Failure(new ParserError)
      case result => result
    }

  def map[B](transform: A => B): Parser[B] = this(_).map(result => result.copy(consumed = transform(result.consumed)))
}

case class ParserResult[+T](consumed: T, output: String)

class ParserError extends RuntimeException("Parser error")
