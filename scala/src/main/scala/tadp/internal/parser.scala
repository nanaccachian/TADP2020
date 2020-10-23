
import scala.util.Try

package object parser {

  case class Parser[+A](method: String => ParserResult[A]) extends (String => Try[ParserResult[A]]) {
    override def apply(input: String): Try[ParserResult[A]] = Try {
      method(input)
    } recover {
      case _ => throw ParserError()
    }

    def <>[B](parserB: Parser[B]): Parser[(A, B)] = Parser { input =>
      val resultA = this (input).get
      val resultB = parserB(resultA.input).get
      ParserResult((resultA.consumed, resultB.consumed), resultB.input)
    }

    def <|>[B](parserB: Parser[B]): Parser[Any] = Parser { input =>
      var result: Try[ParserResult[Any]] = this (input)
      if (result.isFailure)
        result = parserB(input)
      result.get
    }

    def ~>[B](parserB: Parser[B]): Parser[B] = Parser { input =>
      parserB(this (input).get.input).get
    }

    def <~[B](parserB: Parser[B]): Parser[A] = Parser { input =>
      val resultA = this (input).get
      ParserResult(resultA.consumed, parserB(resultA.input).get.input)
    }

    def stepBy[B](parserB: Parser[B]): Parser[String] = Parser { input =>
      var pivot: Try[ParserResult[Any]] = this (input)
      var result = pivot
      while (pivot.isSuccess) {
        result = pivot
        pivot = parserB(pivot.get.input)
        if(pivot.isSuccess)
          pivot = this (pivot.get.input)
      }
      if (!result.get.input.isEmpty)
        throw ParserError()
      ParserResult(input, "")
    }
  }

  case class ParserResult[+T](consumed: T, input: String)

  case class ParserError() extends RuntimeException

  def tryParse[T](method: => ParserResult[T]): Try[ParserResult[T]] = Try {
    method
  } recover {
    case _ => throw ParserError()
  }
}