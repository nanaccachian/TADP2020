import parsers._

package object combinators {

  implicit class Combinators[A](parserA: Parser[A]) {
    def <>[B](parserB: Parser[B]): Parser[(A, B)] =
      (input: String) => tryParse {
        val resultA = parserA(input).get
        val resultB = parserB(resultA.input).get
        ParserResult((resultA.consumed, resultB.consumed), resultB.input)
      }

    def <|>(parserB: Parser[A]): Parser[A] =
      (input: String) => tryParse {
        var result = parserA(input)
        if(result.isFailure)
          result = parserB(input)
        result.get
      }
  }
}