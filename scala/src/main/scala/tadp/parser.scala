package tadp

import scala.annotation.tailrec

trait Parser[+A] extends (String => ParserResult[A]) {

  def <|>[U >: A](parserB: => Parser[U]): Parser[U] = input => this (input).orElse(parserB(input))
  //def <|>[Padre >: A, U <: Padre](parserB: => Parser[U]): Parser[Padre] = input => this (input).orElse(parserB(input))

  def <>[B](parserB: => Parser[B]): Parser[(A, B)] = this(_).flatMap{(consumedA, outputA) => parserB(outputA).map{(consumedA, _)}}

  def ~>[B](parserB: => Parser[B]): Parser[B] = this (_).flatMap((_, outputA) => parserB(outputA))

  def <~[B](parserB: => Parser[B]): Parser[A] = this (_).flatMap((consumedA, outputA) => parserB(outputA).flatMap((_, outputB) => ParserResultSuccess(consumedA, outputB)))

  def sepBy[B](parserB: => Parser[B]): Parser[List[A]] = (this <> (parserB ~> this).*).map { case (first, list) => first :: list }

  def satisfies(condition: => A => Boolean): Parser[A] = this (_).flatMap {
    (consumed, output) =>
      if (condition(consumed)) ParserResultSuccess(consumed, output)
      else ParserResultFailure()
  }

  def opt(): Parser[Option[A]] = input => this(input).map(Some(_)).orElse(ParserResultSuccess(None, input))

  def * : Parser[List[A]] = input => {
    @tailrec
    def recursiveKleene(output: String, consumed: List[A]): ParserResult[List[A]] = {
      this (output) match {
        case ParserResultSuccess(parsed, parsedOutput) => recursiveKleene(parsedOutput, consumed :+ parsed)
        case ParserResultFailure() => ParserResultSuccess(consumed, output)
      }
    }
    recursiveKleene(input, List())
  }

  def + : Parser[List[A]] = (this <> this.*).map { case (first, list) => first :: list }

  def map[B](transform: => A => B): Parser[B] = this (_).map(transform)
}
