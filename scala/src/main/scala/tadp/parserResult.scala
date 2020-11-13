package tadp

abstract class ParserResult[+T]() {
  def map[U](transform: T => U): ParserResult[U]
  def flatMap[U](transform: (T, String) => ParserResult[U]): ParserResult[U]
  val getConsumed: T
  val getOutput: String
  def orElse[U >: T](eLse: => ParserResult[U]): ParserResult[U]
}

case class ParserResultSuccess[T](consumed: T, output: String) extends ParserResult[T] {
  def map[U](transform: T => U): ParserResult[U] = copy(consumed = transform(consumed))
  def flatMap[U](transform: (T, String) => ParserResult[U]): ParserResult[U] = transform(consumed, output)
  lazy val getConsumed: T = consumed
  lazy val getOutput: String = output
  def orElse[U >: T](eLse: => ParserResult[U]): ParserResult[U] = this
}

case class ParserResultFailure[T]() extends ParserResult[T] {
  def map[U](transform: T => U): ParserResult[U] = ParserResultFailure[U]
  def flatMap[U](transform: (T, String) => ParserResult[U]): ParserResult[U] = ParserResultFailure[U]
  lazy val getConsumed: T = throw new ParserError
  lazy val getOutput: String = throw new ParserError
  def orElse[U >: T](eLse: => ParserResult[U]): ParserResult[U] = eLse
}

class ParserError extends RuntimeException("Parser error")