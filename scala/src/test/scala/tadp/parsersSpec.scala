import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers._

class Parsers extends AnyFlatSpec with should.Matchers {

  //=========================== anyChar ===========================//
  "anyChar" should "recibiendo Hola produce una H" in {
    anyChar("Hola").get shouldEqual ParserResult('H', "ola")
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy anyChar("").get
  }

  //=========================== char ===========================//
  "char" should "con target H recibiendo Hola produce una H" in {
    char('H')("Hola").get shouldEqual ParserResult('H', "ola")
  }

  it should "con target X recibiendo Hola produce ParserError" in {
    an [ParserError] should be thrownBy char('X')("Hola").get
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy char('N')("").get
  }

  //=========================== digit ===========================//
  "digit" should "recibiendo 160 produce una 1" in {
    digit("160").get shouldEqual ParserResult(1, "60")
  }

  it should "recibiendo Hola produce ParserError" in {
    an [ParserError] should be thrownBy digit("Hola").get
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy digit("").get
  }

  //=========================== string ===========================//
  "string" should "con target Hola recibiendo Hola Mundo! produce una Hola" in {
    string("Hola")("Hola Mundo!").get shouldEqual ParserResult("Hola", " Mundo!")
  }

  it should "con target Hola recibiendo Chau Mundo! produce ParserError" in {
    an [ParserError] should be thrownBy string("Chau Mundo")("Hola").get
  }

  it should "con target Hola recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy string("Hola")("").get
  }

  //=========================== integer ===========================//
  "integer" should "con target 12 recibiendo 1204 produce una 12" in {
    integer(12)("1204").get shouldEqual ParserResult(12, "04")
  }

  it should "con target 12 recibiendo 4321 produce ParserError" in {
    an [ParserError] should be thrownBy integer(12)("4321").get
  }

  it should "con target 12 recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy integer(12)("").get
  }
}