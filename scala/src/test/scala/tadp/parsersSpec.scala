import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parser._
import parsers._

class ParsersSpect extends AnyFlatSpec with should.Matchers {

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
  "integer" should "recibiendo 5384Hola produce una 5384" in {
    integer("5384Hola").get shouldEqual ParserResult(5384, "Hola")
  }

  "integer" should "recibiendo 5384 produce una 5384" in {
    integer("5384").get shouldEqual ParserResult(5384, "")
  }

  it should "recibiendo Hola5384 produce ParserError" in {
    an [ParserError] should be thrownBy integer("Hola5384").get
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy integer("").get
  }
}