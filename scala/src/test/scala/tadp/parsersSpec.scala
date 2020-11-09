package tadp
import tadp.parsers._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ParsersSpec extends AnyFlatSpec with should.Matchers {

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
  "integer" should "recibiendo 5384Hola produce 5384" in {
    integer("5384Hola").get shouldEqual ParserResult(5384, "Hola")
  }

  it should "recibiendo 5384 produce 5384" in {
    integer("5384").get shouldEqual ParserResult(5384, "")
  }

  it should "recibiendo -5384 produce -5384" in {
    integer("-5384").get shouldEqual ParserResult(-5384, "")
  }

  it should "recibiendo Hola5384 produce ParserError" in {
    an [ParserError] should be thrownBy integer("Hola5384").get
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy integer("").get
  }

  //=========================== double ===========================//
  "double" should "recibiendo 5384Hola produce 5384" in {
    double("5384Hola").get shouldEqual ParserResult(5384, "Hola")
  }

  it should "recibiendo 5384 produce 5384" in {
    double("5384").get shouldEqual ParserResult(5384, "")
  }

  it should "recibiendo 0.01 produce 0.01" in {
    double("0.01").get shouldEqual ParserResult(0.01, "")
  }

  it should "recibiendo -0.01 produce -0.01" in {
    double("-0.01").get shouldEqual ParserResult(-0.01, "")
  }

  it should "recibiendo 53.842 produce 53.842" in {
    double("53.842").get shouldEqual ParserResult(53.842, "")
  }

  it should "recibiendo -513.842 produce 513.842" in {
    double("-513.842").get shouldEqual ParserResult(-513.842, "")
  }

  it should "recibiendo -5384 produce -5384" in {
    double("-5384").get shouldEqual ParserResult(-5384, "")
  }

  it should "recibiendo -25. produce ParserError" in {
    double("-25.").get shouldEqual ParserResult(-25, ".")
  }

  it should "recibiendo .25 produce ParserError" in {
    an [ParserError] should be thrownBy double(".25").get
  }

  it should "recibiendo Hola5384 produce ParserError" in {
    an [ParserError] should be thrownBy double("Hola5384").get
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy double("").get
  }
}
