package tadp
import tadp.parsers._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ParsersSpec extends AnyFlatSpec with should.Matchers {

  //=========================== anyChar ===========================//
  "anyChar" should "recibiendo Hola produce una H" in {
    anyChar("Hola") shouldEqual ParserResultSuccess('H', "ola")
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy anyChar("").getConsumed
  }

  //=========================== char ===========================//
  "char" should "con target H recibiendo Hola produce una H" in {
    char('H')("Hola") shouldEqual ParserResultSuccess('H', "ola")
  }

  it should "con target X recibiendo Hola produce ParserError" in {
    an [ParserError] should be thrownBy char('X')("Hola").getConsumed
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy char('N')("").getConsumed
  }

  //=========================== digit ===========================//
  "digit" should "recibiendo 160 produce una 1" in {
    digit("160") shouldEqual ParserResultSuccess(1, "60")
  }

  it should "recibiendo Hola produce ParserError" in {
    an [ParserError] should be thrownBy digit("Hola").getConsumed
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy digit("").getConsumed
  }

  //=========================== string ===========================//
  "string" should "con target Hola recibiendo Hola Mundo! produce una Hola" in {
    string("Hola")("Hola Mundo!") shouldEqual ParserResultSuccess("Hola", " Mundo!")
  }

  it should "con target Hola recibiendo Chau Mundo! produce ParserError" in {
    an [ParserError] should be thrownBy string("Chau Mundo")("Hola").getConsumed
  }

  it should "con target Hola recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy string("Hola")("").getConsumed
  }

  //=========================== integer ===========================//
  "integer" should "recibiendo 5384Hola produce 5384" in {
    integer("5384Hola") shouldEqual ParserResultSuccess(5384, "Hola")
  }

  it should "recibiendo 5384 produce 5384" in {
    integer("5384") shouldEqual ParserResultSuccess(5384, "")
  }

  it should "recibiendo -5384 produce -5384" in {
    integer("-5384") shouldEqual ParserResultSuccess(-5384, "")
  }

  it should "recibiendo Hola5384 produce ParserError" in {
    an [ParserError] should be thrownBy integer("Hola5384").getConsumed
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy integer("").getConsumed
  }

  //=========================== double ===========================//
  "double" should "recibiendo 5384Hola produce 5384" in {
    double("5384Hola") shouldEqual ParserResultSuccess(5384, "Hola")
  }

  it should "recibiendo 5384 produce 5384" in {
    double("5384") shouldEqual ParserResultSuccess(5384, "")
  }

  it should "recibiendo 0.01 produce 0.01" in {
    double("0.01") shouldEqual ParserResultSuccess(0.01, "")
  }

  it should "recibiendo -0.01 produce -0.01" in {
    double("-0.01") shouldEqual ParserResultSuccess(-0.01, "")
  }

  it should "recibiendo 53.842 produce 53.842" in {
    double("53.842") shouldEqual ParserResultSuccess(53.842, "")
  }

  it should "recibiendo -513.842 produce 513.842" in {
    double("-513.842") shouldEqual ParserResultSuccess(-513.842, "")
  }

  it should "recibiendo -5384 produce -5384" in {
    double("-5384") shouldEqual ParserResultSuccess(-5384, "")
  }

  it should "recibiendo -25. produce ParserError" in {
    double("-25.") shouldEqual ParserResultSuccess(-25, ".")
  }

  it should "recibiendo .25 produce ParserError" in {
    an [ParserError] should be thrownBy double(".25").getConsumed
  }

  it should "recibiendo Hola5384 produce ParserError" in {
    an [ParserError] should be thrownBy double("Hola5384").getConsumed
  }

  it should "recibiendo string vacío produce ParserError" in {
    an [ParserError] should be thrownBy double("").getConsumed
  }
}
