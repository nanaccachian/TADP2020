import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._
import tadp.parsers._

class OperatorsSpec extends AnyFlatSpec with should.Matchers {
  "satisfies" should "anyChar consume h recibiendo hola validando == h" in {
    anyChar.satisfies(_ == 'h')("hola").get shouldEqual ParserResult('h', "ola")
  }

  it should "anyChar produce excepción recibiendo hola validando == a" in {
    an[ParserError] should be thrownBy anyChar.satisfies(_ == 'a')("hola").get
  }

  "opt" should "char 'h' devuelve Option('h') recibiendo hola" in {
    char('h').opt()("hola").get shouldEqual ParserResult(Option('h'), "ola")
  }

  it should "char 'h' devuelve Option('h') recibiendo chau" in {
    char('h').opt()("chau").get shouldEqual ParserResult(Option.empty, "chau")
  }

  "*" should "char 'a' lee [a,a,a] recibiendo aaahh" in {
    char('a').*("aaahh").get shouldEqual ParserResult(List('a', 'a', 'a'), "hh")
  }

  it should "char 'a' lee [a,a,a] recibiendo aaa" in {
    char('a').*("aaa").get shouldEqual ParserResult(List('a', 'a', 'a'), "")
  }

  it should "char 'a' lee [] recibiendo hh" in {
    char('a').*("hh").get shouldEqual ParserResult(List.empty, "hh")
  }

  "+" should "char 'a' lee [a,a,a] recibiendo aaahh" in {
    char('a').+("aaahh").get shouldEqual ParserResult(List('a', 'a', 'a'), "hh")
  }

  it should "char 'a' lee [a,a,a] recibiendo aaa" in {
    char('a').+("aaa").get shouldEqual ParserResult(List('a', 'a', 'a'), "")
  }

  it should "char 'a' produce excepción recibiendo hh" in {
    an[ParserError] should be thrownBy char('a').+("hh").get
  }

  "map" should "char '3' mapeado toInt devuelve int 3" in {
    char('3').map(_.toInt - 48) ("3a").get shouldEqual ParserResult(3, "a")
  }
}
