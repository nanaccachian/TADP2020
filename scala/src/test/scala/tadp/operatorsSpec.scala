import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._
import tadp.parsers._

class OperatorsSpec extends AnyFlatSpec with should.Matchers {
  "satisfies" should "anyChar consume h recibiendo hola validando == h" in {
    anyChar.satisfies(_ == 'h')("hola") shouldEqual ParserResultSuccess('h', "ola")
  }

  it should "anyChar produce excepción recibiendo hola validando == a" in {
    an[ParserError] should be thrownBy anyChar.satisfies(_ == 'a')("hola").getConsumed
  }

  "opt" should "char 'h' devuelve Option('h') recibiendo hola" in {
    char('h').opt()("hola") shouldEqual ParserResultSuccess(Option('h'), "ola")
  }

  it should "char 'h' devuelve Option('h') recibiendo chau" in {
    char('h').opt()("chau") shouldEqual ParserResultSuccess(Option.empty, "chau")
  }

  "*" should "char 'a' lee [a,a,a] recibiendo aaahh" in {
    char('a').*("aaahh") shouldEqual ParserResultSuccess(List('a', 'a', 'a'), "hh")
  }

  it should "char 'a' lee [a,a,a] recibiendo aaa" in {
    char('a').*("aaa") shouldEqual ParserResultSuccess(List('a', 'a', 'a'), "")
  }

  it should "char 'a' lee [] recibiendo hh" in {
    char('a').*("hh") shouldEqual ParserResultSuccess(List.empty, "hh")
  }

  "+" should "char 'a' lee [a,a,a] recibiendo aaahh" in {
    char('a').+("aaahh") shouldEqual ParserResultSuccess(List('a', 'a', 'a'), "hh")
  }

  it should "char 'a' lee [a,a,a] recibiendo aaa" in {
    char('a').+("aaa") shouldEqual ParserResultSuccess(List('a', 'a', 'a'), "")
  }

  it should "char 'a' produce excepción recibiendo hh" in {
    an[ParserError] should be thrownBy char('a').+("hh").getConsumed
  }

  "map" should "char '3' mapeado toInt devuelve int 3" in {
    char('3').map(_.toInt - 48) ("3a") shouldEqual ParserResultSuccess(3, "a")
  }
}
