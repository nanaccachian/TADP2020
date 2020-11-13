import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._
import tadp.parsers._

class CombinatorsSpec extends AnyFlatSpec with should.Matchers {

  "<|>" should "FELIZ A" in {
    val holaOadios: Parser[String] = string("hola") <|> string("adios")
    holaOadios("holamundo") shouldEqual ParserResultSuccess("hola", "mundo")
  }

  it should "FELIZ B" in {
    val holaOadios: Parser[String] = string("hola") <|> string("adios")
    holaOadios("adiosmundo") shouldEqual ParserResultSuccess("adios", "mundo")
  }

  it should "FELIZ C" in {
    val holaOint: Parser[Any] = string("hola") <|> integer
    holaOint("123mundo") shouldEqual ParserResultSuccess(123, "mundo")
  }

  it should "FELIZ D" in {
    val holaOint: Parser[Any] = string("hola") <|> integer
    holaOint("holamundo") shouldEqual ParserResultSuccess("hola", "mundo")
  }

  it should "TRISTE" in {
    val holaOadios: Parser[String] = string("hola") <|> string("adios")
    an[ParserError] should be thrownBy holaOadios("chaumundo").getConsumed
  }

  "<>" should "FELIZ" in {
    val holaMundo: Parser[(String, String)] = string("hola") <> string("mundo")
    holaMundo("holamundolindo") shouldEqual ParserResultSuccess(("hola", "mundo"), "lindo")
  }

  it should "TRISTE" in {
    val holaMundo: Parser[(String, String)] = string("hola") <> string("mundo")
    an[ParserError] should be thrownBy holaMundo("adiosmundolindo").getConsumed
  }

  "~>" should "FELIZ" in {
    val holamundo: Parser[String] = string("hola") ~> string("mundo")
    holamundo("holamundolindo") shouldEqual ParserResultSuccess("mundo", "lindo")
  }

  it should "TRISTE" in {
    val mundohola: Parser[String] = string("mundo") ~> string("hola")
    an[ParserError] should be thrownBy mundohola("holamundolindo").getConsumed
  }

  "<~" should "FELIZ" in {
    val holamundo: Parser[String] = string("hola") <~ string("mundo")
    holamundo("holamundolindo") shouldEqual ParserResultSuccess("hola", "lindo")
  }

  it should "TRISTE" in {
    val mundohola: Parser[String] = string("mundo") <~ string("hola")
    an[ParserError] should be thrownBy mundohola("holamundolindo").getConsumed
  }

  "sepBy" should "FELIZ A" in {
    val telefono: Parser[List[Int]] = integer sepBy char('-')
    telefono("4356-1234") shouldEqual ParserResultSuccess(List(4356, 1234), "")
  }

  it should "FELIZ B" in {
    val telefono: Parser[List[Char]] = char('a') sepBy char('-')
    telefono("a-a-a-a-a") shouldEqual ParserResultSuccess(List('a', 'a', 'a', 'a', 'a'), "")
  }

  it should "FELIZ C" in {
    val telefono: Parser[List[Int]] = integer sepBy char('-')
    telefono("4356 1234") shouldEqual ParserResultSuccess(List(4356), " 1234")
  }

  it should "FELIZ D" in {
    val telefono: Parser[List[Char]] = char('a') sepBy char('-')
    telefono("a-a-a-") shouldEqual ParserResultSuccess(List('a', 'a', 'a'), "-")
  }

  it should "FELIZ E" in {
    val telefono: Parser[List[Char]] = (char('a') <|> char('b')) sepBy char('-')
    telefono("a-b-a-b-a-b") shouldEqual ParserResultSuccess(List('a', 'b', 'a', 'b', 'a', 'b'), "")
  }

  it should "TRISTE B" in {
    val telefono: Parser[List[Int]] = integer sepBy char('/')
    an[ParserError] should be thrownBy telefono("/4356/1234").getConsumed
  }
}
