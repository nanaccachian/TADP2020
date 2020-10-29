import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._

class CombinatorsSpec extends AnyFlatSpec with should.Matchers {

  "<|>" should "FELIZ A" in {
    val holaOadios: Parser[Any] = string("hola") <|> integer
    holaOadios("holamundo").get shouldEqual ParserResult("hola", "mundo")
  }

  it should "FELIZ B" in {
    val holaOadios: Parser[Any] = string("hola") <|> integer
    holaOadios("5384hola").get shouldEqual ParserResult(5384, "hola")
  }

  it should "TRISTE" in {
    val holaOadios: Parser[Any] = string("hola") <|> string("adios")
    an[ParserError] should be thrownBy holaOadios("chaumundo").get
  }

  "<>" should "FELIZ" in {
    val holaMundo: Parser[(String, String)] = string("hola") <> string("mundo")
    holaMundo("holamundolindo").get shouldEqual ParserResult(("hola", "mundo"), "lindo")
  }

  it should "TRISTE" in {
    val holaMundo: Parser[(String, String)] = string("hola") <> string("mundo")
    an[ParserError] should be thrownBy holaMundo("adiosmundolindo").get
  }

  "~>" should "FELIZ" in {
    val holamundo: Parser[String] = string("hola") ~> string("mundo")
    holamundo("holamundolindo").get shouldEqual ParserResult("mundo", "lindo")
  }

  it should "TRISTE" in {
    val mundohola: Parser[String] = string("mundo") ~> string("hola")
    an[ParserError] should be thrownBy mundohola("holamundolindo").get
  }

  "<~" should "FELIZ" in {
    val holamundo: Parser[String] = string("hola") <~ string("mundo")
    holamundo("holamundolindo").get shouldEqual ParserResult("hola", "lindo")
  }

  it should "TRISTE" in {
    val mundohola: Parser[String] = string("mundo") <~ string("hola")
    an[ParserError] should be thrownBy mundohola("holamundolindo").get
  }

  "sepBy" should "FELIZ A" in {
    val telefono: Parser[String] = integer sepBy char('-')
    telefono("4356-1234").get shouldEqual ParserResult("4356-1234", "")
  }

  it should "FELIZ B" in {
    val telefono: Parser[String] = char('a') sepBy char('-')
    telefono("a-a-a-a-a").get shouldEqual ParserResult("a-a-a-a-a", "")
  }

  it should "TRISTE A" in {
    val telefono: Parser[String] = integer sepBy char('-')
    an[ParserError] should be thrownBy telefono("4356 1234").get
  }

  it should "TRISTE B" in {
    val telefono: Parser[String] = integer sepBy char('/')
    an[ParserError] should be thrownBy telefono("/4356/1234").get
  }

  it should "TRISTE C" in {
    val telefono: Parser[String] = char('a') sepBy char('-')
    an[ParserError] should be thrownBy telefono("a-a-a-").get
  }
}
