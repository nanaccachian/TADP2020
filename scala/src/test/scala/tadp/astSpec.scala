import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._

class astSpec extends AnyFlatSpec with should.Matchers  {
  "blank" should "parsea 1 espacios" in {
    ast.blankParser(" a").get shouldEqual ParserResult(List(' '), "a")
  }

  it should "parsea 3 espacios" in {
    ast.blankParser("   a").get shouldEqual ParserResult(List(' ', ' ', ' '), "a")
  }

  it should "parsea 0 espacios" in {
    ast.blankParser("a").get shouldEqual ParserResult(List(), "a")
  }

  "point" should "parsea correctamente '25.2 @  -3425a'" in {
    ast.pointParser("25.2 @  -3425a").get shouldEqual ParserResult(Point(25.2, -3425), "a")
  }

  it should "no parsea '25.2 ,  -3425'" in {
    an[ParserError] should be thrownBy ast.pointParser("25.2 ,  -3425").get
  }
}
