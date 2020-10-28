package tadp

object ast {

  def blankParser: Parser[List[Char]] = (char(' ') <|> char('\n') <|> char('\t')).*

  def pointParser: Parser[Point] = (blankParser ~> double <~ blankParser <~ char('@') <~ blankParser <> double <~ blankParser).map(tuple => Point(tuple._1, tuple._2))

  def intParser: Parser[Int] = blankParser ~> integer <~ blankParser

  def doubleParser: Parser[Double] = blankParser ~> double <~ blankParser

  def commaParser: Parser[Char] = blankParser ~> char(',') <~ blankParser

  def shapeParser: Parser[Shape] = blankParser ~> (rectangleParser <|> triangleParser <|> circleParser <|> groupParser <|> colorParser <|> scaleParser <|> rotateParser) <~ blankParser

  def rectangleParser: Parser[Shape] = (string("rectangulo") ~> blankParser ~> char('[') ~> pointParser <~ commaParser <> pointParser <~ char(']')).map(pointTuple => rectangle(pointTuple._1, pointTuple._2))

  def triangleParser: Parser[Shape] = (string("triangulo") ~> blankParser ~> char('[') ~> pointParser <~ commaParser <> pointParser <~ commaParser <> pointParser <~ char(']')).map(pointTuple => triangle(pointTuple._1._1, pointTuple._1._2, pointTuple._2))

  def circleParser: Parser[Shape] = (string("circulo") ~> blankParser ~> char('[') ~> pointParser <~ commaParser <> doubleParser <~ char(']')).map(pointTuple => circle(pointTuple._1, pointTuple._2))

  def groupParser: Parser[Shape] = (string("grupo") ~> blankParser ~> char('(') ~> shapeParser.stepBy(commaParser) <~ char(')')).map (shapes => group(shapes))

  def colorParser: Parser[Shape] = (string("color") ~> blankParser ~> char('[') ~> intParser <~ commaParser <> intParser <~ commaParser <> intParser <~ char(']') <~ blankParser <> char('(') ~> shapeParser <~ char(')')).map {
    case (((colorR, colorG), colorB), shape) => color(colorR, colorG, colorB)(shape)
  }

  def scaleParser: Parser[Shape] = (string("escala") ~> blankParser ~> char('[') ~> doubleParser <~ commaParser <> doubleParser <~ char(']') <~ blankParser <> char('(') ~> shapeParser <~ char(')')).map {
    case ((scaleX, scaleY), shape) => scale(scaleX, scaleY)(shape)
  }

  def rotateParser: Parser[Shape] = (string("rotacion") ~> blankParser ~> char('[') ~> doubleParser <~ char(']') <~ blankParser <> char('(') ~> shapeParser <~ char(')')).map {
    case (angle, shape) => rotate(angle)(shape)
  }

  def parser: Parser[Shape] = shapeParser
}
