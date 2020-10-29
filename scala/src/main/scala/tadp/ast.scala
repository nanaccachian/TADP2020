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
    case (((colorR, colorG), colorB), shape) => color(colorR, colorG, colorB, shape)
  }

  def scaleParser: Parser[Shape] = (string("escala") ~> blankParser ~> char('[') ~> doubleParser <~ commaParser <> doubleParser <~ char(']') <~ blankParser <> char('(') ~> shapeParser <~ char(')')).map {
    case ((scaleX, scaleY), shape) => scale(scaleX, scaleY, shape)
  }

  def rotateParser: Parser[Shape] = (string("rotacion") ~> blankParser ~> char('[') ~> doubleParser <~ char(']') <~ blankParser <> char('(') ~> shapeParser <~ char(')')).map {
    case (angle, shape) => rotate(angle, shape)
  }

  def parser: Parser[Shape] = shapeParser
  def parserSimplified: String => Shape = shapeParser.andThen{_.get.consumed}

  def colorGroup(input: List[Shape]): Boolean = input match {
    case list @ List(color(rA, gA, bA, _), color(rB, gB, bB, _), _*) if rA == rB && gA == gB && bA == bB => colorGroup(list.tail)
    case List(_) => true
    case _ => false
  }
  def rotateGroup(input: List[Shape]): Boolean = input match {
    case list @ List(rotate(angleA, _), rotate(angleB, _), _*) if angleA == angleB => rotateGroup(list.tail)
    case List(_) => true
    case _ => false
  }
  def scaleGroup(input: List[Shape]): Boolean = input match {
    case list @ List(scale(scaleXA, scaleYA, _), scale(scaleXB, scaleYB, _), _*) if scaleXA == scaleYA && scaleXA == scaleYA  => scaleGroup(list.tail)
    case List(_) => true
    case _ => false
  }
  def translateGroup(input: List[Shape]): Boolean = input match {
    case list @ List(translate(transXA, transYA, _), translate(transXB, transYB, _), _*) if transXA == transXB && transYA == transYB => translateGroup(list.tail)
    case List(_) => true
    case _ => false
  }

  def simplify(input: Shape): Shape = input match {
    case color(_, _, _, childColor: color) => simplify(childColor)

    case group(list @ color(r, g, b, child) :: tail) if colorGroup(list) => simplify(color(r, g, b, group(child :: tail.map{case color(_, _, _, child) => child})))
    case group(list @ rotate(angle, child) :: tail) if rotateGroup(list) => simplify(rotate(angle, group(child :: tail.map{case rotate(_, child) => child})))
    case group(list @ scale(scaleX, scaleY, child) :: tail) if scaleGroup(list) => simplify(scale(scaleX, scaleY, group(child :: tail.map{case scale(_, _, child) => child})))
    case group(list @ translate(transX, transY, child) :: tail) if translateGroup(list) => simplify(translate(transX, transY, group(child :: tail.map{case translate(_, _, child) => child})))

    case rotate(gradesA, rotate(gradesB, child)) => simplify(rotate(gradesA + gradesB, child))
    case scale(scaleXA, scaleYA, scale(scaleXB, scaleYB, child)) => simplify(scale(scaleXA * scaleXB, scaleYA * scaleYB, child))
    case translate(transXA, transYA, translate(transXB, transYB, child)) => simplify(translate(transXA + transXB, transYA + transYB, child))
    case rotate(angle, child) if angle % 360 == 0 => simplify(child)
    case scale(1, 1, child) => simplify(child)
    case translate(0, 0, child) => simplify(child)

    case group(list) => group(list.map{simplify(_)})
    case color(r, g, b, child) => color(r, g, b, simplify(child))
    case rotate(angle, child) => rotate(angle, simplify(child))
    case scale(scaleX, scaleY, child) => scale(scaleX, scaleY, simplify(child))
    case translate(transX, transY, child) => translate(transX, transY, simplify(child))

    case _ => input
  }
}
