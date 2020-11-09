package tadp

import scala.annotation.tailrec

object ast {
  def blankParser: Parser[List[Char]] = (char(' ') <|> char('\n') <|> char('\t')).*

  def spacedCharParser(c: Char): Parser[Char] = blankParser ~> char(c) <~ blankParser

  def pointParser: Parser[Point] = (double <~ spacedCharParser('@') <> double).map { case (x, y) => Point(x, y) }

  def commaParser: Parser[Char] = spacedCharParser(',')

  def shapeParser: Parser[Shape] = blankParser ~> (rectangleParser <|> triangleParser <|> circleParser <|> groupParser <|> colorParser <|> scaleParser <|> rotateParser) <~ blankParser

  // TODO generalizen los que usan [] y () así no tienen que repetir los mismos parsers varias veces (ej: hagan una 
  //  funcion que recibe el parser de lo que está adentro del parentesis por parámetro y 
  //  retorna un parser que maneja los parentesis y el nombre)
  
  def rectangleParser: Parser[Shape] = (string("rectangulo") ~> spacedCharParser('[') ~> pointParser <~ commaParser <> pointParser <~ spacedCharParser(']')).map { case (pA, pB) => rectangle(pA, pB) }

  def triangleParser: Parser[Shape] = (string("triangulo") ~> spacedCharParser('[') ~> pointParser <~ commaParser <> pointParser <~ commaParser <> pointParser <~ spacedCharParser(']')).map { case ((pA, pB), pC) => triangle(pA, pB, pC) }

  def circleParser: Parser[Shape] = (string("circulo") ~> spacedCharParser('[') ~> pointParser <~ commaParser <> double <~ spacedCharParser(']')).map { case (o, r) => circle(o, r) }

  def groupParser: Parser[Shape] = (string("grupo") ~> spacedCharParser('(') ~> shapeParser.sepBy(commaParser) <~ spacedCharParser(')')).map { group(_) }

  def colorParser: Parser[Shape] = (string("color") ~> spacedCharParser('[') ~> integer <~ commaParser <> integer <~ commaParser <> integer <~ spacedCharParser(']') <> spacedCharParser('(') ~> shapeParser <~ spacedCharParser(')')).map {
    case (((colorR, colorG), colorB), shape) => color(colorR, colorG, colorB, shape)
  }

  def scaleParser: Parser[Shape] = (string("escala") ~> spacedCharParser('[') ~> double <~ commaParser <> double <~ spacedCharParser(']') <> spacedCharParser('(') ~> shapeParser <~ spacedCharParser(')')).map {
    case ((scaleX, scaleY), shape) => scale(scaleX, scaleY, shape)
  }

  def rotateParser: Parser[Shape] = (string("rotacion") ~> spacedCharParser('[') ~> double <~ spacedCharParser(']') <> spacedCharParser('(') ~> shapeParser <~ spacedCharParser(')')).map {
    case (angle, shape) => rotate(angle, shape)
  }

  def parser: Parser[Shape] = shapeParser

  def parserSimplified: String => Shape = input => simplify(shapeParser(input).get.consumed)

  def colorGroup(input: List[Shape]): Boolean = input match {
    case list@List(color(rA, gA, bA, _), color(rB, gB, bB, _), _*) if rA == rB && gA == gB && bA == bB => colorGroup(list.tail)
    case List(_) => true
    case _ => false
  }

  def rotateGroup(input: List[Shape]): Boolean = input match {
    case list@List(rotate(angleA, _), rotate(angleB, _), _*) if angleA == angleB => rotateGroup(list.tail)
    case List(_) => true
    case _ => false
  }

  def scaleGroup(input: List[Shape]): Boolean = input match {
    case list@List(scale(scaleXA, scaleYA, _), scale(scaleXB, scaleYB, _), _*) if scaleXA == scaleXB && scaleYA == scaleYB => scaleGroup(list.tail)
    case List(_) => true
    case _ => false
  }

  def translateGroup(input: List[Shape]): Boolean = input match {
    case list@List(translate(transXA, transYA, _), translate(transXB, transYB, _), _*) if transXA == transXB && transYA == transYB => translateGroup(list.tail)
    case List(_) => true
    case _ => false
  }

  def simplify(input: Shape): Shape = input match {
    case color(_, _, _, childColor: color) => simplify(childColor)

    case group(list@color(r, g, b, _) :: _) if colorGroup(list) => simplify(color(r, g, b, group(list.map { case color(_, _, _, child) => child })))
    case group(list@rotate(angle, _) :: _) if rotateGroup(list) => simplify(rotate(angle, group(list.map { case rotate(_, child) => child })))
    case group(list@scale(scaleX, scaleY, _) :: _) if scaleGroup(list) => simplify(scale(scaleX, scaleY, group(list.map { case scale(_, _, child) => child })))
    case group(list@translate(transX, transY, _) :: _) if translateGroup(list) => simplify(translate(transX, transY, group(list.map { case translate(_, _, child) => child })))
    case group(list) => group(list.map { simplify(_) })

    case rotate(gradesA, rotate(gradesB, child)) => simplify(rotate(gradesA + gradesB, child))
    case scale(scaleXA, scaleYA, scale(scaleXB, scaleYB, child)) => simplify(scale(scaleXA * scaleXB, scaleYA * scaleYB, child))
    case translate(transXA, transYA, translate(transXB, transYB, child)) => simplify(translate(transXA + transXB, transYA + transYB, child))

    case rotate(angle, child) if angle % 360 == 0 => simplify(child)
    case scale(1, 1, child) => simplify(child)
    case translate(0, 0, child) => simplify(child)

    case color(r, g, b, child) => color(r, g, b, simplify(child))
    case rotate(angle, child) => rotate(angle, simplify(child))
    case scale(scaleX, scaleY, child) => scale(scaleX, scaleY, simplify(child))
    case translate(transX, transY, child) => translate(transX, transY, simplify(child))

    case _ => input
  }
}
