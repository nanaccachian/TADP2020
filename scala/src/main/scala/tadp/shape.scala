package tadp
import scalafx.scene.paint.Color

import tadp.internal.TADPDrawingAdapter

trait Shape {
  def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter
}

case class rectangulo(pointA: Point, pointB: Point) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.rectangle(pointA.toTuple(), pointB.toTuple())
}

case class triangulo(pointA: Point, pointB: Point, pointC: Point) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.triangle(pointA.toTuple(), pointB.toTuple(), pointC.toTuple())
}

case class circulo(center: Point, radius: Double) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.circle(center.toTuple(), radius)
}

case class grupo(shapes: Shape*) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shapes.foldLeft(adapter) ((adapter, shape) => shape.draw(adapter))
}

case class color(red: Int, green: Int, blue: Int, alpha: Double = 1)(shape: Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginColor(Color.rgb(red, green, blue, alpha))).end()
}

case class escala(scaleX: Int, scaleY: Int)(shape:Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginScale(scaleX, scaleY)).end()
}

case class rotacion(grades: Int)(shape: Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginRotate(grades)).end()
}

case class traslacion(trasX: Int, trasY:Int)(shape: Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginTranslate(trasX, trasY)).end()
}
