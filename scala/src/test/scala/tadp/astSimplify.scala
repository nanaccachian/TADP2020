import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tadp._
import tadp.Point.PointBuilder

class astSimplify extends AnyFlatSpec with should.Matchers {
  "Transformación de color aplicada a otra transformacion de color" should "dejar la de adentro" in {
    ast.simplify(color(1, 2, 3, color(4, 5, 6, circle(0 ! 0, 0)))) shouldEqual color(4, 5, 6, circle(0 ! 0, 0))
  }

  "Transformación triple de color aplicada a otra transformacion de color" should "dejar la de adentro" in {
    ast.simplify(color(1, 2, 3, color(4, 5, 6, color(7, 8, 9, circle(0 ! 0, 0))))) shouldEqual color(7, 8, 9, circle(0 ! 0, 0))
  }

  "Color aplicada a todos los hijos de un grupo" should "devolver una transformación aplicada al grupo" in {
    ast.simplify(group(List(
      color(200, 200, 200, rectangle(100 ! 100, 200 ! 200)),
      color(200, 200, 200, rectangle(150 ! 150, 260 ! 260)),
      color(200, 200, 200, circle(100 ! 300, 150))
    ))) shouldEqual color(200, 200, 200,
      group(List(
        rectangle(100 ! 100, 200 ! 200),
        rectangle(150 ! 150, 260 ! 260),
        circle(100 ! 300, 150)
      )))
  }

  "Rotaciones diferentes aplicada a todos los hijos de un grupo" should "no simplificar el grupo" in {
    ast.simplify(group(List(
      rotate(200, rectangle(100 ! 100, 200 ! 200)),
      rotate(201, rectangle(150 ! 150, 260 ! 260)),
      rotate(200, circle(100 ! 300, 150))
    ))) shouldEqual group(List(
      rotate(200, rectangle(100 ! 100, 200 ! 200)),
      rotate(201, rectangle(150 ! 150, 260 ! 260)),
      rotate(200, circle(100 ! 300, 150))
    ))
  }

  "Rotaciones consecutivas" should "devolver rotación sumada" in  {
    ast.simplify(
      rotate(300, rotate(10, circle(100 ! 300, 150)))
    ) shouldEqual rotate(310, circle(100 ! 300, 150))
  }

  "Escalas consecutivas" should "devolver escala multiplicada" in  {
    ast.simplify(
      scale(2, 3, scale(3, 5, circle(100 ! 300, 150)))
    ) shouldEqual scale(6, 15, circle(100 ! 300, 150))
  }

  "Translaciones consecutivas" should "devolver translación sumada" in  {
    ast.simplify(
      translate(100, 5, translate(20, 10, circle(100 ! 300, 150)))
    ) shouldEqual translate(120, 15, circle(100 ! 300, 150))
  }

  "Rotación de 0 grados" should "devolver la figura hija" in  {
    ast.simplify(
      rotate(0, circle(100 ! 300, 150))
    ) shouldEqual circle(100 ! 300, 150)
  }

  "Rotación de 720 grados" should "devolver la figura hija" in  {
    ast.simplify(
      rotate(720, circle(100 ! 300, 150))
    ) shouldEqual circle(100 ! 300, 150)
  }

  "Escala 1,1" should "devolver la figura hija" in  {
    ast.simplify(
      scale(1, 1, circle(100 ! 300, 150))
    ) shouldEqual circle(100 ! 300, 150)
  }

  "Translación 0,0" should "devolver la figura hija" in  {
    ast.simplify(
      translate(0, 0, circle(100 ! 300, 150))
    ) shouldEqual circle(100 ! 300, 150)
  }
}
