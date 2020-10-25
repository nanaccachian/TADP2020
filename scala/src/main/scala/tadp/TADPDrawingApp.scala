package tadp

import tadp.Point.PointBuilder
import tadp.internal.TADPDrawingAdapter
import scalafx.scene.paint.Color

object TADPDrawingApp extends App {

  TADPDrawingAdapter.forInteractiveScreen { (imageDescription, adapter) =>
    /*  imageDescription match {
        case "triangulo" => adapter.triangle((100, 100), (0, 150), (200, 300))
        case "rectangulo" => adapter.beginColor(Color.rgb(100, 100, 100))
          .rectangle((200, 200), (300, 400))
          .end()
      }*/

    grupo(
      color(0, 0, 255, 0.5)(circulo(400 ! 400, 50)),
      rotacion(45)(rectangulo(400 ! 300, 500 ! 400)),
      traslacion(100, 100)(triangulo(100 ! 100, 0 ! 150, 200 ! 300)),
      rotacion(350)(color(255, 0, 0, 0.5)(triangulo(100 ! 100, 0 ! 150, 200 ! 300)))
    ).draw(adapter)
  }
}
