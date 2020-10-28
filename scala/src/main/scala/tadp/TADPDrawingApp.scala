package tadp
import tadp.internal.TADPDrawingAdapter

object TADPDrawingApp extends App {

  TADPDrawingAdapter.forInteractiveScreen { (imageDescription, adapter) =>
    ast.parser(imageDescription).get.consumed.draw(adapter)
  }
}
