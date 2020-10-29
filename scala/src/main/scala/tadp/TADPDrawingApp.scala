package tadp
import tadp.internal.TADPDrawingAdapter

object TADPDrawingApp extends App {

  TADPDrawingAdapter.forInteractiveScreen { (imageDescription, adapter) =>
    ast.parserSimplified(imageDescription).draw(adapter)
  }
}
