package pixigraphics

import org.scalajs.dom.html

import scala.scalajs.js

/**
 * Common features of WebGLRenderer ans CanvasRenderer.
 */
@js.native
trait PIXIRenderer extends js.Object {

  val view: html.Canvas = js.native

  var autoResize: Boolean = js.native

  // example: renderer.backgroundColor = 0xFF0000
  var backgroundColor: Int = js.native

  def generateTexture(displayObject: DisplayObject): PIXITexture = js.native

  def render(stage: DisplayObject): Unit = js.native

  // example:
  //     renderer.autoResize = true
  //     renderer.resize(512, 512)
  def resize(width: Int, height: Int): Unit = js.native

}
