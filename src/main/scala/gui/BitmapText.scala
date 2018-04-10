package gui

import complex.Complex
import gameengine.Engine
import pixigraphics.{BitmapTextStyle, PIXIBitmapText}

import scala.scalajs.js


class BitmapText(
                  parent: Frame,
                  _font: String,
                  _text: String,
                  _align: String
                ) extends LayeredRegion {


  protected var _parent: Option[Frame] = None


  protected val sprite: PIXIBitmapText = new PIXIBitmapText(_text, new BitmapTextStyle {
    override val font: js.UndefOr[String] = _font

    override val align: js.UndefOr[String] = _align
  })


  private def setSizeProgrammatically(): Unit = {
    super.setSize(sprite.textWidth, sprite.textHeight)
  }

  def setText(text: String): Unit = {
    sprite.text = text

    setSizeProgrammatically()
  }

  override def changeDrawInfo(): Unit = {
    val (x, y) = Engine.graphics.changeCoordinates(Complex(left, top))
    sprite.position.set(x, y)
  }

  def text: String = sprite.text

  setSizeProgrammatically()

  override def setSize(w: Double, h: Double): Unit = throw new UnsupportedOperationException
  override def setSize(s: Double): Unit = setSize(s, s)
  override def setSize(): Unit = setSize(0)

  setParent(parent)
  setDrawLayer(Overlay)

}
