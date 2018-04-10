/**
 * License
 * =======
 *
 * The MIT License (MIT)
 *
 *
 * Copyright (c) 2017 Antoine DOERAENE @sherpal
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package gui

import complex.Complex
import gameengine.Engine
import pixigraphics.{PIXIContainer, WithTint}
import webglgraphics.Vec3


trait LayeredRegion extends Region {

//  def draw(): Unit

  def addToParentContainer(container: PIXIContainer): Unit =
    container.addChild(sprite)

  def removeFromParentContainer(container: PIXIContainer): Unit =
    container.removeChild(sprite)

  /**
   * When the makeActualPoints method of this LayeredRegion is called, if the area can now be drawn, it adapts all info
   * of the Sprite.
   */
  def changeDrawInfo(): Unit = {
    val (x, y) = Engine.graphics.changeCoordinates(Complex(left, top))
    sprite.position.set(x, y)
    sprite.width = width.toInt
    sprite.height = height.toInt
    // TODO: add other stuff here
  }


  protected val sprite: PIXIContainer with WithTint

  override def hide(): Unit = {
    sprite.visible = false
    super.hide()
  }

  override def show(): Unit = {
    sprite.visible = true
    super.show()
  }



  private var _drawLayer: Layer = Artwork
  private var _drawSubLayer: Int = 0

  /** Returns the draw layer of the Region. */
  def drawLayer: Layer = _drawLayer

  /** Returns the draw sub layer of the Region. */
  def drawSubLayer: Int = _drawSubLayer

  /** Sets the draw layer and sublayer of the Region. */
  def setDrawLayer(layer: Layer, subLayer: Int): Unit = {
    _drawLayer = layer
    _drawSubLayer = subLayer

    parent match {
      case Some(p) =>
        p.orderLayeredRegions()
        p.frameContainer.addChild(this)
      case _ =>
        if (scala.scalajs.LinkingInfo.developmentMode) {
          println("Something weird, LayeredRegion does not have parent...")
        }
    }
  }

  /** Sets the layer of the region, and the sublayer to 9 if Region is a FontString, 0 otherwise. */
  def setDrawLayer(layer: Layer): Unit = setDrawLayer(layer, this match {
    case _: FontString => 9
    case _ => 0
  })

  protected var _red: Double = 1.0
  protected var _green: Double = 1.0
  protected var _blue: Double = 1.0

  /** Sets the colours and the alpha of the Region. */
  def setVertexColor(red: Double = 1.0, green: Double = 1.0, blue: Double = 1.0, alpha: Double = 1.0): Unit = {
    _red = if (red < 0) 0.0 else if (red > 1) 1.0 else red
    _green = if (green < 0) 0.0 else if (green > 1) 1.0 else green
    _blue = if (blue < 0) 0.0 else if (blue > 1) 1.0 else blue
    setAlpha(alpha)

    sprite.tint = Vec3(_red, _green, _blue).toInt
    sprite.alpha = alpha
  }
}


