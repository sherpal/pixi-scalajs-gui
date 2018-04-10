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

import pixigraphics.PIXITexture


/**
 * A StatusBar is a particular type of [[Frame]] used to display information on screen.
 * It has methods to set its value, and automatically adapt its length according to that value.
 */
class StatusBar(par: Option[Frame] = Some(UIParent)) extends Frame(par) with ValueBar {

  def this(parent: Frame) = this(Some(parent))

  def this() = this(Some(UIParent))

  private var _reverseFill: Boolean = false

  /**
   * Returns reverseFill mode.
   * If true, the Bar is filled from right to left and top to bottom.
   * If false, it's the other way around.
   */
  def reverseFill: Boolean = _reverseFill

  /** Sets the reverseFill mode. */
  def setReverseFill(enable: Boolean = false): Unit = {
    _reverseFill = enable

    updateTexture()
  }


  private var _redBar: Double = 1.0
  private var _greenBar: Double = 1.0
  private var _blueBar: Double = 1.0
  private var _alphaBar: Double = 1.0

  /** Returns the colours and alpha for the Bar. */
  def statusBarColor: (Double, Double, Double, Double) = (_redBar, _greenBar, _blueBar, _alphaBar)

  /** Sets the colours and alpha for the Bar. */
  def setStatusBarColor(r: Double, g: Double, b: Double, alpha: Double = 1.0): Unit = {
    _redBar = r
    _greenBar = g
    _blueBar = b
    _alphaBar = alpha

    updateTexture()
  }

  private var _useQuad: Boolean = false

  /** Returns whether Bar uses Quad to draw its texture. */
  def useQuad: Boolean = _useQuad

  /**
   * Sets whether Bar should use a Quad to draw it texture.
   * This can be use to make nice end of bar effects.
   **/
  def setUseQuad(enable: Boolean = false): Unit = {
    _useQuad = enable

    updateTexture()
  }

  private var _statusBarGUISprite: Option[GUISprite] = None

  def statusBarGUISprite: Option[GUISprite] = _statusBarGUISprite

  def setStatusBarTexture(texture: PIXITexture, layer: Layer): Unit = {

    _statusBarGUISprite match {
      case Some(sprite) =>
        sprite.setTexture(texture)
        sprite.setDrawLayer(layer)
      case None =>
        _statusBarGUISprite = Some(createSprite(layer = layer, texture = Some(texture)))
    }

    updateTexture()
  }

  def setStatusBarTexture(texture: PIXITexture): Unit =
    setStatusBarTexture(texture, layer = Highlight)

  def setStatusBarTexture(file: String, layer: Layer): Unit =
    setStatusBarTexture(PIXITexture.fromImage(file), layer)

  def setStatusBarTexture(file: String): Unit =
    setStatusBarTexture(file, layer = Highlight)

  def setStatusBarTexture(): Unit =
    _statusBarGUISprite = Some(createSprite(layer = Highlight))

  /**
   * Sets the value of the status bar.
   * The value is clamped between minimum and maximum value.
   * Fires the OnValueChanged script, regardless the value actually changed.
   */
  def setValue(v: Double): Unit = {
    val previousValue = value

    val (minValue, maxValue) = minMaxValues

    _value = if (v < minValue) minValue else if (v > maxValue) maxValue else v

    updateValue()

    fires[Double, Double, Unit](ScriptKind.OnValueChanged)(_value, previousValue)
  }

  setScript(ScriptKind.OnAttributeChanged)(() => updateTexture())

  /// Helper function

  /**
   * Called each time a property of a StatusBar is changed,
   * except if that property is value, in which case updateValue is called.
   * The reason is that updating a value is usually done lots of times at run
   * time, while updating other properties are usually done only when creating the StatusBar.
   */
  protected def updateTexture(): Unit = if (_statusBarGUISprite.isDefined) {
    val sprite = _statusBarGUISprite.get

    sprite.setVertexColor(_redBar, _greenBar, _blueBar, _alphaBar)
    sprite.clearAllPoints()

    val (minValue, maxValue) = minMaxValues

    orientation match {
      case HorizontalBar =>
        if (reverseFill) sprite.setPoint(AnchorPoint(Right, this, Right))
        else sprite.setPoint(AnchorPoint(Left, this, Left))
        sprite.setWidth(width * (value - minValue) / (maxValue - minValue))
        sprite.setHeight(height)
      case VerticalBar =>
        if (reverseFill) sprite.setPoint(AnchorPoint(Top, this, Top))
        else sprite.setPoint(AnchorPoint(Bottom, this, Bottom))
        sprite.setHeight(height * (value - minValue) / (maxValue - minValue))
        sprite.setWidth(width)
    }

    if (value <= minValue)
      sprite.hide()
    else
      sprite.show()

  }


  protected def updateValue(): Double = if (_statusBarGUISprite.isDefined) {
    val sprite = _statusBarGUISprite.get

    val (minValue, maxValue) = minMaxValues
    val coefficient = math.min((value - minValue) / (maxValue - minValue), 1.0)

        if (value <= minValue)
          sprite.hide()
        else {
          sprite.show()

          orientation match {
            case HorizontalBar =>
               sprite.setWidth(width * coefficient)
            case VerticalBar =>
              sprite.setHeight(height * coefficient)
          }
        }

    value
  } else 0

  //    if (_statusBarTexture.isDefined) {
  //    val tex = _statusBarTexture.get
  //
  //    val (minValue, maxValue) = minMaxValues
  //    val coefficient = math.min((value - minValue) / (maxValue - minValue), 1.0)
  //
  //    if (value <= minValue) tex.hide() else {
  //      tex.show()
  //
  //      (orientation, _useQuad) match {
  //        case (HorizontalBar, false) =>
  //          tex.setTexCoord()
  //          tex.setWidth(width * coefficient)
  //        case (VerticalBar, false) =>
  //          tex.setTexCoord()
  //          tex.setHeight(height * coefficient)
  //        case (HorizontalBar, true) =>
  //          tex.setTexCoord(0, 0, width * coefficient, height, width, height)
  //          tex.setWidth(width * coefficient)
  //        case (VerticalBar, true) =>
  //          tex.setTexCoord(0, 0, width, height * coefficient, width, height)
  //          tex.setHeight(height * coefficient)
  //      }
  //    }
  //    _value
  //  } else 0
}




