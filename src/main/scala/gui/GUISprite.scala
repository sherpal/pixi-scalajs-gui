package gui

import gameengine.Engine
import pixigraphics.{PIXIGraphics, PIXITexture, Sprite}

/**
 * A GUISprite is a PIXI sprite attached to a Frame.
 */
class GUISprite(parent: Frame, texture: PIXITexture) extends LayeredRegion {

  def this(parent: Frame) = this(parent, Engine.graphics.webGLRenderer.generateTexture(
    new PIXIGraphics()
      .beginFill(0xFFFFFF)
      .drawRect(1, 1, 30, 30)
      .endFill()
  ))

  protected var _parent: Option[Frame] = Some(parent)

  protected val sprite: Sprite = new Sprite(texture)


  def setTexture(texture: PIXITexture): Unit =
    sprite.texture = texture


}
