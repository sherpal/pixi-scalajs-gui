package gui

import gameengine.Engine
import pixigraphics.PIXIContainer

/**
 * A FrameContainer is some wrapper for PIXIContainers attached to a particular Frame.
 *
 * It has a main PIXIContainer, linked to the Frame, that has two children. The Children below is for the layered
 * regions, and the one above for the Frame children.
 */
class FrameContainer(val frame: Frame) {

  val container: PIXIContainer = new PIXIContainer()
  Engine.graphics.mainStage.addChild(container)
  container.position.set(0,0)

  private val layeredRegionContainer: PIXIContainer = new PIXIContainer()
  container.addChild(layeredRegionContainer)
  layeredRegionContainer.position.set(0,0)

  private val frameChildrenContainer: PIXIContainer = new PIXIContainer()
  container.addChild(frameChildrenContainer)
  frameChildrenContainer.position.set(0,0)

  def addChild(child: Region): Unit = child match {
    case child: LayeredRegion =>
      child.addToParentContainer(layeredRegionContainer)
      sortLayeredRegionContainer()
    case child: Frame =>
      frameChildrenContainer.addChild(child.frameContainer.container)
      sortFrameChildrenContainer()
  }

  def removeChild(child: Region): Unit = child match {
    case child: LayeredRegion =>
      child.removeFromParentContainer(layeredRegionContainer)
    case child: Frame =>
      frameChildrenContainer.removeChild(child.frameContainer.container)
  }

  private def sortLayeredRegionContainer(): Unit = {
    // Maybe optimize this in the future, but will do for now.
    frame.layeredChildren.foreach(_.removeFromParentContainer(layeredRegionContainer))
    frame.layeredChildren.foreach(_.addToParentContainer(layeredRegionContainer))
  }

  private def sortFrameChildrenContainer(): Unit = {
    // Maybe optimize this in the future, but will do for now.
    val frameChildren = frame.getChildren
      .filter(_.isInstanceOf[Frame])
      .map(_.asInstanceOf[Frame])
      .toList
      .sortWith(Frame.compareDrawOrder)
    frameChildren.foreach(child => frameChildrenContainer.removeChild(child.frameContainer.container))
    frameChildren.foreach(child => frameChildrenContainer.addChild(child.frameContainer.container))
  }


}
