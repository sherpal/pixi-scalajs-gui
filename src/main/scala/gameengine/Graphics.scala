package gameengine

import complex.Complex
import gui.UIParent
import org.scalajs.dom.html
import pixigraphics.{PIXIContainer, RendererOptions, WebGLRenderer}
import webglgraphics.Vec3

import scala.scalajs.js
import scala.scalajs.js.UndefOr

/**
 * A Graphics class contains most of the PIXI related low level operations, as well as canvas handling.
 */
class Graphics(val canvas: html.Canvas) {


  val webGLRenderer: WebGLRenderer = new WebGLRenderer(canvas.width, canvas.height, new RendererOptions {

    override val view: js.UndefOr[html.Canvas] = canvas

    override val antialias: UndefOr[Boolean] = true

  })

  def resize(width: Int, height: Int): Unit = {
    canvas.width = width
    canvas.height = height

    webGLRenderer.autoResize = true

    webGLRenderer.resize(width, height)

    UIParent.resize(width, height)
  }

  val mainStage: PIXIContainer = new PIXIContainer()
  mainStage.interactiveChildren = false

  /**
   * Contains all the graphics related to the game.
   */
  val graphicsStage: PIXIContainer = new PIXIContainer()
  mainStage.addChild(graphicsStage)

  /**
   * Contains all the GUI related sprites.
   *
   * The GUI is always above the game graphics, hence adding after.
   */
  val guiStage: PIXIContainer = new PIXIContainer()
  mainStage.addChild(guiStage)

  def render(): Unit = webGLRenderer.render(mainStage)

  def changeCoordinates(z: Complex): (Double, Double) = (z.re + canvas.width / 2, canvas.height / 2 - z.im)

  def setBackgroundColor(red: Double, green: Double, blue: Double): Unit = {
    webGLRenderer.backgroundColor = Vec3(red, green, blue).toInt
  }

  def dimensions: (Int, Int) = (canvas.width, canvas.height)

}
