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

import scala.collection.mutable

/**
 * A ScriptObject is any UIObject that can register scripts in order to fire them when some event occurs.
 */
trait ScriptObject extends UIObject {

  // type is Any but is actually a Handler, will never be something else
  protected val handlers: mutable.Map[ScriptKind, Any] = mutable.Map()

  /**
   * Sets the handler function of a [[ScriptKind]].
   * @param kind    kind of script to register.
   * @param handler function to be called when script is fired.
   */
  def setScript(kind: ScriptKind)(handler: kind.Handler): Unit = {
    handlers += (kind -> handler)
    ScriptObject.objectsWithScriptByKind(kind) += this
  }

  /** Returns the handler function associated to the specified [[ScriptKind]], or None if it is not set. */
  def getScript(kind: ScriptKind): Option[kind.Handler] = {
    if (hasScript(kind)) Some(handlers(kind).asInstanceOf[kind.Handler])
    else None
  }

  /** Returns whether the script was set. */
  def hasScript(kind: ScriptKind): Boolean = handlers.isDefinedAt(kind)

  /** Removes all scripts set. */
  def removeAllScripts(): Unit = handlers.toList.foreach(elem => removeScript(elem._1))

  /** Removes the specified script. */
  def removeScript(kind: ScriptKind): Unit = if (hasScript(kind)) {
    handlers -= kind
    ScriptObject.objectsWithScriptByKind(kind) -= this
  }


  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts without arguments.
   *
   * @param kind              script kind to fire.
   * @tparam R                return type of the handler, most often Unit.
   */
  def fires[R](kind: ScriptKind { type Handler = () => R })(): Unit =
    getScript(kind) match {
      case Some(handler) => handler()
      case _ =>
    }

  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts with one argument.
   *
   * @param kind              script kind to fire.
   * @tparam T1               type of argument that kind of script takes.
   * @tparam R                return type of the handler, most often Unit.
   */
  def fires[T1, R](kind: ScriptKind { type Handler = (T1) => R })
                  (arg: T1): Unit =
    getScript(kind) match {
      case Some(handler) => handler(arg)
      case _ =>
    }

  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts with two arguments.
   *
   * @param kind              script kind to fire.
   * @tparam T1               first type of argument that kind of script takes.
   * @tparam T2               second type of argument that kind of script takes.
   * @tparam R                return type of the handler, most often Unit.
   */
  def fires[T1, T2, R](kind: ScriptKind { type Handler = (T1, T2) => R })
                      (arg1: T1, arg2: T2): Unit = getScript(kind) match {
    case Some(handler) => handler(arg1, arg2)
    case _ =>
  }

  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts with three arguments.
   *
   * @param kind              script kind to fire.
   * @tparam T1               first type of argument that kind of script takes.
   * @tparam T2               second type of argument that kind of script takes.
   * @tparam T3               third type of argument that kind of script takes.
   * @tparam R                return type of the handler, most often Unit.
   */
  def fires[T1, T2, T3, R](kind: ScriptKind { type Handler = (T1, T2, T3) => R })(
    arg1: T1, arg2: T2, arg3: T3): Unit = getScript(kind) match {
    case Some(handler) => handler(arg1, arg2, arg3)
    case _ =>
  }

  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts with four arguments.
   *
   */
  def fires[T1, T2, T3, T4, R](kind: ScriptKind {
    type Handler = (T1, T2, T3, T4) => R
  })(arg1: T1, arg2: T2, arg3: T3, arg4: T4): Unit = getScript(kind) match {
    case Some(handler) => handler(arg1, arg2, arg3, arg4)
    case _ =>
  }

  /**
   * Call the handler function of the ScriptObject associated with the specified [[ScriptKind]], if any.
   * This overloaded method applies for scripts with five arguments.
   *
   */
  def fires[T1, T2, T3, T4, T5, R](kind: ScriptKind {
    type Handler = (T1, T2, T3, T4, T5) => R
  })(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): Unit = getScript(kind) match {
    case Some(handler) => handler(arg1, arg2, arg3, arg4, arg5)
    case _ =>
  }

}


object ScriptObject {
  // TODO: Map must be filled in with empty mutable.Sets for each kind of script
  private val objectsWithScriptByKind: Map[ScriptKind, mutable.Set[ScriptObject]] = Map(
    ScriptKind.OnAttributeChanged -> mutable.Set(),
    ScriptKind.OnClick -> mutable.Set(), ScriptKind.OnEnter -> mutable.Set(), ScriptKind.OnEvent -> mutable.Set(),
    ScriptKind.OnHide -> mutable.Set(), ScriptKind.OnHorizontalScroll -> mutable.Set(),
    ScriptKind.OnKeyPressed -> mutable.Set(), ScriptKind.OnKeyReleased -> mutable.Set(),
    ScriptKind.OnLeave -> mutable.Set(), ScriptKind.OnLoseFocus -> mutable.Set(),
    ScriptKind.OnMinMaxValuesChanged -> mutable.Set(), ScriptKind.OnMouseMoved -> mutable.Set(),
    ScriptKind.OnMouseReleased -> mutable.Set(), ScriptKind.OnScrollRangeChanged -> mutable.Set(),
    ScriptKind.OnShow -> mutable.Set(), ScriptKind.OnUIParentResize -> mutable.Set(),
    ScriptKind.OnUpdate -> mutable.Set(),
    ScriptKind.OnValueChanged -> mutable.Set(), ScriptKind.OnVerticalScroll -> mutable.Set(),
    ScriptKind.OnWheelMoved -> mutable.Set(), ScriptKind.OnWinFocus -> mutable.Set()
  )

  def firesScript[R](kind: ScriptKind { type Handler = () => R })
                                      (): Unit =
    objectsWithScriptByKind(kind).foreach(_.fires[R](kind)())

  /** Fires the script of every ScriptObject that registered it. */
  def firesScript[T, R](kind: ScriptKind { type Handler = (T) => R })
                                         (arg: T): Unit =
    objectsWithScriptByKind(kind).foreach(_.fires[T, R](kind)(arg))

  /** Fires the script of every ScriptObject that registered it. */
  def firesScript[T1, T2, R](kind: ScriptKind { type Handler = (T1, T2) => R })
                                              (arg1: T1, arg2: T2): Unit =
    objectsWithScriptByKind(kind).foreach(_.fires[T1, T2, R](kind)(arg1, arg2))

  /** Fires the script of every ScriptObject that registered it. */
  def firesScript[T1, T2, T3, R](kind: ScriptKind { type Handler = (T1, T2, T3) => R })
                                (arg1: T1, arg2: T2, arg3: T3): Unit =
    objectsWithScriptByKind(kind).foreach(_.fires[T1, T2, T3, R](kind)(arg1, arg2, arg3))

  val framesWithEventByKind: mutable.Map[ScriptKind, mutable.Set[Frame]] = mutable.Map()
  val hiddenFramesWithEventByKind: mutable.Map[ScriptKind, mutable.Set[Frame]] = mutable.Map()
  def possibleEvents: Iterable[ScriptKind] = framesWithEventByKind.keys


  /** Fires the Event of every [[Frame]] that registered it. */
  def firesEvent[R](kind: ScriptKind {type Handler = () => R}): Unit =
    framesWithEventByKind(kind).foreach(_.fires[R](kind)())

  /** Fires the Event of every [[Frame]] that registered it. */
  def firesEvent[T, R](kind: ScriptKind {type Handler = (T) => R})(arg: T): Unit =
    framesWithEventByKind(kind).foreach(_.fires[T, R](kind)(arg))

  /** Fires the Event of every [[Frame]] that registered it. */
  def firesEvent[T1, T2, R](kind: ScriptKind {type Handler = (T1, T2) => R})(arg1: T1, arg2: T2): Unit =
    framesWithEventByKind(kind).foreach(_.fires[T1, T2, R](kind)(arg1, arg2))

  /** Fires the Event of every [[Frame]] that registered it. */
  def firesEvent[T1, T2, T3, R](kind: ScriptKind {type Handler = (T1, T2, T3) => R})
                               (arg1: T1, arg2: T2, arg3: T3): Unit =
    framesWithEventByKind(kind).foreach(_.fires[T1, T2, T3, R](kind)(arg1, arg2, arg3))

  /** Fires the Event of every [[Frame]] that registered it. */
  def firesEvent[T1, T2, T3, T4, R](kind: ScriptKind {type Handler = (T1, T2, T3, T4) => R})
                                   (arg1: T1, arg2: T2, arg3: T3, arg4: T4): Unit =
    framesWithEventByKind(kind).foreach(_.fires[T1, T2, T3, T4, R](kind)(arg1, arg2, arg3, arg4))
}


/**
 * A ScriptKind is a type of script or event that can be triggered.
 * Each ScriptKind is associated with a Handler, that is a function with the specified types of arguments.
 */
sealed trait ScriptKind { type Handler }
object ScriptKind {

  /**
   * Creates a new ScriptKind with the associated Handler type F, and returns it.
   * F has to be a function.
   */
  def makeEvent[F]: ScriptKind {type Handler = F} = {
    val script = new ScriptKind {type Handler = F}
    ScriptObject.framesWithEventByKind += (script -> mutable.Set())
    ScriptObject.hiddenFramesWithEventByKind += (script -> mutable.Set())
    script
  }

  /**
   * - OnAttributeChanged
   * arguments:
   * trigger: when the makeActualPoints method had to be called on the Region.
   */
  val OnAttributeChanged: ScriptKind {type Handler = () => Unit} = new ScriptKind {
    type Handler = () => Unit
  }

  /**
   * - OnClick:
   * arguments: mouse_x, mouse_y, button
   * trigger: mouse is pressed. Corresponds to love mousepressed event.
   * remark: as most mouse scripts, it fires only if the frame is the top most frame
   * under the mouse (should it change?)
   */
  val OnClick: ScriptKind {type Handler = (Double, Double, Int) => Unit} = new ScriptKind {
      type Handler = (Double, Double, Int) => Unit
    }

  /**
   * - OnEnter:
   * arguments: entered frame, previous left frame
   * trigger: if the mouse enters the frame
   */
  val OnEnter: ScriptKind {type Handler = (Frame, Frame) => Unit} = new ScriptKind {
    type Handler = (Frame, Frame) => Unit
  }

  /**
   * - OnEvent:
   * arguments: list of arguments of that event
   * trigger: manually, when user wants the event to trigger
   * remark: this script can be used to trigger the OnEvent on every frame that has set the OnEvent scripts, most
   * probably for testing purposes. Drawback of this script is that it has no type checking. If you want typechecking,
   * you should use firesEvent instead.
   */
  val OnEvent: ScriptKind {type Handler = (ScriptKind, List[Any]) => Unit} = new ScriptKind {
    type Handler = (ScriptKind, List[Any]) => Unit
  }

  /**
   * - OnHide:
   * arguments:
   * trigger: when a frame becomes hidden
   */
  val OnHide: ScriptKind {type Handler = () => Unit} = new ScriptKind {
    type Handler = () => Unit
  }

  /**
   * - OnHorizontalScroll:
   * arguments: new value, previous value
   * trigger : when a ScrollFrame sets horizontal scroll
   */

  val OnHorizontalScroll: ScriptKind {type Handler = (Double, Double) => Unit} = new ScriptKind {
    type Handler = (Double, Double) => Unit
  }

  /**
   * - OnKeyPressed:
   * arguments: key pressed, key code of key pressed, whether it is a repeat
   * trigger: keyboard is pressed. Corresponds to love keypressed event
   */

  val OnKeyPressed: ScriptKind {type Handler = (String, Int, Boolean) => Unit} = new ScriptKind {
    type Handler = (String, Int, Boolean) => Unit
  }

  /**
   * - OnKeyReleased
   * arguments: key released, key code.
   * trigger: keyboard is released. Corresponds to love keyreleased event
   */

  val OnKeyReleased: ScriptKind {type Handler = (String, Int) => Unit} = new ScriptKind {
    type Handler = (String, Int) => Unit
  }

  /**
   * - onLeave:
   * arguments: left frame, new entered frame
   * trigger: if the mouse leaves the frame
   */
  val OnLeave: ScriptKind {type Handler = (Frame, Frame) => Unit} = new ScriptKind {
    type Handler = (Frame, Frame) => Unit
  }

  /**
   * - OnLoseFocus:
   * arguments: new focused frame (may be nil)
   * trigger: when focus is lost
   * remark: Only concerns Focusable frames
   */
  val OnLoseFocus: ScriptKind {type Handler = (Option[Focusable]) => Unit} = new ScriptKind {
    type Handler = (Option[Focusable]) => Unit
  }

  /**
   * - OnMinMaxValuesChanged
   * arguments: minimum of range, maximum of range.
   * trigger: when setMinMaxValues method is called.
   */
  val OnMinMaxValuesChanged: ScriptKind {type Handler = (Double, Double) => Unit} = new ScriptKind {
    type Handler = (Double, Double) => Unit
  }

  /**
   * - OnMouseMoved:
   * arguments: x mouse position, y mouse position, dx mouse difference, dy mouse difference, button number.
   * trigger: any time the onmousemove handler of the canvas is triggered.
   */
  val OnMouseMoved: ScriptKind {type Handler = (Double, Double, Double, Double, Int) => Unit} = new ScriptKind {
    type Handler = (Double, Double, Double, Double, Int) => Unit
  }

  /**
   * - OnMouseReleased:
   * arguments: mouse_x, mouse_y, button
   * trigger: mouse is released. Corresponds to love mousereleased event. This event
   *   triggers for both the frame under the mouse and the frame that was clicked
   *   before. Triggers only once if both these frames are the same.
   */
  val OnMouseReleased: ScriptKind {type Handler = (Double, Double, Int) => Unit} = new ScriptKind {
    type Handler = (Double, Double, Int) => Unit
  }

  /**
   * - OnScrollRangeChanged
   * arguments: horizontal scroll range, vertical scroll range
   * trigger: when the horizontal scroll range or the vertical scroll range changes
   * remark: this only concerns ScrollFrame frames
   */
  val OnScrollRangeChanged: ScriptKind {type Handler = (Double, Double) => Unit} = new ScriptKind {
    type Handler = (Double, Double) => Unit
  }

  /**
   * - OnShow:
   * arguments:
   * trigger: when a frame becomes visible
   * remark: this may happen if the parent frame becomes visible
   */
  val OnShow: ScriptKind {type Handler = () => Unit} = new ScriptKind {
    type Handler = () => Unit
  }

  /**
   * - OnUIParentResize
   * arguments:
   * trigger: when the UIParent is resized.
   */
  val OnUIParentResize: ScriptKind {type Handler = () => Unit} = new ScriptKind {
    type Handler = () => Unit
  }

  /**
   * - OnUpdate:
   * arguments: dt
   * trigger: every time the love.update() function is called
   */
  val OnUpdate: ScriptKind {type Handler = (Double) => Unit} = new ScriptKind {
    type Handler = (Double) => Unit
  }

  /**
   * - OnValueChanged:
   * arguments: new value, previous value
   * trigger: when value of StatusBar or Slider has been changed
   * remark: this triggers regardless whether new and previous values are different.
   * It would be more accurate to say that it triggers when the setValue method of a
   * ValueBar is called.
   */
  val OnValueChanged: ScriptKind {type Handler = (Double, Double) => Unit} = new ScriptKind {
    type Handler = (Double, Double) => Unit
  }

  /**
   * - OnVerticalScroll:
   * arguments: new value, previous value
   * trigger : when a ScrollFrame sets vertical scroll
   */
  val OnVerticalScroll: ScriptKind {type Handler = (Double, Double) => Unit} = new ScriptKind {
    type Handler = (Double, Double) => Unit
  }

  /**
   * - OnWheelMoved:
   * arguments: dx, dy
   * trigger: when mouse wheel has moved, regardless whether the frame is under the mouse or not
   * remark: dy is positive to the bottom, negative to the top. dx and dy go by +- 100 for each activation.
   */
  val OnWheelMoved: ScriptKind {type Handler = (Int, Int) => Unit} = new ScriptKind {
    type Handler = (Int, Int) => Unit
  }

  /**
   * - OnWinFocus:
   * arguments: previous focused frame (may be nil)
   * trigger: when keyboard focus is received
   */
  val OnWinFocus: ScriptKind {type Handler = (Option[Focusable]) => Unit} = new ScriptKind {
    type Handler = (Option[Focusable]) => Unit
  }
}
