package gui

trait StackedFrame extends Frame {

  def isEmpty: Boolean = false

  def head: StackedFrame = this

  def last: StackedFrame =
    if (tail.isEmpty) this
    else tail.last

  def tail: StackedFrame = _next match {
    case Some(frame) =>
      frame
    case None =>
      EmptyStack
  }

  def apply(index: Int): StackedFrame = index match {
    case 0 =>
      head
    case n =>
      tail(n - 1)
  }

  def length: Int = {
    def lengthAcc(stackedFrame: StackedFrame, acc: Int): Int = {
      if (stackedFrame.isEmpty) acc else lengthAcc(stackedFrame.tail, acc + 1)
    }

    lengthAcc(this, 0)
  }

  def foreach(f: StackedFrame => Unit): Unit = {
    f(this)
    tail.foreach(f)
  }

  private var _previous: Option[StackedFrame] = None
  def previous: Option[StackedFrame] = _previous

  private var _next: Option[StackedFrame] = None
  def next: Option[StackedFrame] = _next

  def attachAfter(frame: StackedFrame): Unit = {
    // setting previous frame
    _previous = Some(frame)

    // attaching this to the previous frame
    clearAllPoints()
    setPoint(Top, frame, Bottom)

    // if frame had already a next frame, setting it the next of this
    frame._next match {
      case Some(f) =>
        // attaching it to this
        f.clearAllPoints()
        f.setPoint(Top, this, Bottom)
        // the previous of that frame is now this
        f._previous = Some(this)
        _next = Some(f)
      case None =>
    }

    frame._next = Some(this)
  }

  def addChild(child: StackedFrame, position: Int = length): StackedFrame = {
    if (position == 0) {
      this.attachAfter(child)
      child
    } else {
      child.attachAfter(apply(position - 1))
      this
    }
  }

  def removeChild(position: Int = length): StackedFrame = {
    val toRemove = apply(position)

    (toRemove.previous, toRemove.next) match {
      case (Some(prev), Some(ne)) =>
        ne.clearAllPoints()
        ne.setPoint(Top, prev, Bottom)

        ne._previous = Some(prev)
        prev._next = Some(ne)

        if (position == 0)
          prev
        else
          this
      case (Some(prev), None) =>
        prev._next = None
        if (position == 0)
          prev
        else
          this
      case (None, Some(ne)) =>
        ne._previous = None
        ne.clearAllPoints()
        ne
      case (None, None) =>
        EmptyStack
    }

  }

}


object EmptyStack extends StackedFrame {

  override def isEmpty: Boolean = true

  override def head: StackedFrame = throw new NoSuchElementException

  override def last: StackedFrame = throw new NoSuchElementException

  override def tail: StackedFrame = throw new UnsupportedOperationException

  override def apply(index: Int): StackedFrame = throw new IndexOutOfBoundsException

  override def addChild(child: StackedFrame, position: Int = length): StackedFrame = child

  override def attachAfter(frame: StackedFrame): Unit = throw new UnsupportedOperationException

  override def length: Int = 0

  override def foreach(f: StackedFrame => Unit): Unit = {}

}