package memorymanager

import scala.math.{max, min}

object MemorySegment {
  def heapObserver(segment: MemorySegment, newIndex: Int): Unit = {
    segment.heapIndex = newIndex
  }

  implicit def memorySegmentSizeCompare: Ordering[MemorySegment] = Ordering.by(x => (-x.size, x.left))
}

class MemorySegment(var left: Int, var right: Int) {
  var heapIndex: Int = OrderedHeap.kNullIndex

  def size: Int = right - left

  def unite(other: MemorySegment): Unit = {
    if (max(left, other.left) != min(right, other.right))
      throw new IllegalArgumentException("Segments must be nearby each other to be united")

    _update(left = min(left, other.left), right = max(right, other.right))
  }

  def splitAt(index: Int): Option[MemorySegment] = {
    if (index < size) {
      val restSize = size - index
      _update(right = left + index)
      Option(new MemorySegment(right, right + restSize))
    }
    else Option.empty[MemorySegment]
  }

  private def _update(left : Int = this.left, right: Int = this.right): Unit = {
    this.left = left
    this.right = right
  }
}
