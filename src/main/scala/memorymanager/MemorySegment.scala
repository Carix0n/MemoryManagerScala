package memorymanager

import scala.math.{max, min}

object MemorySegment {
  def heapObserver(segment: MemorySegment, newIndex: Int): Unit = {
    segment.heapIndex = newIndex
  }

  implicit def memorySegmentSizeCompare: Ordering[MemorySegment] = Ordering.by(x => (x.size, -x.left))
}

class MemorySegment(var left: Int, var right: Int) {
  var heapIndex: Int = OrderedHeap.kNullIndex

  def size: Int = right - left

  def Unite(other: MemorySegment): MemorySegment = {
    if (max(left, other.left) != min(right, other.right))
      throw new IllegalArgumentException("Segments must be nearby each other to be united")

    new MemorySegment(min(left, other.left), max(right, other.right))
  }
}
