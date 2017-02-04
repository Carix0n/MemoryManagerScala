package memorymanager

import scala.math.{max, min}
import scala.math.Ordered.orderingToOrdered

object MemorySegment {
  def heapObserver(segment: MemorySegment, newIndex: Int): Unit = {
    segment.heapIndex = newIndex
  }

  implicit def ordering: Ordering[MemorySegment] =
    (lhs: MemorySegment, rhs: MemorySegment) => (lhs.size, -lhs.left) compare (rhs.size, -rhs.left)
}

class MemorySegment(val left: Int, val right: Int) {
  var heapIndex: Int = OrderedHeap.kNullIndex

  def size: Int = right - left

  def Unite(other: MemorySegment): MemorySegment = {
    if (max(left, other.left) != min(right, other.right))
      throw new IllegalArgumentException("Segments must be nearby each other to be united")

    new MemorySegment(min(left, other.left), max(right, other.right))
  }
}
