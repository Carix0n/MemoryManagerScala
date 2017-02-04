package memorymanager

object OrderedHeap {
  val kNullIndex: Int = -1
}

class OrderedHeap[T](indexChangeObserver: (T, Int) => Unit = (_: T, _: Int) => null.asInstanceOf[Unit])
                    (implicit ordering: Ordering[T]) {
  private var elements = scala.collection.immutable.Vector.empty[T]

  private val kNullIndex = OrderedHeap.kNullIndex

  def Push(value: T): Int = {
    elements :+= value
    val lastElementIndex = size - 1
    NotifyIndexChange(value, lastElementIndex)
    SiftUp(lastElementIndex)
  }

  def Erase(index: Int): Unit = {
    if (index + 1 == size) {
      NotifyIndexChange(elements.last, kNullIndex)
      elements = elements.dropRight(1)
    }
    else {
      val lastElementIndex = size - 1
      SwapElements(index, lastElementIndex)
      NotifyIndexChange(elements.last, kNullIndex)

      if (CompareElements(lastElementIndex, index)) {
        elements = elements.dropRight(1)
        SiftUp(index)
      }
      else {
        elements = elements.dropRight(1)
        SiftDown(index)
      }
    }
  }

  def top: T = elements.head

  def Pop(): Unit = Erase(0)

  def size: Int = elements.size

  def isEmpty: Boolean = elements.isEmpty

  private def parent(index: Int): Int = {
    val potentialParentIndex = (index - 1) / 2

    if (potentialParentIndex >= 0) potentialParentIndex
    else kNullIndex
  }

  private def leftSon(index: Int): Int = {
    val potentialLeftSonIndex = 2 * index + 1

    if (potentialLeftSonIndex < size) potentialLeftSonIndex
    else kNullIndex
  }

  private def rightSon(index: Int): Int = {
    val potentialRightSonIndex = 2 * index + 2

    if (potentialRightSonIndex < size) potentialRightSonIndex
    else kNullIndex
  }

  def CompareElements(lhsIndex: Int, rhsIndex: Int): Boolean =
    ordering.compare(elements.apply(lhsIndex), elements.apply(rhsIndex)) == -1

  def NotifyIndexChange(value: T, newElementIndex: Int): Unit = indexChangeObserver(value, newElementIndex)

  def SwapElements(lhsIndex: Int, rhsIndex: Int): Unit = {
    val (lhs, rhs) = (elements.apply(lhsIndex), elements.apply(rhsIndex))

    elements = elements.updated(lhsIndex, rhs)
    elements = elements.updated(rhsIndex, lhs)
    NotifyIndexChange(elements.apply(lhsIndex), lhsIndex)
    NotifyIndexChange(elements.apply(rhsIndex), rhsIndex)
  }

  def SiftUp(index: Int): Int = {
    if (index > 0 && CompareElements(parent(index), index)) {
      SwapElements(parent(index), index)
      SiftUp(parent(index))
    }
    else index
  }

  def SiftDown(index: Int): Unit = {
    var swappingElementIndex = index
    val leftSonIndex = leftSon(index)
    val rightSonIndex = rightSon(index)

    if (leftSonIndex < size && leftSonIndex != kNullIndex && CompareElements(swappingElementIndex, leftSonIndex))
      swappingElementIndex = leftSonIndex

    if (rightSonIndex < size && rightSonIndex != kNullIndex && CompareElements(swappingElementIndex, rightSonIndex))
      swappingElementIndex = rightSonIndex

    if (index != swappingElementIndex) {
      SwapElements(index, swappingElementIndex)
      SiftDown(swappingElementIndex)
    }
  }
}
