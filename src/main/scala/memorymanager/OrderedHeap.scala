package memorymanager

import scala.collection.mutable.ArrayBuffer

object OrderedHeap {
  val kNullIndex: Int = -1
}

class OrderedHeap[T](indexChangeObserver: (T, Int) => Unit = (_: T, _: Int) => null.asInstanceOf[Unit])
                    (implicit ordering: Ordering[T]) {
  private val _elements = ArrayBuffer.empty[T]

  private val kNullIndex = OrderedHeap.kNullIndex

  def Push(value: T): Int = {
    _elements += value
    val lastElementIndex = size - 1
    NotifyIndexChange(value, lastElementIndex)
    SiftUp(lastElementIndex)
  }

  def Erase(index: Int): Unit = {
    val lastElementIndex = size - 1

    if (index == lastElementIndex) {
      NotifyIndexChange(_elements.last, kNullIndex)
      _elements.remove(lastElementIndex)
    }
    else {
      SwapElements(index, lastElementIndex)
      NotifyIndexChange(_elements.last, kNullIndex)

      if (CompareElements(lastElementIndex, index)) {
        _elements.remove(lastElementIndex)
        SiftUp(index)
      }
      else {
        _elements.remove(lastElementIndex)
        SiftDown(index)
      }
    }
  }

  def top: T = _elements.head

  def Pop(): Unit = Erase(0)

  def size: Int = _elements.size

  def isEmpty: Boolean = _elements.isEmpty

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
    ordering.compare(_elements.apply(lhsIndex), _elements.apply(rhsIndex)) == -1

  def NotifyIndexChange(value: T, newElementIndex: Int): Unit = indexChangeObserver(value, newElementIndex)

  def SwapElements(lhsIndex: Int, rhsIndex: Int): Unit = {
    val (lhs, rhs) = (_elements.apply(lhsIndex), _elements.apply(rhsIndex))

    _elements.update(lhsIndex, rhs)
    _elements.update(rhsIndex, lhs)
    NotifyIndexChange(_elements.apply(lhsIndex), lhsIndex)
    NotifyIndexChange(_elements.apply(rhsIndex), rhsIndex)
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
