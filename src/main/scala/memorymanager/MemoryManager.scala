package memorymanager

import scala.collection.mutable.ListBuffer

object MemoryManager {
  val undefinedHandle: MemorySegment = null.asInstanceOf[MemorySegment]

  def run(memorySize: Int, queries: Seq[MemoryManagerQuery]): Seq[MemoryManagerAllocationResponse] = {
    val memoryManager = new MemoryManager(memorySize)
    val handles = ListBuffer.empty[MemorySegment]
    val responses = queries.map {
      case AllocationQuery(allocationSize) =>
        val handle = memoryManager.allocate(allocationSize)
        val response =
          if (handle != undefinedHandle) makeSuccessfulAllocation(handle.left)
          else makeFailedAllocation()

        handles.append(handle)
        response
      case FreeQuery(allocationQueryIndex) =>
        memoryManager.free(handles.apply(allocationQueryIndex - 1))
        handles.append(undefinedHandle)
        null.asInstanceOf[MemoryManagerAllocationResponse]
    }

    responses.filter(_ != null)
  }

  def makeSuccessfulAllocation(position: Int): MemoryManagerAllocationResponse =
    MemoryManagerAllocationResponse(success = true, position)

  def makeFailedAllocation(): MemoryManagerAllocationResponse =
    MemoryManagerAllocationResponse(success = false, 0)
}

class MemoryManager(memorySize: Int) {
  def allocate(size: Int): MemorySegment = {
    if (_freeMemorySegments.isEmpty || _freeMemorySegments.top.size < size) undefinedHandle
    else {
      val allocatedSegment = _freeMemorySegments.top
      _freeMemorySegments.Pop()
      val optExtraSegment = allocatedSegment.splitAt(size)

      if (optExtraSegment.isDefined) {
        _freeMemorySegments.Push(optExtraSegment.get)
        _memorySegments.insert(_memorySegments.indexWhere(_ == allocatedSegment) + 1, optExtraSegment.get)
      }

      allocatedSegment
    }
  }

  def free(segment: MemorySegment): Unit = {
    if (segment == undefinedHandle) return

    if (segment != _memorySegments.head) _appendIfFreePrevious(segment)

    if (segment != _memorySegments.last) _appendIfFreeNext(segment)

    _freeMemorySegments.Push(segment)
  }

  private val undefinedHandle = MemoryManager.undefinedHandle

  private val _memorySegments = ListBuffer(new MemorySegment(0, memorySize))

  private val _freeMemorySegments = {
    val segments = new OrderedHeap[MemorySegment](MemorySegment.heapObserver)
    segments.Push(_memorySegments.head)
    segments
  }

  private def _appendIfFree(remainingIndex: Int, appendingIndex: Int): Unit = {
    val remaining = _memorySegments.apply(remainingIndex)
    val appending = _memorySegments.apply(appendingIndex)

    if (appending.heapIndex != OrderedHeap.kNullIndex) {
      _freeMemorySegments.Erase(appending.heapIndex)
      _memorySegments -= appending
      remaining.unite(appending)
    }
  }

  private def _appendIfFreePrevious(segment: MemorySegment): Unit = {
    val segmentIndex = _memorySegments.indexWhere(_ == segment)
    val previousIndex = segmentIndex - 1

    _appendIfFree(segmentIndex, previousIndex)
  }

  private def _appendIfFreeNext(segment: MemorySegment): Unit = {
    val segmentIndex = _memorySegments.indexWhere(_ == segment)
    val nextIndex = segmentIndex + 1

    _appendIfFree(segmentIndex, nextIndex)
  }
}
