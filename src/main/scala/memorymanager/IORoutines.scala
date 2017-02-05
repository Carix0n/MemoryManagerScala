package memorymanager

import scala.io.Source

object IORoutines {
  private val _params = Source
    .fromInputStream(System.in)
    .getLines()
    .flatMap(_.split(" "))
    .map(_.toInt)

  def readMemorySize(): Int = _params.next()

  def readMemoryManagerQueries(): Seq[MemoryManagerQuery] = {
    val numberOfQueries = _params.next()
    for (_ <- 0 until numberOfQueries) yield {
      val queryDescriptor = _params.next()

      if (queryDescriptor >= 0) AllocationQuery(queryDescriptor)
      else FreeQuery(-queryDescriptor)
    }
  }

  def outputMemoryManagerResponses(responses: Seq[MemoryManagerAllocationResponse]): Unit = {
    responses.foreach {
      case MemoryManagerAllocationResponse(true, position) => println(position + 1)
      case MemoryManagerAllocationResponse(false, _) => println(-1)
    }
  }
}
