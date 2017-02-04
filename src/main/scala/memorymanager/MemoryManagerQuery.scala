package memorymanager

trait MemoryManagerQuery

case class AllocationQuery(allocationSize: Int) extends MemoryManagerQuery

case class FreeQuery(allocationQueryIndex: Int) extends MemoryManagerQuery

case class MemoryManagerAllocationResponse(success: Boolean, position: Int)
