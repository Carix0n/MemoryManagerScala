package memorymanager

object Main extends App {
  val memorySize = IORoutines.readMemorySize()
  val queries = IORoutines.readMemoryManagerQueries()
  val responses = MemoryManager.run(memorySize, queries)
  IORoutines.outputMemoryManagerResponses(responses)
}
