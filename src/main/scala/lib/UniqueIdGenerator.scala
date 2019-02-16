package lib

class UniqueIdGenerator {
  var count = 0

  def generateInt() : Int = {
    val value = count
    count += 1
    value
  }
}
