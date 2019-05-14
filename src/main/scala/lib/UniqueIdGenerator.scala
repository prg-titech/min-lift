package lib

class UniqueIdGenerator(prefix: String) {
  var count = 0

  def this() = this("temp")

  def generateInt() : Int = {
    val value = count
    count += 1
    value
  }

  def generateString() : String = {
    s"${prefix}${generateInt()}"
  }
}
