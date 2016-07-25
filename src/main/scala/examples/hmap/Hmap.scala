package examples.hmap

object Hmap {

  class GenericType[T](value: T)
  class GMap[K,V]

  case class GInt(k: Int) extends GenericType[Int](k)
  case class GString(k: String) extends GenericType[String](k)

  implicit object int2String extends GMap[GInt, GString]
  implicit object string2Int extends GMap[GString, GInt]

  def main(args: Array[String]) = {

    val hoMap = shapeless.HMap[GMap](
      GInt(0) -> GString("a"),
      GInt(99) -> GString("b"),
      GString("key-a") -> GInt(12)
    )

    println(hoMap.get(GInt(0)).get)
    println(hoMap.get(GString("key-a")).get)
    println(hoMap.get(GInt(99)).get)
  }
}