package funsets

object Main extends App {
  import FunSets._
  println(map(singletonSet(2), x => x*x))
}
