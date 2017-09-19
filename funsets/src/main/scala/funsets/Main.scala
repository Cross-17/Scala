package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  def test : Set = x => if(x < 10) true else false
  println(test(9),test(11))
  def mapt: Set = map(test,x => 100 + x)
  println(mapt(109),mapt(111))
  printSet(mapt)
  printSet(test)
}
