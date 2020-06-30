package euler

object problem1 {
  def mults_below(n: Int): List[Int] = {
    List.range(1,n).filter((p:Int) => p % 3 == 0 || p % 5 == 0)
  }

  def sum_mul_bel(n:Int):Int = {
    mults_below(n).sum
  }

  def test() = {
    println(sum_mul_bel(1000))
  }
}
