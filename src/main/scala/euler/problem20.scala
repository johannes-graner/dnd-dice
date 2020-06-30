package euler

object problem20 {
  def factorial(n: Int): Int = {
      if (n == 0)
        1
      else
        n*factorial(n-1)
  }

  def test() {
      println(factorial(50))
  }
}
