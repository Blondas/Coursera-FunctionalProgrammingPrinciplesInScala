object Session {
  def sumInts(a: Int, b: Int): Int =
    if(a > b) 0 else a + sumInts(a + 1, b)

//  def sumInts(a: Int, b: Int) = sum(x => x, a, b)

  def sum(f: Int => Int)(a: Int, b: Int) = {

    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  sum(x => x, 1, 3)

}