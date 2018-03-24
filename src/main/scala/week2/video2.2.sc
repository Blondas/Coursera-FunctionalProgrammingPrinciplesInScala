object excercise {

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factorial(x: Int) = product(x => x)(1, x)




  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))







  factorial(5)

  mapReduce((x: Int) => x * x  , (x: Int, y: Int) => x , 1)(1, 2)
}

