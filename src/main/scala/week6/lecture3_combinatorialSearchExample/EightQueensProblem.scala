package week6.lecture3_combinatorialSearchExample

/*
  Problem: to place eight queens on a chessboard so that no queen is threatened
  by another => no 2 queens in the same row or diagonal.

  Requirements: chessboard any size.

  k - number of queens
  n - board size
  solution - set in reverse order of lists with columns numbers.

  Algorithm:
    1. Suppose that we have already generated all the solutions consisting of placing
      k-1 queens on a board of size n.
    2. Each solution is represented by a list (length = k-1) containing the numbers
      of columns (betweend 0 and n-1)
    3. The column number of the queen in the k-1th comes first in the list, followed
      by the column number of the queen in row k-2. etc.
    4. The solution set is thus represented as a set of lists, with one element for
      each solution.
    5. Now, to place the kth queen, we generate all possible extensions of each solution
      preceded by a new queen
 */
object EightQueensProblem extends App{

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else for {
        queens <- placeQueens(k -1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

    def isSafe(col: Int, queens: List[Int]): Boolean ={
      val row = queens.length
      val queensWithRows = (row -1 to 0 by -1) zip queens

      queensWithRows forall{case (r,c) => (c != col) && (math.abs(col - c) != row - r)}
    }

    placeQueens(n)
  }

  def show(queens: List[Int]){
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString

    println("\n" + (lines mkString "\n"))
  }

  queens(4) map show
}
