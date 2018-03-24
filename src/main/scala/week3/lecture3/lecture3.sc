trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}
class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

def nth[T](index: Int, list: List[T]): T =
  if (list isEmpty) throw new IndexOutOfBoundsException("list is empty")
  else if (index == 0) list head
  else nth(index -1, list tail)


val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(0, list)