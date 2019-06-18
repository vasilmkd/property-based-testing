package mk.ukim.finki.skit.property

sealed trait LinkedSet[A] extends (A => Boolean) {
  import LinkedSet._

  def union(other: LinkedSet[A]): LinkedSet[A] =
    foldLeft(other)((set, a) => Cons(a, set))

  def intersection(other: LinkedSet[A]): LinkedSet[A] =
    filter(other.apply)

  def diff(other: LinkedSet[A]): LinkedSet[A] =
    filter(!other(_))

  def xor(other: LinkedSet[A]): LinkedSet[A] =
    diff(other) union other.diff(this)

  def +(elem: A): LinkedSet[A] = Cons(elem, this)

  def -(elem: A): LinkedSet[A] = filter(_ != elem)

  def foldRight[B](zero: B)(f: (A, B) => B): B = this match {
    case Empty() => zero
    case Cons(h, t) => f(h, t.foldRight(zero)(f))
  }

  def foldLeft[B](zero: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(set: LinkedSet[A], zero: B)(f: (B, A) => B): B = set match {
      case Empty() => zero
      case Cons(h, t) => loop(t, f(zero, h))(f)
    }

    loop(this, zero)(f)
  }

  def filter(p: A => Boolean): LinkedSet[A] =
    foldLeft(LinkedSet.empty[A]) { (set, a) =>
      if (p(a)) Cons(a, set) else set
    }

  def toList: List[A] =
    foldLeft(List.empty[A])((list, a) => a :: list)

  def mkString(pre: String = "", sep: String = "", post: String = ""): String =
    foldLeft(pre)(_ + _.toString + ", ").dropRight(2) + post

  override def toString: String = mkString("LinkedSet(", ",", ")")
}

object LinkedSet { self =>

  private final case class Empty[A]() extends LinkedSet[A] {
    override def apply(a: A): Boolean = false
  }

  private final case class Cons[A](head: A, tail: LinkedSet[A]) extends LinkedSet[A] {
    override def apply(a: A): Boolean = a == head || tail(a)
  }

  def empty[A]: LinkedSet[A] = Empty()

  def apply[A](as: A*): LinkedSet[A] =
    as.foldLeft(empty[A])(_ + _)
}
