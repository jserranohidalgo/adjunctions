package adjunctions
package instances

trait Monoid[T]{
  def zero(): T
  def append(a1: T, a2: T): T
}

object Monoid{

  def apply[T](implicit M: Monoid[T]) = M

  implicit def fromFunction1[A: Monoid, B: Monoid](f: A => B): Homomorphism[A, B] =
    Homomorphism(f)

  case class Homomorphism[A: Monoid, B: Monoid](f: Function1[A, B]){

    def law1(): Boolean =
      f(Monoid[A].zero) == Monoid[B].zero

    def law2(a1: A, a2: A): Boolean =
      f(Monoid[A].append(a1, a2)) == Monoid[B].append(f(a1), f(a2))
  }

  object Homomorphism{

    object MonoidsCategory extends Category[Homomorphism]{
      type ObjectClass[T] = Monoid[T]

      def identity[A: Monoid]: Homomorphism[A, A] =
        (a: A) => a

      def compose[O1: Monoid, O2: Monoid, O3: Monoid](
          a1: Homomorphism[O1, O2],
          a2: Homomorphism[O2, O3]): Homomorphism[O1, O3] =
        a2.f compose a1.f
    }
  }
}

