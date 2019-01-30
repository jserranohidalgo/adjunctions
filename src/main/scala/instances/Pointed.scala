package adjunctions
package instances

case class Pointed[T](nullT: T)

object Pointed{

  def apply[T](implicit P: Pointed[T]) = P

  implicit def optionPointed[T] = Pointed[Option[T]](None)

  implicit def fromFunction1[A: Pointed, B: Pointed](f: A => B): Homomorphism[A, B] =
    Homomorphism(f)

  case class Homomorphism[A: Pointed, B: Pointed](f: A => B){
    def law(): Boolean =
      f(Pointed[A].nullT) == Pointed[B].nullT
  }

  object Homomorphism{

    object PointedsCategory extends Category[Homomorphism]{
      type ObjectClass[T] = Pointed[T]

      def identity[A: Pointed]: Homomorphism[A, A] =
        (a: A) => a

      def compose[O1: Pointed, O2: Pointed, O3: Pointed](
          a1: Homomorphism[O1, O2],
          a2: Homomorphism[O2, O3]): Homomorphism[O1, O3] =
        a2.f compose a1.f
    }
  }
}
