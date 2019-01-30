package adjunctions
package instances

sealed class IsUnit[T]

object IsUnit{

  def apply[T](U: IsUnit[T]) = U

  implicit def unit = new IsUnit[Unit]

  case class Homomorphism[A: IsUnit, B: IsUnit]()

  object Homomorphism{

    object UnitCategory extends Category[Homomorphism]{

      type ObjectClass[T] = IsUnit[T]

      def identity[O: IsUnit] =
        Homomorphism[O, O]()

      def compose[O1: IsUnit, O2: IsUnit, O3: IsUnit](
          a1: Homomorphism[O1, O2], a2: Homomorphism[O2, O3]) =
        Homomorphism[O1, O3]()
    }
  }
}
