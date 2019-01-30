package adjunctions
package instances

abstract class Product[Class1[_], Class2[_], T]{
  type T1
  type T2

  val T1: Class1[T1]
  val T2: Class2[T2]

  def apply(t: T): (T1, T2)
  def make(t1: T1, t2: T2): T
}

object Product{

  type Aux[C1[_], C2[_], T, _T1, _T2] = Product[C1, C2, T]{
    type T1 = _T1
    type T2 = _T2
  }

  implicit def toClass1[C1[_], C2[_], T](implicit P: Product[C1, C2, T]): C1[P.T1] =
    P.T1

  implicit def toClass2[C1[_], C2[_], T](implicit P: Product[C1, C2, T]): C2[P.T2] =
    P.T2

  case class HomomorphismModule[C1[_], A1[_, _], C2[_], A2[_, _]](
    A1: Category.Aux[A1, C1],
    A2: Category.Aux[A2, C2]){

    abstract class Homomorphism[A, B]{
      val A: Product[C1, C2, A]
      val B: Product[C1, C2, B]

      val f1: A1[A.T1, B.T1]
      val f2: A2[A.T2, B.T2]
    }

    object Homomorphism{

      type Aux[A, B, PA <: Product[C1, C2, A], PB <: Product[C1, C2, B]] = Homomorphism[A, B]{
        val A: PA
        val B: PB
      }

      def apply[O1, O2](implicit _O1: Product[C1, C2, O1], _O2: Product[C1, C2, O2]) =
        new Builder[O1, O2, _O1.T1, _O2.T1, _O1.T2, _O2.T2](_O1, _O2)

      class Builder[O1, O2, O11, O21, O12, O22](
          _O1: Product.Aux[C1, C2, O1, O11, O12],
          _O2: Product.Aux[C1, C2, O2, O21, O22]){
        def apply(_f1: A1[O11, O21], _f2: A2[O12, O22]) = new Homomorphism[O1, O2]{
          val A = _O1
          val B = _O2

          val f1 = _f1
          val f2 = _f2
        }
      }

      object Category extends Category[Homomorphism]{
        type ObjectClass[T] = Product[C1, C2, T]

        def identity[O](implicit O: ObjectClass[O]): Homomorphism[O, O] =
          Homomorphism[O, O].apply(A1.identity[O.T1], A2.identity[O.T2])

        def compose[O1, O2, O3](
            a1: Homomorphism[O1, O2],
            a2: Homomorphism[O2, O3])(implicit
            O1: ObjectClass[O1],
            O2: ObjectClass[O2],
            O3: ObjectClass[O3]): Homomorphism[O1, O3] = {
          val h1 = A1.compose(a1.f1, a2.f1)(a1.A.T1, a1.B.T1, a2.B.T1)
          ??? // Homomorphism[O1, O3].apply(A1.compose(a1.f1, a2.f1), A2.compose(a1.f2, a2.f2))
        }
      }
    }
  }
}
