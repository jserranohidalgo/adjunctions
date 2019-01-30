package adjunctions

trait Exponent[~>[_, _]]{
  type x[_, _]
  val P: Product[x]

  def eval[A, X](exp: X ~> A, x: X): A

  def curry[B, A, X](f: B x X => A): B => (X ~> A)
}

object Exponent{

  implicit def ScalaFn[A] = new Exponent[Function1]{
    type x[A, B] = (A, B)
    val P = Product.tuple2Product

    def eval[A, X](exp: X => A, x: X): A =
      exp(x)

    def curry[B, A, X](f: ((B, X)) => A): B => X => A =
      (b: B) => (x: X) => f((b,x))
  }
}
