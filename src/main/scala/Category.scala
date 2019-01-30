package adjunctions


trait Category[A[_, _]]{
  type ObjectClass[_]

  def identity[O: ObjectClass]: A[O, O]
  def compose[O1: ObjectClass, O2: ObjectClass, O3: ObjectClass](
    a1: A[O1, O2], a2: A[O2, O3]): A[O1, O3]
}

object Category{

  type Aux[A[_, _], OC[_]] = Category[A]{ type ObjectClass[t] = OC[t] }
}
