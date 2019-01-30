package adjunctions
package instances

object FunctionCategory extends Category[Function1]{

  type ObjectClass[T] = DummyImplicit

  def identity[O: ObjectClass]: O => O = identity[O]
  def compose[O1: ObjectClass, O2: ObjectClass, O3: ObjectClass](
    a1: O1 => O2, a2: O2 => O3): O1 => O3 = a2 compose a1
}
