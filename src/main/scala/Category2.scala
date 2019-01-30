package adjunctions

abstract class Category{
  type Object[_]
  type Arrow[_, _]

  def identity[T]: Arrow[Object[T], Object[T]]
  def comp[T1, T2, T2](a1: Arrow[Object[T1], Object[T2]],
    a2: Arrow[Object[T2], Object[T3]]): Arrow[Object[T1], Object[T3]]
}
