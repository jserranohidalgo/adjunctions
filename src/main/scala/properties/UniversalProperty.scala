package adjunctions


trait UniversalProperty[L[_], X]{
  type A

  def universalArrow: (A, L[A] => X)


}
