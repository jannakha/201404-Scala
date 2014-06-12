object Lecture5Part4 {
	//packs consecutive elements in an original list into a list
  def pack[T](xs: List[T]): List[List[T]] = xs match {
  	case Nil => Nil
  	case x :: xs1 =>
  		val (head, tail) = xs span (y => y == x)
  		head :: pack(tail)
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  
  //converts a list into a list of pairs of value and list count
  def encode[T] (xs: List[T]) : List[(T, Int)] = {
  	pack(xs) map (subList => (subList.head, subList.length))
  }                                               //> encode: [T](xs: List[T])List[(T, Int)]
  
  val someList = List("a", "a", "a", "c", "c", "b", "a", "d", "d")
                                                  //> someList  : List[String] = List(a, a, a, c, c, b, a, d, d)
  pack(someList)                                  //> res0: List[List[String]] = List(List(a, a, a), List(c, c), List(b), List(a),
                                                  //|  List(d, d))
  encode(someList)                                //> res1: List[(String, Int)] = List((a,3), (c,2), (b,1), (a,1), (d,2))
}
