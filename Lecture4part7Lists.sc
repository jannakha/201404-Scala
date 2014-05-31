object Lecture4part7 {

	//insertion sort implementation is N*N 
	//pattern matching on List (Scala's own list)
  def isort(xs: List[Int]): List[Int]  = xs match {
  	case List() => List()
	  case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]
  
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
	  case List() => List(x)
	  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
	}                                         //> insert: (x: Int, xs: List[Int])List[Int]
	
  val nums = 1 :: 4 :: 2 :: 8 :: Nil              //> nums  : List[Int] = List(1, 4, 2, 8)
  isort(nums)                                     //> res0: List[Int] = List(1, 2, 4, 8)
}
