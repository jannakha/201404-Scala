import math.Ordering

object Lecture5Part3 {

  ///merge sort for any type of data - msort[T]
  ///need to pass less than (lt) function that takes to parameters of T and returns Boolean
	def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
	
	def merge(xs: List[T], ys: List[T]) : List[T] = (xs, ys) match {
		case (List(), ys) => ys
		case (xs, List()) => xs
		case (x::xs1, y::ys1) => if (lt(x,y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
	}
	
		val n = xs.length/2
		if (n == 0) xs
		else {
			val (fst, snd) = xs splitAt n
			merge(msort(fst)(lt), msort(snd)(lt))
		}
	}                                         //> msort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]
	
	///merge sort for any type of data - msort[T]
  ///math.Ordering[T]
	def msortUsingOrdering[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
	
	def merge(xs: List[T], ys: List[T]) : List[T] = (xs, ys) match {
		case (List(), ys) => ys
		case (xs, List()) => xs
		case (x::xs1, y::ys1) => if (ord.lt(x,y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
	}
	
		val n = xs.length/2
		if (n == 0) xs
		else {
			val (fst, snd) = xs splitAt n
			merge(msortUsingOrdering(fst)(ord), msortUsingOrdering(snd)(ord))
		}
	}                                         //> msortUsingOrdering: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])L
                                                  //| ist[T]
	
	
	val unsortedList = List(1, 4, 5, 3, 6, 7, 3, 4, 5, 2, 1, 2, 6)
                                                  //> unsortedList  : List[Int] = List(1, 4, 5, 3, 6, 7, 3, 4, 5, 2, 1, 2, 6)
  val unsortedFruits = List("apple", "orange", "banana", "pineapple")
                                                  //> unsortedFruits  : List[String] = List(apple, orange, banana, pineapple)
  
  //msort with comparison function
	println(msort(unsortedList)((x: Int, y:Int) => x < y))
                                                  //> List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7)
  //scala compiler will infer the types of compare function, so no need to put types
  println(msort(unsortedFruits)((x, y) => x.compareTo(y) < 0))
                                                  //> List(apple, banana, orange, pineapple)
  //msort using math.Ordering
  println(msortUsingOrdering(unsortedList)(Ordering.Int))
                                                  //> List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7)
  println(msortUsingOrdering(unsortedFruits)(Ordering.String))
                                                  //> List(apple, banana, orange, pineapple)
  //using implicit parameter, eg compiler will find a single most specific definition for the argument
  //compiler will synthesize Ordering.Int for List[Int] and Ordering.String for List[String]
  println(msortUsingOrdering(unsortedList))       //> List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7)
  println(msortUsingOrdering(unsortedFruits))     //> List(apple, banana, orange, pineapple)
   
}
