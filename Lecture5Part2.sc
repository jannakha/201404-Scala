object Lecture5Part2 {
  
	def msort(xs: List[Int]): List[Int] = {
		val n = xs.length/2
		if (n == 0) xs
		else {
			val (fst, snd) = xs splitAt n
			mergeAsTuple(msort(fst), msort(snd))
		}
	}                                         //> msort: (xs: List[Int])List[Int]
	
	def mergeSimple(xs: List[Int], ys: List[Int]) : List[Int] = xs match {
		case Nil => ys
		case x :: xs1 =>
			ys match {
				case Nil => xs
			case y :: ys1 =>
				if (x < y) x :: mergeSimple(xs1, ys)
				else y :: mergeSimple(xs, ys1)
			}
	}                                         //> mergeSimple: (xs: List[Int], ys: List[Int])List[Int]
	
	def mergeAsTuple(xs: List[Int], ys: List[Int]) : List[Int] = (xs, ys) match {
		case (List(), ys) => ys
		case (xs, List()) => xs
		case (x::xs1, y::ys1) => if (x < y) x :: mergeAsTuple(xs1, ys) else y :: mergeAsTuple(xs, ys1)
	}                                         //> mergeAsTuple: (xs: List[Int], ys: List[Int])List[Int]
	
	
	val unsortedList = List(1, 4, 5, 3, 6, 7, 3, 4, 5, 2, 1, 2, 6)
                                                  //> unsortedList  : List[Int] = List(1, 4, 5, 3, 6, 7, 3, 4, 5, 2, 1, 2, 6)
	println(msort(unsortedList))              //> List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7)
}
