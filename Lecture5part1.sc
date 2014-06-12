//Lists, functions on lists

object Lecture5Part1 {

///get last element of list
def last[T](xs:List[T]): T = xs match {
case List() => throw new Error("empmty list")
case List(y) => y
case y::ys => last(ys)
}                                                 //> last: [T](xs: List[T])T

///get a list of first elements of list (exclude last element)
def init[T](xs:List[T]) : List[T] = xs match {
	case List()  => throw new Error("empty list")
	case List(x) => List()
	case y::ys => y :: init(ys)
}                                                 //> init: [T](xs: List[T])List[T]

//concatenate 2 lists
//complexity: |xs| (length of list)
def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
	case List() => ys
	case z::zs => z :: concat(zs, ys)
}                                                 //> concat: [T](xs: List[T], ys: List[T])List[T]

///reverse list
///complexity: m*m - better implementation later
def reverse[T](xs: List[T]): List[T] = xs match {
	case List() => List()
	case y::ys => reverse(ys) ++ List(y)
}                                                 //> reverse: [T](xs: List[T])List[T]

///remove element at position n
def removeAt[T](n: Int, xs: List[T]) : List[T] = xs match {
	case List() => List()
	case y::yx => if (n == 0) removeAt(-1, yx) else y :: removeAt(n-1, yx)
}                                                 //> removeAt: [T](n: Int, xs: List[T])List[T]

///remove element at position n with drop and take
def removeAtSolution[T](n: Int, xs: List[T]) : List[T] = (xs take n) ::: (xs drop (n+1))
                                                  //> removeAtSolution: [T](n: Int, xs: List[T])List[T]

///flatten list of elements that can be lists in lists
def flatten(xs: List[Any]) : List [Any] = xs match {
	case List() => List()
	//y:List[Any] - matching on type, can du y:Int, y:Boolean etc
	case (y:List[Any])::ys => flatten(y) ::: flatten(ys)
	case y::ys => y :: flatten(ys)
}                                                 //> flatten: (xs: List[Any])List[Any]

var multiList : List[Any] = List(List(1,1), 2, List(3, List(5, 8)))
                                                  //> multiList  : List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))
println(flatten(multiList))                       //> List(1, 1, 2, 3, 5, 8)


var list: List[Char] = List('a', 'b', 'c', 'd', 'e')
                                                  //> list  : List[Char] = List(a, b, c, d, e)

println(removeAt(1, list))                        //> List(a, c, d, e)
println(removeAt(5, list))                        //> List(a, b, c, d, e)
println(removeAt(0, list))                        //> List(b, c, d, e)
println(removeAt(-1, list))                       //> List(a, b, c, d, e)

}
