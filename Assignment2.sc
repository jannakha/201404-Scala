object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x => x == elem)
                                                  //> singletonSet: (elem: Int)Int => Boolean
    
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = ( x => s(x) || t(x))
                                                  //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
  

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = ( x => s(x) && t(x))
                                                  //> intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
	
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = ( x => (s(x) && !t(x)) || (!s(x) && t(x)))
                                                  //> diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x => s(x) && p(x))
                                                  //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 50                                  //> bound  : Int = 50

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    
    def iter(a: Int): Boolean = {
    	
    	
      if (a > bound) true //terminating condition: didn't return false earlier => must be true
      else if (s(a) && !p(a)) {
      println(a)
      false
      }
      else iter(a + 1)
    }
    
    iter(-bound)
  }                                               //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
                                                  //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean
  	

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = x => exists(s, y => f(y) == x )
                                                  //> map: (s: Int => Boolean, f: Int => Int)Int => Boolean
  
  //
  /*
  ///slightly map reduced map function
  def mapR(s: Set, f: Int => Int): Set =
  {

  	def mapReduce(combine:(Set, Set) => Set)(a:Int, t:Set): Set =
			if (a>bound) Set()
			else if (s(a)) union(singletonSet(f(a)), mapReduce(union)(a+1, t) )
			else mapReduce(union)(a+1, t)
			
		mapReduce(union)(-bound, Set())
  }
  
  */
  
  ///iterative map function
  def mapI(s: Set, f: Int => Int): Set = {
  
  	def iter(a: Int, t: Set): Set = {
      if (a > bound) t //terminating condition: didn't return false earlier => must be true
      else if (s(a) && f(a) >= -bound && f(a) <= bound) {
      	iter(a + 1, union(t, singletonSet(f(a))))
      }
      else iter(a + 1, t)
    }
    iter(-bound, Set())
  }                                               //> mapI: (s: Int => Boolean, f: Int => Int)Int => Boolean
  
 //*/
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Int => Boolean)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Int => Boolean)Unit
  
  
  
  var set00 : Set = x => x > 0 && x < 9           //> set00  : Int => Boolean = <function1>
  printSet(set00)                                 //> {1,2,3,4,5,6,7,8}
  
  var set01 : Set = x => x > 5 && x < 10          //> set01  : Int => Boolean = <function1>
  printSet(set01)                                 //> {6,7,8,9}
  
  forall(set01, x => x < 15)                      //> res0: Boolean = true
  forall(set01, x => x < 0)                       //> 6
                                                  //| res1: Boolean = false
  
  var setFf = map(set01, x => x*x)                //> setFf  : Int => Boolean = <function1>
  printSet(setFf)                                 //> 6
                                                  //| 7
                                                  //| {36,49}
  
  exists(set01, x => x < 8)                       //> 6
                                                  //| res2: Boolean = true
  exists(set01, x => x < -1)                      //> res3: Boolean = false
  
  var setM = map(set01, x => x * x)               //> setM  : Int => Boolean = <function1>
  printSet(setM)                                  //> 6
                                                  //| 7
                                                  //| {36,49}
  
  var setMR = mapI(set01, x => x * x)             //> setMR  : Int => Boolean = <function1>
  printSet(setMR)                                 //> {36,49}
  
  var setF = filter(set00, x => x < 0)            //> setF  : Int => Boolean = <function1>
  printSet(setF)                                  //> {}
  
  var setU = union(set00, set01)                  //> setU  : Int => Boolean = <function1>
  printSet(setU)                                  //> {1,2,3,4,5,6,7,8,9}
  
  var setIntersect = intersect(set00, set01)      //> setIntersect  : Int => Boolean = <function1>
  printSet(setIntersect)                          //> {6,7,8}
  
  var setDiff = diff(set00, set01)                //> setDiff  : Int => Boolean = <function1>
  printSet(setDiff)                               //> {1,2,3,4,5,9}
  
  var sing1: Set = singletonSet(1)                //> sing1  : Int => Boolean = <function1>
  var sing2: Set = singletonSet(2)                //> sing2  : Int => Boolean = <function1>
  printSet(union(sing1, sing2))                   //> {1,2}
}
