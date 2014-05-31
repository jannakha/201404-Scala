///Pattern matching example
object Lec4part6 {

trait Expr {
  def eval: Int = this match {
		case Number(n) => n
		case Sum(e1, e2) => e1.eval + e2.eval
		}
		
	def show: String = this match {
		case Number(n) => n.toString
		case Sum(e1, e2) => "(" + e1.show + " + " + e2.show + ")"
		case Var(n) => n
		case Prod(e1, e2) => e1.show + " * " + e2.show
	}
		
}
case class Number(n: Int) extends Expr
case class Var(n: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
		
Sum(Number(1), Number(2)).eval                    //> res0: Int = 3
Sum(Number(2), Number(4)).eval                    //> res1: Int = 6

Sum(Prod(Number(2), Var("x")), Var("y")).show     //> res2: String = (2 * x + y)
Prod(Sum(Number(2), Var("x")), Var("y")).show     //> res3: String = (2 + x) * y
}
