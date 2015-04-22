package calculator

object testing {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr
  
  def computeValues(
      namedExpressions: Map[String, Expr]): Map[String, Double] = {
      
        Map("a" -> 3.0)
        
  }                                               //> computeValues: (namedExpressions: Map[String,calculator.testing.Expr])Map[St
                                                  //| ring,Double]
  
  def eval(expr: Expr, references: Map[String, Expr]): Double = {
    
   3.0

  }                                               //> eval: (expr: calculator.testing.Expr, references: Map[String,calculator.test
                                                  //| ing.Expr])Double
  
  val names = (0 until 10).map(i => ('a' + i).toChar.toString)
                                                  //> names  : scala.collection.immutable.IndexedSeq[String] = Vector(a, b, c, d, 
                                                  //| e, f, g, h, i, j)
  
  
  
}
  
  
  