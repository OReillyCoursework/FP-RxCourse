package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      
      Var {
        (b() * b()) - (4 * a() * c()) 
      }      
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      
      var set = Set[Double]
      set += (-b() - sqrt(delta())) / (2*a())
      set += (-b() + sqrt(delta())) / (2*a())
      Var {
        set      
      }
  }
}
