package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      
      Var {
        b() * b() - 4.0 * a() * c() 
      }      
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      
      var set: Set[Double] = Set()
      val aa = a() + a()
      
      if (delta() < 0) {
        
          val re = -b() / aa
          val im = math.sqrt(-delta())/aa
          // not doing anything with the result of complex roots
      } else {
        val re = if (b() < 0) (-b + math.sqrt(delta()))/aa else (-b - math.sqrt(delta()))/aa)
        // need to add roots to set
      }

      Var {
        set      
      }
  }
}
