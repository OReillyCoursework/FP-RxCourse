package calculator

object Polynomial { 
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {

   Signal {
        b()  
   }

  }

 def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Var {    
          delta() match {
              case delta if delta > 0 => Set(((-b() + math.sqrt(delta))/a()),((-b() - math.sqrt(delta))/a()))  
              case delta if delta == 0 => Set(((-b() + math.sqrt(delta))/a()),((-b() - math.sqrt(delta))/a()))  
              case _ => Set(0.0) // delta less than zero which has no roots
          }
      }
  }    
}



