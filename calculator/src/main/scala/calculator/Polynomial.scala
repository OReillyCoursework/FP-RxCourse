package calculator

object Polynomial { 
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {

        b() * b() - 4.0 * a() * c() 

  }

 def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

      var set: Set[Double] = Set()

      val aa = a() + a()

      if (delta() < 0) {

          val re = -b() / aa

          val im = math.sqrt(-delta())/aa

          // not doing anything with the result of complex roots

      } else if (delta() > 0) {

        
        var root1 =  (-b() + math.sqrt(delta()))/aa  
        var root2 =  (-b() - math.sqrt(delta()))/aa  
        if (root1 != root2) {
            set += root1
            set += root2
        } else 
            set += root1
      

      } else {  // delta is zero
        
        var root1 =  (-b() + math.sqrt(delta()))/aa  
        var root2 =  (-b() - math.sqrt(delta()))/aa  
        if (root1 != root2) {
            set += root1
            set += root2
        } else 
            set += root1

      }

        set      

  }

}



