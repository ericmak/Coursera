package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
 
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val delta = computeDelta(a, b, c)
    
      Signal(delta() match {
        case 0 => Set(-b() / (a() * 2))
        case d if delta() > 0 => 
          import java.lang.Math.sqrt
          Set(
            (-b() + sqrt(delta()))/(a() * 2),
            (-b() - sqrt(delta()))/(a() * 2))  
        case d if d < 0 => Set(Double.NaN, Double.NaN)
      })
  }
}
