package webglgraphics

object Color {

  def HSL2RGB(h: Double, s: Double, l: Double): (Double, Double, Double) = {
    if (s == 0.0) {
      (l, l, l)
    } else {
      def HUE2RGB(p: Double, q: Double, t: Double): Double = {
        val newT = if (t < 0) t + 1 else if (t > 1) t - 1 else t

        if (t < 1.0 / 6)      p + (q - p) * 6 * newT
        else if (t < 1.0 / 2) q
        else if (t < 2.0 / 3) p + (q - p) * (2.0 / 3 - t) * 6
        else                  p
      }

      val q = if (l < 0.5) l * (1 + s) else l + s - l * s
      val p = 2 * l - q

      (HUE2RGB(p, q, h + 1.0 / 3), HUE2RGB(p, q, h), HUE2RGB(p, q, h - 1.0 / 3))
    }
  }

  def HSL2RGB(hsl: (Double, Double, Double)): (Double, Double, Double) = HSL2RGB(hsl._1, hsl._2, hsl._3)

  def RGB2HSL(r: Double, g: Double, b: Double): (Double, Double, Double) = {
    val max = math.max(math.max(r, g), b)
    val min = math.min(math.min(r, g), b)

    val l = (max + min) / 2

    if (max == min)
      (0, 0, l)
    else {
      val d = max - min
      val s = if (l > 0.5) d / (2 - max - min) else d / (max + min)

      val h: Double = (if (max == r) (g - b) / d + (if (g < b) 6.0 else 0.0)
      else if (max == g) (b - r) / 2 + 2
      else if (max == b) (r - g) / d + 4
      else throw new MatchError("Max of r, g, b should have been one of these.")) / 6.0

      (h, s, l)
    }
  }

  def RGB2HSL(rgb: (Double, Double, Double)): (Double, Double, Double) = RGB2HSL(rgb._1, rgb._2, rgb._3)


}
