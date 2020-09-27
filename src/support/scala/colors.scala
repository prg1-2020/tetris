package sdraw

trait Color extends colors.IColor
case object Black     extends colors.Black     with Color
case object Blue      extends colors.Blue      with Color
case object Cyan      extends colors.Cyan      with Color
case object DarkGray  extends colors.DarkGray  with Color
case object Green     extends colors.Green     with Color
case object LightGray extends colors.LightGray with Color
case object Magenta   extends colors.Magenta   with Color
case object Orange    extends colors.Orange    with Color
case object Pink      extends colors.Pink      with Color
case object Red       extends colors.Red       with Color
case object White     extends colors.White     with Color
case object Yellow    extends colors.Yellow    with Color
case object NoColor   extends colors.White     with Color
object Transparent extends Color {
  def getColor(): java.awt.Color = new java.awt.Color(1, 1, 1, 1)
}
case class HSB(h: Float, s: Float, b: Float) extends Color {
  def getColor(): java.awt.Color = java.awt.Color.getHSBColor(h, s, b)
}
