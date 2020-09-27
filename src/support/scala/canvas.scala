package sdraw

import colors.IColor
import geometry.{Posn => JavaPos}
import draw.{Canvas => JavaCanvas}

import sgeometry.Pos

case class Canvas(_canvas: JavaCanvas) {
  def width: Int = _canvas.getWidth()
  def height: Int = _canvas.getHeight()

  def drawCircle(c: Pos, radius: Int, color: IColor): Boolean =
    _canvas.drawCircle(new JavaPos(c.x, c.y), radius, color)

  def drawDisk(c: Pos, radius: Int, color: IColor): Boolean =
    _canvas.drawDisk(new JavaPos(c.x, c.y), radius, color)

  def drawOval(c: Pos, radiusx: Int, radiusy: Int, color: IColor): Boolean =
    _canvas.drawOval(new JavaPos(c.x, c.y), radiusx, radiusy, color)

  def drawOvals(cs: List[Pos], radiusx: Int, radiusy: Int, color: IColor): Boolean = {
    cs match {
      case Nil     => true
      case c :: cs =>
        _canvas.drawOval(new JavaPos(c.x, c.y), radiusx, radiusy, color)
        drawOvals(cs, radiusx, radiusy, color)
    }
  }

  def drawFrame(c: Pos, width: Int, height: Int, color: IColor): Boolean = {
    _canvas.drawLine(new JavaPos(c.x, c.y), new JavaPos(c.x+width, c.y), color)
    _canvas.drawLine(new JavaPos(c.x+width, c.y), new JavaPos(c.x+width, c.y+height), color)
    _canvas.drawLine(new JavaPos(c.x+width, c.y+height), new JavaPos(c.x, c.y+height), color)
    _canvas.drawLine(new JavaPos(c.x, c.y+height), new JavaPos(c.x, c.y), color)
  }

  def drawRect(c: Pos, width: Int, height: Int, color: IColor): Boolean =
    _canvas.drawRect(new JavaPos(c.x, c.y), width, height, color)

  def drawString(c: Pos, msg: String): Boolean =
  _canvas.drawString(new JavaPos(c.x, c.y), msg)
}
