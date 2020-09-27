package sdraw

import draw.{World => _World}

abstract class World() { world =>
  object _world extends _World {
    override def onClick(p: geometry.Posn): _World =
      world.click(sgeometry.Pos(p.x, p.y))._world
    def onTick(): _World =
      world.tick()._world
    def onKeyEvent(key: String): _World =
      world.keyEvent(key)._world
    def draw(): Boolean = world.draw()
  }

  var theCanvas: Option[Canvas] = None
  def canvas: Canvas = {
    if (theCanvas.isEmpty) theCanvas = Some(Canvas(_world.theCanvas))
    theCanvas.get
  }

  def bigBang(width: Int, height: Int, t: Double): Boolean = _world.bigBang(width, height, t)
  def endOfWorld(s: String) { _world.endOfWorld(s) }

  def draw(): Boolean

  def click(p: sgeometry.Pos): World
  def tick(): World
  def keyEvent(key: String): World
}
