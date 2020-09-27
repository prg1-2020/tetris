package draw.test;

import colors.Blue;
import colors.Red;
import draw.World;
import geometry.Posn;

public class TestWorldClick extends World {
  private Posn lastLoc;

  TestWorldClick(Posn lastLoc) {
    this.lastLoc = lastLoc;
  }
  public static void main(String[] args) {
    new TestWorldClick(new Posn(0,0)).bigBang(500, 500, 0.1);
  }

  @Override
  public World onTick() {
    return new TestWorldClick(new Posn(this.lastLoc.x, this.lastLoc.y+5));
  }

  @Override
  public World onKeyEvent(String key) {
    if (key.equals("x")) endOfWorld("bye");
    System.out.println(key);
    return this;
  }

  @Override
  public boolean draw() {
    return theCanvas.drawRect(new Posn(0,0), 500, 500, new Blue())
        && theCanvas.drawDisk(this.lastLoc, 5, new Red());
  }
  @Override
  public World onClick(Posn location) {
    return new TestWorldClick(location);
  }

}
