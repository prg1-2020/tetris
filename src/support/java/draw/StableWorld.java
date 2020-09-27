package draw;

//to represent a stable world
class StableWorld extends World {
  private String message;
  StableWorld(String message) {
    this.message = message;
  }
  @Override
  public World onTick() {
    return this;
  }

  @Override
  public World onKeyEvent(String key) {
    return this;
  }

  @Override
  public boolean draw() {
    return true;
  }
  @Override
  String endingMessage() {
    return this.message;
  }
  @Override
  boolean inProgess() {
    return false;
  }
}
