package colors;

import java.awt.Color;

//to represent a color
public abstract class AbstractColor implements IColor {

  @Override
  public abstract Color getColor();

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    return true;
  }

  @Override
  public String toString() {
    return "new "+getClass().getSimpleName();
  }

}
