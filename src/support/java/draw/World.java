package draw;

import geometry.Posn;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.SynchronousQueue;

import javax.swing.Timer;

/**
 * to represent worlds of games
 * 
 */
public abstract class World implements KeyListener {
  public Canvas theCanvas;
  private int worldWidth;
  private int worldHeight;

  /**
   * to start evolving the world from this state, calling onTick method in every
   * s seconds
   * 
   * @param width
   *          --- size of the canvas
   * @param height
   *          --- size of the canvas
   * @param s
   *          --- interval of calling onTick
   * @return true if the world is properly stopped
   */
  public boolean bigBang(int width, int height, final double s) {
    if (width <= 0)
      throw new RuntimeException("The method bigBang(int,int,double) expects "
          + "the first argument to be greather than 0, given " + width);
    if (height <= 0)
      throw new RuntimeException("The method bigBang(int,int,double) expects "
          + "the second argument to be greather than 0, given " + height);
    if (s <= 0)
      throw new RuntimeException("The method bigBang(int,int,double) expects "
          + "the third argument to be greather than 0, given " + s);
    this.worldWidth = width;
    this.worldHeight = height;
    theCanvas = new Canvas(width, height, this.getClass().getSimpleName());
    theCanvas.addKeyListener(this);
    theCanvas.show();
    theCanvas.dialog(driverLoop(s, World.this).endingMessage());
    return true;
  }

  String endingMessage() {
    return "This is the end of the world.";
  }

  /**
   * to stop this world (same as endOfWorld)
   * 
   * @param message
   * @return
   */
  // public boolean endOfTime(String message) {
  // endOfWorld(message);
  // return true;
  // }

  /**
   * to stop this world with showing a dialog for confirmation
   * 
   * @param s
   *          --- a message to show
   * @return this world
   */
  public World endOfWorld(final String s) {
    return new StableWorld(s);
  }

  /**
   * to create the state of this world after elapsed a specified amount of time
   * 
   * @return the world with new state
   */
  public abstract World onTick();

  /**
   * to create the state of this world after pressed a key
   * 
   * @param key
   *          --- the character of the pressed a regular key, or the name of the
   *          pressed special key
   * @return the world with new state
   */
  public abstract World onKeyEvent(String key);

  /**
   * to create the state of this world after clicked the given location on the
   * canvas
   * 
   * @param location
   * @return the world with new state
   */
  public World onClick(Posn location) {
    return this;
  }

  /**
   * to draw the current state of this world onto theCanvas field
   * 
   * @return true if successfully drawn
   */
  public abstract boolean draw();

  // ------------------------------------------

  private World world;

  private World driverLoop(final double s, World w) {
    this.world = w;
    final SynchronousQueue<World> q = new SynchronousQueue<>();
    Timer timer = new Timer((int) (1000 * s), new ActionListener() {

      @Override
      public void actionPerformed(ActionEvent e) {
        World.this.updateWorld();
        if (!World.this.world.inProgess()) {
          q.add(World.this);
        }
      }
    });
    theCanvas.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        World.this.world = new StableWorld(
            "The world has been terminated by the external force.");
        q.add(World.this.world);
      }
    });
    theCanvas.addMouseListener(new MouseAdapter() {

      @Override
      public void mouseClicked(MouseEvent e) {
        World.this.mouseClicked(e);
      }
    });
    timer.start();

    try {
      q.take();
    } catch (InterruptedException e1) {
      e1.printStackTrace();
    }
    timer.stop();
    return this.world;
  }

  private void updateWorld() {
    this.world = this.world.onTick();
    this.world.theCanvas = this.theCanvas;
    this.world.draw();
  }

  boolean inProgess() {
    return true;
  }

  private void mouseClicked(MouseEvent e) {
    int x = e.getX(), y = e.getY();
    int w = theCanvas.getWidth(), h = theCanvas.getHeight();
    int worldX = this.worldWidth*x / w, worldY = this.worldHeight*y / h;
    
    this.world = this.world.onClick(new Posn(worldX, worldY));
    this.world.theCanvas = this.theCanvas;
    this.world.draw();
  }
  @Override
  public void keyTyped(KeyEvent e) {
    
  }

  @Override
  public void keyPressed(KeyEvent e) {
    this.world = this.world.onKeyEvent(keyEventName(e));
    this.world.theCanvas = this.theCanvas;
    this.world.draw();
  }

  @Override
  public void keyReleased(KeyEvent e) {
  }

  private static String keyEventName(KeyEvent e) {
    String name = keyCodeName(e.getKeyCode());
    return name != null ? name : "" + e.getKeyChar();
  }

  private static String keyCodeName(int keycode) {
    return 0 <= keycode && keycode < keyNames.length ? keyNames[keycode] : null;
  }

  static String[] keyNames = new String[Character.MAX_VALUE];
  static {
    keyNames[KeyEvent.VK_ACCEPT] = "ACCEPT";
    keyNames[KeyEvent.VK_ADD] = "ADD";
    keyNames[KeyEvent.VK_AGAIN] = "AGAIN";
    keyNames[KeyEvent.VK_ALL_CANDIDATES] = "ALL_CANDIDATES";
    keyNames[KeyEvent.VK_ALPHANUMERIC] = "ALPHANUMERIC";
    keyNames[KeyEvent.VK_ALT] = "ALT";
    keyNames[KeyEvent.VK_ALT_GRAPH] = "ALT_GRAPH";
    keyNames[KeyEvent.VK_AMPERSAND] = "AMPERSAND";
    keyNames[KeyEvent.VK_ASTERISK] = "ASTERISK";
    keyNames[KeyEvent.VK_AT] = "AT";
    keyNames[KeyEvent.VK_BACK_QUOTE] = "BACK_QUOTE";
    keyNames[KeyEvent.VK_BACK_SLASH] = "BACK_SLASH";
    keyNames[KeyEvent.VK_BACK_SPACE] = "BACK_SPACE";
    keyNames[KeyEvent.VK_BEGIN] = "BEGIN";
    keyNames[KeyEvent.VK_BRACELEFT] = "BRACELEFT";
    keyNames[KeyEvent.VK_BRACERIGHT] = "BRACERIGHT";
    keyNames[KeyEvent.VK_CANCEL] = "CANCEL";
    keyNames[KeyEvent.VK_CAPS_LOCK] = "CAPS_LOCK";
    keyNames[KeyEvent.VK_CIRCUMFLEX] = "CIRCUMFLEX";
    keyNames[KeyEvent.VK_CLEAR] = "CLEAR";
    keyNames[KeyEvent.VK_CLOSE_BRACKET] = "CLOSE_BRACKET";
    keyNames[KeyEvent.VK_CODE_INPUT] = "CODE_INPUT";
    keyNames[KeyEvent.VK_COLON] = "COLON";
    keyNames[KeyEvent.VK_COMMA] = "COMMA";
    keyNames[KeyEvent.VK_COMPOSE] = "COMPOSE";
    keyNames[KeyEvent.VK_CONTEXT_MENU] = "CONTEXT_MENU";
    keyNames[KeyEvent.VK_CONTROL] = "CONTROL";
    keyNames[KeyEvent.VK_CONVERT] = "CONVERT";
    keyNames[KeyEvent.VK_COPY] = "COPY";
    keyNames[KeyEvent.VK_CUT] = "CUT";
    keyNames[KeyEvent.VK_DEAD_ABOVEDOT] = "DEAD_ABOVEDOT";
    keyNames[KeyEvent.VK_DEAD_ABOVERING] = "DEAD_ABOVERING";
    keyNames[KeyEvent.VK_DEAD_ACUTE] = "DEAD_ACUTE";
    keyNames[KeyEvent.VK_DEAD_BREVE] = "DEAD_BREVE";
    keyNames[KeyEvent.VK_DEAD_CARON] = "DEAD_CARON";
    keyNames[KeyEvent.VK_DEAD_CEDILLA] = "DEAD_CEDILLA";
    keyNames[KeyEvent.VK_DEAD_CIRCUMFLEX] = "DEAD_CIRCUMFLEX";
    keyNames[KeyEvent.VK_DEAD_DIAERESIS] = "DEAD_DIAERESIS";
    keyNames[KeyEvent.VK_DEAD_DOUBLEACUTE] = "DEAD_DOUBLEACUTE";
    keyNames[KeyEvent.VK_DEAD_GRAVE] = "DEAD_GRAVE";
    keyNames[KeyEvent.VK_DEAD_IOTA] = "DEAD_IOTA";
    keyNames[KeyEvent.VK_DEAD_MACRON] = "DEAD_MACRON";
    keyNames[KeyEvent.VK_DEAD_OGONEK] = "DEAD_OGONEK";
    keyNames[KeyEvent.VK_DEAD_SEMIVOICED_SOUND] = "DEAD_SEMIVOICED_SOUND";
    keyNames[KeyEvent.VK_DEAD_TILDE] = "DEAD_TILDE";
    keyNames[KeyEvent.VK_DEAD_VOICED_SOUND] = "DEAD_VOICED_SOUND";
    keyNames[KeyEvent.VK_DECIMAL] = "DECIMAL";
    keyNames[KeyEvent.VK_DELETE] = "DELETE";
    keyNames[KeyEvent.VK_DIVIDE] = "DIVIDE";
    keyNames[KeyEvent.VK_DOLLAR] = "DOLLAR";
    keyNames[KeyEvent.VK_DOWN] = "DOWN";
    keyNames[KeyEvent.VK_END] = "END";
    keyNames[KeyEvent.VK_ENTER] = "ENTER";
    keyNames[KeyEvent.VK_EQUALS] = "EQUALS";
    keyNames[KeyEvent.VK_ESCAPE] = "ESCAPE";
    keyNames[KeyEvent.VK_EURO_SIGN] = "EURO_SIGN";
    keyNames[KeyEvent.VK_EXCLAMATION_MARK] = "EXCLAMATION_MARK";
    keyNames[KeyEvent.VK_F1] = "F1";
    keyNames[KeyEvent.VK_F10] = "F10";
    keyNames[KeyEvent.VK_F11] = "F11";
    keyNames[KeyEvent.VK_F12] = "F12";
    keyNames[KeyEvent.VK_F13] = "F13";
    keyNames[KeyEvent.VK_F14] = "F14";
    keyNames[KeyEvent.VK_F15] = "F15";
    keyNames[KeyEvent.VK_F16] = "F16";
    keyNames[KeyEvent.VK_F17] = "F17";
    keyNames[KeyEvent.VK_F18] = "F18";
    keyNames[KeyEvent.VK_F19] = "F19";
    keyNames[KeyEvent.VK_F2] = "F2";
    keyNames[KeyEvent.VK_F20] = "F20";
    keyNames[KeyEvent.VK_F21] = "F21";
    keyNames[KeyEvent.VK_F22] = "F22";
    keyNames[KeyEvent.VK_F23] = "F23";
    keyNames[KeyEvent.VK_F24] = "F24";
    keyNames[KeyEvent.VK_F3] = "F3";
    keyNames[KeyEvent.VK_F4] = "F4";
    keyNames[KeyEvent.VK_F5] = "F5";
    keyNames[KeyEvent.VK_F6] = "F6";
    keyNames[KeyEvent.VK_F7] = "F7";
    keyNames[KeyEvent.VK_F8] = "F8";
    keyNames[KeyEvent.VK_F9] = "F9";
    keyNames[KeyEvent.VK_FINAL] = "FINAL";
    keyNames[KeyEvent.VK_FIND] = "FIND";
    keyNames[KeyEvent.VK_FULL_WIDTH] = "FULL_WIDTH";
    keyNames[KeyEvent.VK_GREATER] = "GREATER";
    keyNames[KeyEvent.VK_HALF_WIDTH] = "HALF_WIDTH";
    keyNames[KeyEvent.VK_HELP] = "HELP";
    keyNames[KeyEvent.VK_HIRAGANA] = "HIRAGANA";
    keyNames[KeyEvent.VK_HOME] = "HOME";
    keyNames[KeyEvent.VK_INPUT_METHOD_ON_OFF] = "INPUT_METHOD_ON_OFF";
    keyNames[KeyEvent.VK_INSERT] = "INSERT";
    keyNames[KeyEvent.VK_INVERTED_EXCLAMATION_MARK] = "INVERTED_EXCLAMATION_MARK";
    keyNames[KeyEvent.VK_JAPANESE_HIRAGANA] = "JAPANESE_HIRAGANA";
    keyNames[KeyEvent.VK_JAPANESE_KATAKANA] = "JAPANESE_KATAKANA";
    keyNames[KeyEvent.VK_JAPANESE_ROMAN] = "JAPANESE_ROMAN";
    keyNames[KeyEvent.VK_KANA] = "KANA";
    keyNames[KeyEvent.VK_KANA_LOCK] = "KANA_LOCK";
    keyNames[KeyEvent.VK_KANJI] = "KANJI";
    keyNames[KeyEvent.VK_KATAKANA] = "KATAKANA";
    keyNames[KeyEvent.VK_KP_DOWN] = "KP_DOWN";
    keyNames[KeyEvent.VK_KP_LEFT] = "KP_LEFT";
    keyNames[KeyEvent.VK_KP_RIGHT] = "KP_RIGHT";
    keyNames[KeyEvent.VK_KP_UP] = "KP_UP";
    keyNames[KeyEvent.VK_LEFT] = "LEFT";
    keyNames[KeyEvent.VK_LEFT_PARENTHESIS] = "LEFT_PARENTHESIS";
    keyNames[KeyEvent.VK_LESS] = "LESS";
    keyNames[KeyEvent.VK_META] = "META";
    keyNames[KeyEvent.VK_MINUS] = "MINUS";
    keyNames[KeyEvent.VK_MODECHANGE] = "MODECHANGE";
    keyNames[KeyEvent.VK_MULTIPLY] = "MULTIPLY";
    keyNames[KeyEvent.VK_NONCONVERT] = "NONCONVERT";
    keyNames[KeyEvent.VK_NUM_LOCK] = "NUM_LOCK";
    keyNames[KeyEvent.VK_NUMBER_SIGN] = "NUMBER_SIGN";
    keyNames[KeyEvent.VK_NUMPAD0] = "NUMPAD0";
    keyNames[KeyEvent.VK_NUMPAD1] = "NUMPAD1";
    keyNames[KeyEvent.VK_NUMPAD2] = "NUMPAD2";
    keyNames[KeyEvent.VK_NUMPAD3] = "NUMPAD3";
    keyNames[KeyEvent.VK_NUMPAD4] = "NUMPAD4";
    keyNames[KeyEvent.VK_NUMPAD5] = "NUMPAD5";
    keyNames[KeyEvent.VK_NUMPAD6] = "NUMPAD6";
    keyNames[KeyEvent.VK_NUMPAD7] = "NUMPAD7";
    keyNames[KeyEvent.VK_NUMPAD8] = "NUMPAD8";
    keyNames[KeyEvent.VK_NUMPAD9] = "NUMPAD9";
    keyNames[KeyEvent.VK_OPEN_BRACKET] = "OPEN_BRACKET";
    keyNames[KeyEvent.VK_PAGE_DOWN] = "PAGE_DOWN";
    keyNames[KeyEvent.VK_PAGE_UP] = "PAGE_UP";
    keyNames[KeyEvent.VK_PASTE] = "PASTE";
    keyNames[KeyEvent.VK_PAUSE] = "PAUSE";
    keyNames[KeyEvent.VK_PERIOD] = "PERIOD";
    keyNames[KeyEvent.VK_PLUS] = "PLUS";
    keyNames[KeyEvent.VK_PREVIOUS_CANDIDATE] = "PREVIOUS_CANDIDATE";
    keyNames[KeyEvent.VK_PRINTSCREEN] = "PRINTSCREEN";
    keyNames[KeyEvent.VK_PROPS] = "PROPS";
    keyNames[KeyEvent.VK_QUOTE] = "QUOTE";
    keyNames[KeyEvent.VK_QUOTEDBL] = "QUOTEDBL";
    keyNames[KeyEvent.VK_RIGHT] = "RIGHT";
    keyNames[KeyEvent.VK_RIGHT_PARENTHESIS] = "RIGHT_PARENTHESIS";
    keyNames[KeyEvent.VK_ROMAN_CHARACTERS] = "ROMAN_CHARACTERS";
    keyNames[KeyEvent.VK_SCROLL_LOCK] = "SCROLL_LOCK";
    keyNames[KeyEvent.VK_SEMICOLON] = "SEMICOLON";
    keyNames[KeyEvent.VK_SEPARATER] = "SEPARATER";
    keyNames[KeyEvent.VK_SEPARATOR] = "SEPARATOR";
    keyNames[KeyEvent.VK_SHIFT] = "SHIFT";
    keyNames[KeyEvent.VK_SLASH] = "SLASH";
    keyNames[KeyEvent.VK_SPACE] = "SPACE";
    keyNames[KeyEvent.VK_STOP] = "STOP";
    keyNames[KeyEvent.VK_SUBTRACT] = "SUBTRACT";
    keyNames[KeyEvent.VK_TAB] = "TAB";
    keyNames[KeyEvent.VK_UNDEFINED] = "UNDEFINED";
    keyNames[KeyEvent.VK_UNDERSCORE] = "UNDERSCORE";
    keyNames[KeyEvent.VK_UNDO] = "UNDO";
    keyNames[KeyEvent.VK_UP] = "UP";
    keyNames[KeyEvent.VK_WINDOWS] = "WINDOWS";

  }
}
