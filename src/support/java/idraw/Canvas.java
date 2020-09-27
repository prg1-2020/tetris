package idraw;

import geometry.Posn;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import colors.IColor;

// to represent a drawable area on a screen (imperative version)
public class Canvas {
  private int width = 0;
  private int height = 0;
  private JFrame frame;
  private BufferedImage buffer;
  private JPanel panel;
  private String title;

  public Canvas(int width, int height, String title) {
    this.width = width;
    this.height = height;
    this.title = title;
  }
  public Canvas(int width, int height) {
    this(width, height, "Canvas");
  }

  //to close the window that is displaying this canvas
  public void close() {
    getFrame().setVisible(false);
    return;
  }

  //to draw a circumference of a circle of the given radius at the given center with the given color 
  public void drawCircle(Posn center, int radius, IColor color) {
    Graphics2D g = getG();
    g.setColor(color.getColor());
    g.drawOval(center.x - radius, center.y - radius, radius + radius, radius + radius);
    return ;
  }

  //to draw a filled circle of the given radius at the given center with the given color 
  public void drawDisk(Posn center, int radius, IColor color) {
    Graphics2D g = getG();
    g.setColor(color.getColor());
    g.fillOval(center.x - radius, center.y - radius, radius + radius, radius + radius);
    update();
    return ;
  }

  //to draw a filled oval of the given radius at the given center with the given color 
    public boolean drawOval(Posn center, int radiusx, int radiusy, IColor color) {
    Graphics g = getG();
    g.setColor(color.getColor());
    g.fillOval(center.x - radiusx, center.y - radiusy, radiusx + radiusx, radiusy + radiusy);
    update();
    return true;
  }

  //to draw a line from the start to end with the given color
  public void drawLine(Posn start, Posn end, IColor color) {
    Graphics2D g = getG();
    g.setColor(color.getColor());
    g.drawLine(start.x, start.y, end.x, end.y);
    update();
    return ;
  }

  //to draw a filled rectangle whose top-left corner is given as the topLeft,
  //with the given width, height and color.
  public void drawRect(Posn topLeft, int width, int height, IColor color) {
    Graphics2D g = getG();
    g.setColor(color.getColor());
    g.fillRect(topLeft.x, topLeft.y, width, height);
    update();
    return ;
  }

  //to draw a message at p
  public void drawString(Posn p, String message) {
    // getG().setColor(c.getColor());
    Graphics2D g = getG();
    g.setColor(Color.black);
    g.drawString(message, p.x, p.y);
    update();
    return;
  }

  //to obtain an off-screen image buffer of this canvas
  private Image getBuffer() {
    if (buffer == null) {
      buffer = new BufferedImage(this.width, this.height,
          BufferedImage.TYPE_INT_RGB);
      Graphics2D g = (Graphics2D)buffer.getGraphics();
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

      g.setColor(Color.white);
      g.fillRect(0, 0, buffer.getWidth(), buffer.getHeight());
    }
    return buffer;
  }

  //to construct a window that displays this canvas
  private JFrame getFrame() {
    if (frame == null) {
      frame = new JFrame(this.title);
      frame.setSize(this.width, this.height);
      panel = new JPanel() {

        @Override
        public void paint(Graphics g) {
          Image img = getBuffer();
          g.drawImage(img, 0, 0, panel.getWidth(), panel.getHeight(), 0, 0,
              img.getWidth(null), img.getHeight(null), null);
        }

      };
      panel.setPreferredSize(new Dimension(this.width, this.height));
      // p.setBackground(Color.white);
      frame.getContentPane().add(panel);
      frame.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
//      frame.setLocationByPlatform(true);
      frame.setLocation(getNextFrameLocation());
      frame.pack();
    }
    return frame;
  }

  //the location where the next frame should be located
  private static Point nextFrameLocation = new Point(0,0);
  private static final int offsetFromPreviousFrame = 10;
  //to get the location for the next frame
  private static Point getNextFrameLocation() {
    Point n = nextFrameLocation;
    nextFrameLocation = new Point(n.x+offsetFromPreviousFrame, n.y+offsetFromPreviousFrame);
    return n;
  }

  //to obtain a graphics context for drawing something on this canavs
  private Graphics2D getG() {
    return (Graphics2D)getBuffer().getGraphics();
  }

  //to create a window and show this canvas in it.
  public void show() {
    getFrame().setVisible(true);
    getFrame().repaint();
    return ;
  }

  //to show a message in a dialog window and wait until the button is pressed
  public boolean pause(String s) {
    return this.dialog(s) == 0;
  }

  //to show a message in a dialog window and wait until the button is pressed
  public boolean pause() {
    return this.pause("Continue?");
  }

  private void update() {
    getFrame().repaint();
  }

  //to represent a dialog window for pausing
  class OKDialog extends JDialog {
    private BlockingQueue<Integer> queue = new LinkedBlockingQueue();
    OKDialog(String s) {
      super(Canvas.this.frame, "Pausing...", false);
      JLabel l = new JLabel(s);
      this.getContentPane().add(l, BorderLayout.NORTH);
      JButton b = new JButton("OK");
      b.addActionListener(new ActionListener() {

        @Override
        public void actionPerformed(ActionEvent e) {
          //called when OK button is pressed
          OKDialog.this.queue.add(0);
        }
      });
      this.addWindowListener(new WindowAdapter() {
        
        @Override
        public void windowClosing(WindowEvent e) {
          //called when close button of the dialog is pressed
          OKDialog.this.queue.add(-1);
        }
      });
      Canvas.this.frame.addWindowListener(new WindowAdapter() {

        @Override
        public void windowClosing(WindowEvent e) {
          //called when close button of the parent frame is pressed
          OKDialog.this.queue.add(-1);
        }
      });
      this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      this.getContentPane().add(b, BorderLayout.SOUTH);
      this.pack();
      this.setLocation(frame.getX()+frame.getWidth(), frame.getY());
      this.setVisible(true);
    }

    private int waitForClick() {
      try {
        return queue.take();
      } catch (InterruptedException e) {
        e.printStackTrace();
      } finally {
        this.dispose();
      }
      return -1;
    }

  }

  int dialog(String s) {
    return new OKDialog(s).waitForClick();
  }

  public void setTitle(String title) {
    getFrame().setTitle(title);
  }
  
  void addKeyListener(KeyListener l) {
    this.getFrame().addKeyListener(l);
  }
  void repaint() {
    this.getFrame().repaint();
  }
  void addWindowListener(WindowListener l) {
    this.getFrame().addWindowListener(l);
  }
  void addMouseListener(MouseListener l) {
    this.getFrame();
    this.panel.addMouseListener(l);
  }
  int getWidth() {
    this.getFrame();
    return this.panel.getWidth();
  }
  int getHeight() {
    this.getFrame();
    return this.panel.getHeight();
  }
}
