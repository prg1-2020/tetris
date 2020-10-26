/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project tetris と入力し、return を押す
4. run と入力し、return を押す
5. コンパイルが成功したら、tetris.A を選択（1 と入力）し、return を押す
6. ゲーム画面を閉じたら、手動で java を終了する
7. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package tetris

import scala.util.Random

import sgeometry.Pos
import sdraw.{World, Color, Transparent, HSB}

import tetris.{ShapeLib => S}

// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape) extends World() {

  // マウスクリックは無視
  def click(p: sgeometry.Pos): World = this

  // ブロックの描画
  def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Boolean = {
    canvas.drawRect(Pos(A.BlockSize * x, A.BlockSize * y), A.BlockSize * w, A.BlockSize * h, c)
  }

  // shape の描画（与えられた位置）
  def drawShape(pos: (Int, Int), shape: S.Shape): Boolean = {
    val pos_colors = shape.zipWithIndex.flatMap(row_i => {
      val (row, i) = row_i
      row.zipWithIndex.map(box_j => {
        val (color, j) = box_j
        (j, i, color)
      })
    })

    val (x, y) = pos
    pos_colors.forall(pos_color => {
      val (dx, dy, color) = pos_color
      drawRect(x + dx, y + dy, 1, 1, color)
    })
  }

  // shape の描画（原点）
  def drawShape00(shape: S.Shape): Boolean = drawShape((0, 0), shape)

  // ゲーム画面の描画
  val CanvasColor = HSB(0, 0, 0.1f)

  def draw(): Boolean = {
    val (pos, shape) = piece
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    drawShape00(pile) &&
    drawShape(pos, shape)
  }

  // 1, 4, 7. tick
  // 目的：
  def tick(): World = {
    val ((x,y),shape) = piece
    /* kadai 1
    val new_piece = ((x,y+1),shape)
    */

    def make_new_y(y:Int,cols:Int):Int={
        if (y+cols >= 10) y
        else y+1
    }
    val new_piece = ((x,make_new_y(y,shape.length)),shape)
    TetrisWorld(new_piece,pile)
  }

  // 2, 5. keyEvent
  // 目的：
  def keyEvent(key: String): World = {
      val ((x,y),shape) = piece
       //kadai2
       /*
      key match{
          case "RIGHT" => TetrisWorld(((x+1,y),shape),pile)
          case "LEFT" => TetrisWorld(((x-1,y),shape),pile)
          case "UP" => TetrisWorld(((x,y),S.rotate(shape)),pile)
      }
      */
      

      def keyEvent_temp(key:String):TetrisWorld={
          key match{
            case "RIGHT" => TetrisWorld(((x+1,y),shape),pile)
            case "LEFT" => TetrisWorld(((x-1,y),shape),pile)
            case "UP" => TetrisWorld(((x,y),S.rotate(shape)),pile)
        }
    }
    val temp_world = keyEvent_temp(key)

    if (collision(keyEvent_temp(key))) TetrisWorld(((x,y),shape),pile)
    else keyEvent_temp(key)
    
  }

  
  // 3. collision
  // 目的：
  def collision(world: TetrisWorld): Boolean = {
      //bool(1~4) をifで判定
    val TetrisWorld(piece,pile) = world
    val ((x,y),shape) = piece
    val piece_locate = S.shiftSE(shape,x,y)
    val (rows,cols) = S.size(shape)
    //それぞれの条件(エラー存在＝＞true)
    S.overlap(S.shiftSE(shape,x,y),pile)
    val bool1 = S.overlap(pile,S.shiftSE(shape,x,y)) //被ってたらtrue
    val bool2 = x<0
    val bool3 = (cols + x) > A.WellWidth
    val bool4 = (rows + y) > A.WellHeight

    bool1 || bool2 || bool3 || bool4
  }


  // 6. eraseRows
  // 目的：
  def eraseRows(pile: S.Shape): S.Shape = {
    pile
  }
}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 10
  val BlockSize = 30

  // 新しいテトロミノの作成
  val r = new Random()

  def newPiece(): ((Int, Int), S.Shape) = {
    val pos = (WellWidth / 2 - 1, 0)
    (pos,
     List.fill(r.nextInt(4))(0).foldLeft(S.random())((shape, _) => shape))
  }

  // 最初のテトロミノ
  val piece = newPiece()

  // ゲームの初期値
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)))

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
}
