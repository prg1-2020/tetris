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
  def tick(): World = {
    /*
    // 課題１
    // 目的：テトロミノを１だけ下に動かす
    val ((x, y), shape) = piece
    TetrisWorld(((x, y+1), shape), pile)
    */

    // 課題４
    // 目的：テトロミノが一番下に達したらそれ以上落下しないようにする
    val ((x, y), shape) = piece
    if (y + 1 + shape.length > A.WellHeight) TetrisWorld(piece, pile)
    else TetrisWorld(((x, y+1), shape), pile)
    
    /*
    // 課題７
    // 目的：テトロミノが一番下に達したら適切な処理を行う
    val ((x, y), shape) = piece
    val world = TetrisWorld(((x, y+1), shape), pile)
    if (collision(world) == true) {
      //if (collision(TetrisWorld(A.newPiece(), pile)) == false) {
        TetrisWorld(A.newPiece(), S.shiftSE(shape, x, y) ++ pile)
      //}
      //else TetrisWorld(piece, pile)
      // endOfWorld("Game Over")
    }
    else world
    */

  }

  // 2, 5. keyEvent
  def keyEvent(key: String): World = {
    /*
    // 課題２
    // 目的：キー入力に従って世界を更新する
    val ((x, y), shape) = piece
    if(key == "RIGHT") TetrisWorld(((x+1, y), shape), pile)
    else if(key == "LEFT") TetrisWorld(((x-1, y), shape), pile)
    else if(key == "UP") TetrisWorld(((x, y), S.rotate(shape)), pile)
    else TetrisWorld(piece, pile)
    */

    // 課題５
    // 目的：衝突が起きるならその操作を無視する
    val ((x, y), shape) = piece
    val world = {
      if (key == "RIGHT") TetrisWorld(((x+1, y), shape), pile)
      else if (key == "LEFT") TetrisWorld(((x-1, y), shape), pile)
      else if (key == "UP") TetrisWorld(((x, y), S.rotate(shape)), pile)
      else TetrisWorld(piece, pile)
    }
    if (collision(world)) TetrisWorld(piece, pile)
    else world
  }

  // 3. collision
  // 目的：衝突が起きているか判定する
  def collision(world: TetrisWorld): Boolean = {
    val ((x, y), shape) = world.piece
    val (r, c) = S.size(shape) 
    if(x < 0 || x + c > A.WellWidth || y + r > A.WellHeight || S.overlap(S.shiftSE(shape, x, y), pile)) true
    else false
  }

  // 6. eraseRows
  // 目的：揃った行を削除する
  def eraseRows(pile: S.Shape): S.Shape = {
    pile
    /*
    pile match {
      case Nil => Nil
      case x :: xs => {
        if (S.blockCount(List(x)) == A.WellWidth) eraseRows(xs)
        else x :: eraseRows(xs)
      }
    }
    */
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
