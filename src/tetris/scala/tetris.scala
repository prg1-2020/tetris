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

/*
  // 1. tick
  // 目的：
  def tick(): World = {
    val ((x, y), shape) = piece
    val new_piece = ((x, y + 1), shape)
    TetrisWorld(new_piece, pile)
  }

  // 2. keyEvent
  // 目的：
  def keyEvent(key: String): World = {
    val ((x, y), shape) = piece
    key match {
      case "RIGHT" => TetrisWorld(((x + 1, y), shape), pile)
      case "LEFT" => TetrisWorld(((x - 1, y), shape), pile)
      case "UP" => TetrisWorld(((x + 1, y), S.rotate(shape)), pile)
      case _ => TetrisWorld(piece, pile)
    }
  }
*/

  // 3. collision
  // 目的： 衝突が起きているか判定する
  def collision(world: TetrisWorld): Boolean = {
    val ((x, y), s) = world.piece
    val (r, c) = S.size(s)
    (x < 0) || (x + c > A.WellWidth) || (y + r > A.WellHeight) || (S.overlap(S.shiftSE(s, x, y), world.pile))
  }
/*
  // 4. tick
  // 目的：
  def tick(): World = {
    val ((x, y), s) = piece
    val new_World = TetrisWorld(((x, y + 1), s), pile)
    if (collision(new_World)) TetrisWorld(piece, pile)
    else new_World
  }
*/

  // 5. keyEvent
  // 目的: 衝突が起きるならその操作を無視する
  def keyEvent(key: String): World = {
    val ((x, y), s) = piece
    val newWorld = (key match {
      case "RIGHT" => TetrisWorld(((x + 1, y), s), pile)
      case "LEFT" => TetrisWorld(((x - 1, y), s), pile)
      case "UP" => TetrisWorld(((x, y), S.rotate(s)), pile)
      case "DOWN" => TetrisWorld(((x, y + 1), s), pile)
      case _ => TetrisWorld(piece, pile)
    })
    if (collision(newWorld)) TetrisWorld(piece, pile)
    else newWorld
  }

  // 6. eraseRows
  // 目的： そろった行を消す
  def eraseRows(pile: S.Shape): S.Shape = {
    def rowcheck(row: S.Row): Boolean = {
      row.contains(Transparent) || row.length != A.WellWidth
    }
    S.empty(A.WellHeight - pile.count(rowcheck), A.WellWidth) ++ pile.filter(rowcheck)
  }

  // 7. tick
  // 目的: テトロミノが下に移動できなくなった時に適切な処理をするようにする
  def tick(): World = {
    val ((x,y),shape) = piece
    val newWorld = TetrisWorld(((x,y+1),shape), pile)
    if (collision(newWorld)){
      val z = TetrisWorld(A.newPiece(),eraseRows(S.combine(S.shiftSE(shape,x,y),pile)))
      if (collision(z)) z
      else z
    }
    else newWorld
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
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 0.2)
}
