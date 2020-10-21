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
  // 目的：時間の経過に応じて落下中のテトロミノを 1 だけ下に動かす
/*  def tick(): World = {
    val cur_x = piece._1._1
    val cur_y = piece._1._2
    if(collision(TetrisWorld(((cur_x, cur_y + 1), piece._2) , pile))) return this
    TetrisWorld(((cur_x, cur_y + 1), piece._2) , pile)
  }
*/
  def tick(): World = {
    val cur_x = piece._1._1
    val cur_y = piece._1._2
    if(collision(TetrisWorld(((cur_x, cur_y + 1), piece._2) , pile))) return this
    TetrisWorld(((cur_x, cur_y + 1), piece._2) , pile)
  }

  // 2, 5. keyEvent
  // 目的：キー入力に従って世界を更新する
/*  def keyEvent(key: String): World = {
    val cur_x = piece._1._1
    val cur_y = piece._1._2
    key match {
      case "RIGHT" | "l" => TetrisWorld(((cur_x + 1, cur_y),piece._2) , pile)
      case "LEFT" | "j" => TetrisWorld(((cur_x - 1, cur_y), piece._2) , pile)
      case "UP" => TetrisWorld(((cur_x, cur_y), S.rotate(piece._2)) , pile)
      case _ => TetrisWorld(piece, pile)
    }
  }
*/
  def keyEvent(key: String): World = {
    val cur_x = piece._1._1
    val cur_y = piece._1._2
    val nextWorld = key match {
      case "RIGHT" | "l" => TetrisWorld(((cur_x + 1, cur_y),piece._2) , pile)
      case "LEFT" | "j" => TetrisWorld(((cur_x - 1, cur_y), piece._2) , pile)
      case "UP" => TetrisWorld(((cur_x, cur_y), S.rotate(piece._2)) , pile)
      case _ => TetrisWorld(piece, pile)
    }
    if(collision(nextWorld)) {
      return this
    }

    nextWorld
  }


  // 3. collision
  // 目的：受け取った世界で衝突が起きているかを判定する
  def collision(world: TetrisWorld): Boolean = {
    val (piece, pile) = (world.piece, world.pile)
    val (lsy, lsx) = S.size(pile)
    val (sy, sx) = S.size(piece._2)
    val cur_x = piece._1._1
    val cur_y = piece._1._2
    if(cur_x < 0 || cur_x + sx > lsx || cur_y + sy > lsy) return true
    val cpiece = S.shiftSE(piece._2, cur_x, cur_y)
    if(S.overlap(cpiece, pile)) return true
    false
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
