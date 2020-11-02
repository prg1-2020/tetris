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
  /*1のtick
  def tick(): World = {
    val ((x,y),s) = piece
    TetrisWorld(((x,y+1),s),pile)
  }
  */
  /*4のtick
  def tick(): World = {
    val ((x,y),s) = piece
    val wd = TetrisWorld(((x,y+1),s),pile)
    if (collision(wd)) TetrisWorld(piece,pile) else wd
  }
  */
  def tick(): World = {
    val ((x,y),s) = piece
    if(s == List(List(Transparent))) return this
    if(collision(TetrisWorld(((x,y+1),s),pile))) {
      val bigpiece = S.shiftSE(s,x,y)
      val nextPile = eraseRows(S.combine(pile, bigpiece))
      val nextWorld = TetrisWorld(A.newPiece(), nextPile)
      if(collision(nextWorld)){
        println("Game Over")
        return TetrisWorld(((0, 0), List(List(Transparent))), nextPile)
      }
      else return nextWorld
    }
    TetrisWorld(((x,y+1),s),pile)
  }
  // 2, 5. keyEvent
  // 目的：
  /*
  def keyEvent(key: String): World = {
    val ((x,y),s) = piece
    key match{
      case "RIGHT" => TetrisWorld(((x+1,y),s),pile)
      case "LEFT" => TetrisWorld(((x-1,y),s),pile)
      case "UP" => TetrisWorld(((x,y),S.rotate(s)),pile)
    }
  }
  */
  def keyEvent(key: String):World = {
    val ((x,y),s) = piece
    val nextpiece = key match{
      case "RIGHT" => ((x+1,y),s)
      case "LEFT" => ((x-1,y),s)
      case "UP" => ((x,y),S.rotate(s))
    }
    if (collision(TetrisWorld(nextpiece,pile))) TetrisWorld(piece,pile) else TetrisWorld(nextpiece,pile)
  }
  
  // 3. collision
  // 目的：
  def collision(world: TetrisWorld): Boolean = {
    val ((x,y),s) = piece
    val (m,n) = S.size(s)
    x<0 || A.WellWidth<x+n || A.WellHeight<=y+m || S.overlap(S.shiftSE(s,x,y),pile)
  }

  // 6. eraseRows
  // 目的：
  def eraseRows(pile: S.Shape): S.Shape = {
    def denseRow(row: S.Row): Boolean = {
      row.foldLeft(true)((p, block) => p && (block != Transparent))
    }
    val condensedPile = pile.foldRight(List[List[S.Block]]())((row, nextPile) => if(denseRow(row)) nextPile else row::nextPile)
    List.fill(A.WellHeight - condensedPile.length)(List.fill(A.WellWidth)(Transparent)) ++ condensedPile
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
