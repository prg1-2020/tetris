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
  // 目的：時間経過で落下させ、一番下まできたら落下をやめる
  def tick(): World = {
    val ((x,y),shape) = piece
    if(collision(TetrisWorld(((x,y+1),shape),pile))){
      TetrisWorld(A.newPiece(),eraseRows(S.combine(S.shiftSE(shape,x,y),pile)))
    }else{
      TetrisWorld(((x,y+1),shape),pile)
    }
  }

  //課題4で定義した関数
  /*
  def tick(): World = {
    val ((x,y),shape) = piece
    if(collision(TetrisWorld(((x,y+1),shape),pile))){
      TetrisWorld(piece,pile)
    }else{
      TetrisWorld(((x,y+1),shape),pile)
    }
  }
  */

  //課題1で定義した関数
  // 目的：時間経過で落下させる
  /*
  def tick(): World = {
    val ((x,y),shape) = piece
    TetrisWorld(((x,y+1),shape),pile)
  }
  */

  // 2, 5. keyEvent
  // 目的：キー入力によってworldを変更するが、衝突が生じる際は無視する
  def keyEvent(key: String): World = {
    val ((x,y),shape) = piece
    key match {
      case "RIGHT" => {
        if(collision(TetrisWorld(((x+1,y),shape),pile))){
          TetrisWorld(piece,pile)
        }else{
          TetrisWorld(((x+1,y),shape),pile)
        }
      }
      case "LEFT" => {
        if(collision(TetrisWorld(((x-1,y),shape),pile))){
          TetrisWorld(piece,pile)
        }else{
          TetrisWorld(((x-1,y),shape),pile)
        }
      }
      case "UP" => {
        if(collision(TetrisWorld(((x,y),S.rotate(shape)),pile))){
          TetrisWorld(piece,pile)
        }else{
          TetrisWorld(((x,y),S.rotate(shape)),pile)
        }
      }
      case _ => {
        TetrisWorld(piece, pile)
      }
    }
  }

  //課題２で定義した関数
  // 目的：キー入力によってworldを変更する
  /*
  def keyEvent(key: String): World = {
    val ((x,y),shape) = piece
    key match {
      case "RIGHT" => TetrisWorld(((x+1,y),shape),pile)
      case "LEFT" => TetrisWorld(((x-1,y),shape), pile)
      case "UP" => TetrisWorld(((x,y),S.rotate(shape)), pile)
      case _ => TetrisWorld(piece, pile)
    }
  }
  */


  // 3. collision
  // 目的：受け取ったworldで衝突が起きているか判定する
  def collision(world: TetrisWorld): Boolean = {
    val TetrisWorld(((x,y),shape),plie) = world
    val (s,t) = S.size(shape)
    if(x<0 | x+t > A.WellWidth | y<0 | y+s> A.WellHeight) true //world境界との衝突
    else S.overlap(S.shiftSE(shape,x,y),pile) //pileとの衝突
  }

  // 6. eraseRows
  // 目的：pileの揃った行を削除する
  def eraseRows(pile: S.Shape): S.Shape = {
    def erase(_pile: S.Shape): S.Shape = {
      _pile.foldRight(Nil: S.Shape)((r,rs) =>
        if(r.filter(_ == Transparent).length == 0) rs //空のブロックがなければその行を無視する
        else r::rs
      )
    }
    S.empty(A.WellHeight-erase(pile).length,A.WellWidth) ++ erase(pile)
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
