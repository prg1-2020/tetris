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
  // 目的：落下中のテトロミノを 1 だけ下に 動かす(課題１)
/*
  def tick(): World = {
    var ((x: Int, y: Int), shapet) = piece
    TetrisWorld(((x,  y + 1), shapet), pile)
  }
  // 目的：テトロミノが画面の一番下に達したら、それ以上落下しない(課題４)
  def tick(): World = {
    var ((x: Int, y: Int), shapet) = piece
    if(y >= 10 - S.size(shapet)._1) TetrisWorld(piece, pile)
    else TetrisWorld(((x,  y + 1), shapet), pile)
  }
*/
  //テトロミノが下に移動できなくなったときに、適切な処理が行われる(課題７)
  def tick(): World ={
    val ((x: Int, y: Int), shapet) = piece
    val future = TetrisWorld(((x,  y + 1), shapet), pile)
    if(collision(TetrisWorld(piece, pile))) TetrisWorld(piece, pile)
    else if(collision(future))  TetrisWorld(A.newPiece(), eraseRows(S.combine(S.shiftSE(shapet, x, y), pile)))
    else if(y >= 10 - S.size(shapet)._1) TetrisWorld(piece, pile)
    else future

  }

  // 2, 5. keyEvent
/*
  // 目的：キー入力に従って世界を更新する(課題２)
  def keyEvent(key: String): World = {
    var ((x: Int, y: Int), shapet) = piece
    if(collision(TetrisWorld(piece, pile)) TetrisWorld(piece, pile)
    else key match{
      case "UP" => TetrisWorld(((x, y), S.rotate(shapet)), pile)
      case "LEFT" => TetrisWorld(((x - 1, y), shapet), pile)
      case "RIGHT" => TetrisWorld(((x + 1, y), shapet), pile)
      case _ => TetrisWorld(piece, pile)
    }
    
  }
*/
  // 目的：キー操作によって衝突が起きるなら、その操作を無視する(課題５)
  def keyEvent(key: String): World = {
    val ((x: Int, y: Int), shapet) = piece
    val future = key match{
      case "UP" => TetrisWorld(((x, y), S.rotate(shapet)), pile)
      case "LEFT" => TetrisWorld(((x - 1, y), shapet), pile)
      case "RIGHT" => TetrisWorld(((x + 1, y), shapet), pile)
      case _ => TetrisWorld(piece, pile)
    }

    if (collision(future) || collision(TetrisWorld(piece, pile))) TetrisWorld(((x, y), shapet), pile)
    else future
    
  }

  // 3. collision
  // 目的：受け取った世界で衝突が起きているかを判定する関数(課題３)
  def collision(world: TetrisWorld): Boolean = {
    var ((x: Int, y: Int), shapet) = world.piece
    if(x <  0 || x > 10 - S.size(shapet)._2 ||  y > 10 - S.size(shapet)._1 || S.overlap(S.shiftSE(shapet, x, y), pile)) true
    else false
  }

  // 6. eraseRows
  // 目的：pile を受け取ったら、揃った行を削除する
  def eraseRows(pile: S.Shape): S.Shape = {
    val cols = S.size(pile)._2
    //アキュムレータ： eraseNum(消すべき行の数、underの上に空行を加える時に使う), under(消さないべき行、画面下に寄せる)
    def eraseRowsAcc(pile: S.Shape, eraseNum: Int, under: S.Shape): S.Shape ={
      pile match {  
        case Nil => List.fill(eraseNum)(List.fill(cols)(Transparent)) ++ under
        case x :: xs
         => if (S.blockCount(List(x)) != cols) eraseRowsAcc(xs, eraseNum, under ++ List(x))
            else eraseRowsAcc(xs, eraseNum + 1, under)
      }
    }

    eraseRowsAcc(pile, 0, Nil)
  
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
