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
  // 目的：落下中のテトロミノを1だけ下に動かす
  /*
  //1
  def tick(): World = {
    val ((x,y),s)= piece
    TetrisWorld(((x,y+1),s), pile)
  }
  */
  /*
  //4
  def tick(): World = {
    val ((x,y),s)= piece
    val (r,c) = S.size(s)
    if (y+r >= 10 || S.overlap(s, pile)) TetrisWorld(piece, pile)
    else TetrisWorld(((x,y+1),s), pile)
  }
  */
  //6
  def tick(): World = {
    var ((x,y),s)= piece
    var (r,c) = S.size(s)
    if (y+r >= 10 || S.overlap(S.shiftSE(s,x,y+1), pile)){
      val npile = S.combine(S.shiftSE(s,x,y),pile)
      val ((xx,yy),ss) = A.newPiece()
      if (S.overlap(S.shiftSE(ss,xx,yy),eraseRows(npile))){
        endOfWorld("GameOver")
        TetrisWorld(piece,pile)
      }
      else TetrisWorld(((xx,yy),ss), eraseRows(npile))
    }
    else TetrisWorld(((x,y+1),s), pile)
  }
  
  

  // 2, 5. keyEvent
  // 目的：キー入力に従って世界を更新する
  /*
  //2
  def keyEvent(key: String): World = {
    val ((x,y),s) = piece
    key match{
      case "RIGHT" => TetrisWorld(((x+1,y),s), pile)
      case "LEFT" => TetrisWorld(((x-1,y),s), pile)
      case "UP" => TetrisWorld(((x,y), S.rotate(s)), pile)
      case _ => TetrisWorld(piece, pile)
    }
  }
  */
  //5
  def keyEvent(key: String): World = {
    var ((x,y),s) = piece
    val (r, c) = S.size(s)
    key match{
      case "RIGHT" => {
        x += 1
        if (collision(TetrisWorld(((x,y),s),pile))) x -= 1
        TetrisWorld(((x,y),s),pile)
      }
      case "LEFT" => {
        x -= 1
        if (collision(TetrisWorld(((x,y),s), pile))) x += 1
        TetrisWorld(((x,y),s),pile)
      }
      case "UP" => {
        if (collision(TetrisWorld(((x,y), S.rotate(s)), pile))) TetrisWorld(piece, pile)
        else TetrisWorld(((x,y), S.rotate(s)), pile)
      }
      case _ => {
        TetrisWorld(piece, pile)
      }
    }
  }
  

  // 3. collision
  // 目的：衝突が起きているか判断する
  def collision(world: TetrisWorld): Boolean = {
    val ((x,y),s) = world.piece
    val p = world.pile
    val (r,c) = S.size(s)
    (S.overlap(S.shiftSE(s,x,y), p)) || (x <= -1) || (x+c-1 >= 10) || (y+r-1 >= 10)
  }

  // 6. eraseRows
  // 目的：pile を受け取ったら、揃った行を削除する
  def eraseRows(pile: S.Shape): S.Shape = {
    var cnt = 0
    var np = Nil:S.Shape
    for(i <- 0 to 9){
      if (pile(i).forall((x:S.Block) => x!= Transparent))  cnt += 1
      else np = pile(i)::np
    }
    S.empty(cnt,10)++(np.reverse)
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
