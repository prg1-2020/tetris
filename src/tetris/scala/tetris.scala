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
import sdraw.{World, Color, NoColor, Transparent, HSB}

import tetris.{ShapeLib => S}

// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape) extends World() {

  // マウスクリックは無視
  def click(p: sgeometry.Pos): World = this

  // ブロックの描画
  def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Boolean = {
    canvas.drawRect(Pos(A.BlockSize * x, A.BlockSize * y), A.BlockSize * w, A.BlockSize * h, c)
  }

  // shape の描画（与えられた位置）pos := (x, y)
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
  
  //// 自作関数群
  /**
   * piece をずらすための関数
   * 右側に dx, 下側に dy だけずらす
   */
  def movePiece(piece: ((Int, Int), S.Shape), dx: Int, dy: Int) = {
    val (x, y) = piece._1
    ((x + dx, y + dy), piece._2)
  }
  
  
  //////

  // 1, 4, 7. tick
  // 目的：時間の経過に応じて世界を更新する関数(操作中のテトリミノを 1 マスだけ下に落下させる)
  // // 1. 目的：下に落とすだけ
  // def tick(): World = {
  //   TetrisWorld(movePiece(piece, 0, 1), pile)
  // }
  // 4. 目的：一番下との判定を行う
  def tick(): World = {
    val next_world = TetrisWorld(movePiece(piece, 0, 1), pile)
    if (!collision(next_world)) next_world
    else this
  }

  // 2, 5. keyEvent
  // 目的：キー入力に従って世界を更新する
  // // 2. 目的: 画面外判定は行わない
  // def keyEvent(key: String): World = {
  //   val next_piece = key match {
  //     case "RIGHT" => movePiece(piece, 1, 0)
  //     case "LEFT" => movePiece(piece, -1, 0)
  //     case "UP" => (piece._1, S.rotate(piece._2))
  //     case _ => piece
  //   }
  //   TetrisWorld(next_piece, pile)
  // }
  // 5. 画面外なら操作を無視する
  def keyEvent(key: String): World = {
    val next_piece = key match {
      case "RIGHT" => movePiece(piece, 1, 0)
      case "LEFT" => movePiece(piece, -1, 0)
      case "UP" => (piece._1, S.rotate(piece._2))
      case _ => piece
    }
    val next_world = TetrisWorld(next_piece, pile)
    if (!collision(next_world)) next_world
    else this
  }
  
  // 3. collision
  // 目的：受け取った世界で衝突が起きているかを判定する
  def collision(world: TetrisWorld): Boolean = {
    val (pileH, pileW) = S.size(world.pile)
    val ((x, y), shape) = world.piece
    
    // 判定に用いる pile
    val j_pile =
      S.combHorizShapes(
        S.combHorizShapes(
          List.fill(pileH, 1)(NoColor),
          world.pile
        ),
        List.fill(pileH, 1)(NoColor)
      ) :+ List.fill(pileW + 2)(NoColor)
    
    S.overlap(
      S.shiftSE(shape, x + 1, y),
      j_pile
    )
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
    (pos, S.random())
  }

  // 最初のテトロミノ
  val piece = newPiece()

  // ゲームの初期値
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)))

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
}
