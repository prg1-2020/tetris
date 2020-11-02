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
  // 目的： (課題1)pieceのyをインクリメントし，下に1ブロック下げる。
  //      (課題4)pieceの落下は画面一番下で停止するように。
  //      (課題7)pieceをpileへ堆積，行削除の判定
  def tick(): World = {
    //課題1
    /*
    val ((x, y), s) = piece
    S.show(s)
    TetrisWorld(((x, y+1), s), pile)
    */

    //課題4
    /*
    val ((x, y), s) = piece
    val (h, w) = S.size(s)
    if(y+h == A.WellHeight){
      TetrisWorld(piece, pile)
    }else{
      TetrisWorld(((x, y+1), s), pile)
    }
    */

    //課題7
    val ((x, y), s) = piece
    val (h, w) = S.size(s)
    val nextW = TetrisWorld(((x, y+1), s), pile)

    if(y+h == A.WellHeight || collision(nextW)){
      val newW = TetrisWorld(A.newPiece(), eraseRows(S.combine(S.shiftSE(s,x,y), pile)))
      if(collision(newW)){
        endOfWorld("Game Over")
      }
      newW
      
    }else{
      nextW
    }

  }

  // 2, 5. keyEvent
  // 目的：(課題2)矢印キー入力でpieceを移動。UPで反時計に90°回転
  //     (課題5)入力により衝突が発生するならば無視。
  def keyEvent(key: String): World = {
    //課題2
    /*
    var ((x, y), s) = piece

    key match{
      case "RIGHT" => x = x+1
      case "LEFT" =>  x = x-1
      case "UP" =>  s = S.rotate(s)
    }

    TetrisWorld(((x, y), s), pile)
    */
    
    //課題5
    var ((x, y), s) = piece
    
    key match{
      case "RIGHT" => x = x+1
      case "LEFT" =>  x = x-1
      case "UP" =>  s = S.rotate(s)
    }

    val nextW = TetrisWorld(((x, y), s), pile)
    
    if(collision((nextW))){
      TetrisWorld(piece, pile)
    }else{
      nextW
    }
    
  }

  // 3. collision
  // 目的：(課題3)pieceが画面外 or pileと衝突でTrue
  def collision(world: TetrisWorld): Boolean = {

    val ((x, y), s) = world.piece
    val (h, w) = S.size(s)
    val absS = S.shiftSE(s,x,y)

    x < 0 || A.WellWidth < x + w || S.overlap(absS, world.pile)
    
  }

  // 6. eraseRows
  // 目的： pileを受け取ったら揃った行を削除する。
  def eraseRows(pile: S.Shape): S.Shape = {

    //揃った行(けすべき) => false, 揃ってない行(けさないべき) => true
    def thisFilter[A](row:List[A]):Boolean = {
      row.map(x => if(x!=Transparent) false else true).foldLeft(false)(_||_)
    }
    def padToButOnlyToSouth(s: S.Shape, rows: Int): S.Shape = {
      val sr = s.length
      assert(rows >= sr)
      S.shiftSE(s, 0, rows-sr)
    }

    val newPile = pile.filter(thisFilter)
    val formattedPile = padToButOnlyToSouth(newPile, A.WellHeight)

    formattedPile
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
