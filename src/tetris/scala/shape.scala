/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project tetris と入力し、return を押す
4. run と入力し、return を押す
5. コンパイルが成功したら、tetris.ShapeTest を選択（2 と入力）し、return を押す
6. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package tetris

import scala.collection.immutable.Range
import scala.util.Random
import scala.math.max

import sdraw._

// テトロミノを操作するための関数
object ShapeLib {
  // 色とブロックの表現
  type ColorSymbol = Char

  val blockSymbols = List('I', 'J', 'T', 'O', 'Z', 'L', 'S')
  val blockColors = {
    val n = blockSymbols.length
    for (i <- Range(0, n)) yield (HSB(360f * i / n, 0.3f, 1))
  }
  val colorSymbols = blockSymbols ++ List('G', 'g')
  val colors = blockColors ++ List(DarkGray, LightGray)
  val Color2Sym = colors.zip(colorSymbols).toList

  val Sym2Color: List[(ColorSymbol, Color)] =
    Color2Sym.map(cn => (cn._2, cn._1))

  // テトロミノの表現
  type Block = Color
  type Row = List[Block]
  type Shape = List[Row]
  type ShapeSpec = List[String]

  // テトロミノの表示（テスト用）
  def show(shape: Shape): Unit = println(showShape(shape))

  def showShape(shape: Shape): String = shape.map(showRow).mkString("\n")

  def showRow(row: Row): String = row.map(showBlock).mkString

  def showBlock(block: Block): Char = {
    Color2Sym.find(_._1 == block) match {
      case Some((_, sym)) => sym
      case None => '.'
    }
  }

  // テトロミノの定義
  val shapeSpecs: List[ShapeSpec] =
    List(
      List("I", "I", "I", "I"),
      List(" J", " J", "JJ"),
      List("TTT", " T "),
      List("OO", "OO"),
      List("ZZ ", " ZZ"),
      List("L ", "L ", "LL"),
      List(" SS", "SS "))

  def make(spec: ShapeSpec): Shape = {

    def color(c: ColorSymbol): Color =
      Sym2Color.find(p => p._1.equals(c)) match {
        case Some((_, c)) => c
        case _ => Transparent
      }

    spec.map((row: String) => row.toList.map(color))
  }

  // 7種類のテトロミノが入ったリスト
  val allShapes: List[Shape] = shapeSpecs.map(make)
  val List(shapeI, shapeJ, shapeT, shapeO, shapeZ, shapeL, shapeS) = allShapes

  // 次のテトロミノの選択
  val r = new Random()

  def random(): Shape = allShapes(r.nextInt(allShapes.length))

  // 1. duplicate
  // 目的：整数 n と任意の型の値 a を受け取り、n 個の a からなるリストを作る
  // 定義
  def duplicate[T] (n : Int, a : T) : List[T] = {
    if (n == 0) Nil
    else a :: duplicate(n - 1, a)
  }



  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る
  // 定義
  def empty(rows : Int, cols : Int) : Shape = duplicate(rows, duplicate(cols, Transparent))


  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す
  // 定義
  def size(shape : Shape) : (Int, Int) = (shape.length, shape.foldLeft(0)((w, row) => max(w, row.length)))


  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す関数
  // 定義
  def blockCount(shape : Shape) : Int =
    shape.foldLeft(0)((cnt, row) => cnt + row.foldLeft(0)((cnt, block) => cnt + (if (block != Transparent) 1 else 0)))



  // 5. wellStructured
  // 目的：受け取った shape が "まっとう" であるかを判断する関数
  // 定義
  def wellStructured(shape : Shape) : Boolean = {
    if (shape == Nil) false
    else shape.map(_.length).foldLeft(shape(0).length)((l, c) => if (l == c) l else 0) != 0
  }


// 目的: shape1, shape2 を横に結合する
// 契約: 2 つとも Nil でなければ縦の長さが同じ
def combHorizShapes(shape1: Shape, shape2: Shape): Shape = {
  if (shape1 != Nil && shape2 != Nil) assert(shape1.length == shape2.length)
  
  // 各行を結合
  def combHorizShapesSub(row1: Row, row2: Row): Row = row1 ++ row2
  
  (shape1, shape2) match {
    case (Nil, _) => shape2
    case (_, Nil) => shape1
    case (row1 :: res1, row2 :: res2) => combHorizShapesSub(row1, row2) :: combHorizShapes(res1, res2)
  }
}


  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転させる
  // 契約：引数の shape はまっとうである
  // 定義
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape))
    
    // 1 行を回転して 1 列に変換する
    def rotateRow(row: Row) : Shape = row.foldRight[Shape](Nil)((block, sum) => sum ++ List(List(block)))
    
    shape.foldLeft[Shape](Nil)((sum, row) => combHorizShapes(sum, rotateRow(row)))
  }

  
  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらした shape を作成する
  // 定義
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    if (shape == Nil) empty(y, x)
    else empty(y, x + shape(0).length) ++ combHorizShapes(empty(shape.length, x), shape)
  }


  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらした shape を作成する
  // 定義
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    if (shape == Nil) empty(y, x)
    else combHorizShapes(shape, empty(shape.length, x)) ++ empty(y, x + shape(0).length)
  }


  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を作成する
  // 契約：rows, cols はそれぞれ shape の行数, 列数以上
  // 定義
  def padTo(shape: Shape, rows: Int, cols: Int): Shape = {
    assert(shape.length <= rows)
    assert(shape == Nil || shape(0).length <= cols)
    
    if (shape == Nil) empty(rows, cols)
    else shiftNW(shape, cols - shape(0).length, rows - shape.length)
  }


  // 10. overlap
  // 目的：2 つの shape が重なりを持つかを判断する
  // 定義
  def overlap(shape1: Shape, shape2: Shape): Boolean = {
    
    def overlapRow(row1: Row, row2: Row): Boolean = {
      (row1, row2) match {
        case (Nil, _) => false
        case (_, Nil) => false
        case (block1 :: res1, block2 :: res2) =>
          (block1 != Transparent && block2 != Transparent) || overlapRow(res1, res2)
      }
    }
    
    (shape1, shape2) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (row1 :: res1, row2 :: res2) =>
        overlapRow(row1, row2) || overlap(res1, res2)
    }
  }


  // 11. combine
  // 目的：2 つの shape を結合する関数(左上を合わせて結合する)
  // 契約：結合する shape は重なりを持たない
  // 定義
  def combine(shape1: Shape, shape2: Shape): Shape = {
    assert(overlap(shape1, shape2) == false)
    
    def combineSquare(shape1: Shape, shape2: Shape): Shape = {
      
      def combineRow(row1: Row, row2: Row): Row = {
        (row1, row2) match {
          case (Nil, _) => Nil
          case (_, Nil) => Nil
          case (Transparent :: res1, block2 :: res2) => block2 :: combineRow(res1, res2)
          case (block1 :: res1, Transparent :: res2) => block1 :: combineRow(res1, res2)
          case (_, _) => Nil
        }
      }
      
      (shape1, shape2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (row1 :: res1, row2 :: res2) => combineRow(row1, row2) :: combineSquare(res1, res2)
      }
    }
    
    val (h1, w1) = size(shape1)
    val (h2, w2) = size(shape2)
    combineSquare(padTo(shape1, max(h1, h2), max(w1, w2)), padTo(shape2, max(h1, h2), max(w1, w2)))
  }
}


// テスト
object ShapeTest extends App {
  import ShapeLib._

  // 関数を定義するたびに、コメント開始位置を後ろにずらす

  // 1. duplicate
  println("duplicate")
  println(duplicate(0, 42) == Nil)
  println(duplicate(1, true) == List(true))
  println(duplicate(3, "hi") == List("hi", "hi", "hi"))
  println(duplicate(2, null) == List(null, null))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(0, 0) == Nil)

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeL) == (3, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeL) == 4)


  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(shapeL) == true)
  println(wellStructured(List(List(Yellow, Yellow), Nil)) == false)


  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeT))

  // rotate が満たすべき性質のテスト
  // 左に 90 度回転しているかどうか
  println(rotate(List(List(Red, Blue), List(Yellow, Pink))) == List(List(Blue, Pink), List(Red, Yellow)))
  // (h, w) -> (w, h) かどうか, まっとうかどうか
  println(rotate(List(List(Red, Yellow, Yellow), List(Blue, Pink, Blue))) ==
    List(List(Yellow, Blue), List(Yellow, Pink), List(Red, Blue)))

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  println(shiftSE(List(List(Blue, Red), List(Yellow, Transparent)), 2, 1) ==
    List(List(Transparent, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Blue, Red),
         List(Transparent, Transparent, Yellow, Transparent))
  )
  show(shiftSE(shapeT, 2, 0))
  show(shiftSE(shapeL, 0, 1))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  println(shiftNW(List(List(Blue, Red), List(Yellow, Transparent)), 2, 1) ==
    List(List(Blue, Red, Transparent, Transparent),
         List(Yellow, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent)))
  show(shiftNW(shapeT, 2, 0))
  show(shiftNW(shapeL, 0, 1))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  println(padTo(List(List(Blue, Red), List(Yellow, Transparent)), 3, 4) ==
    List(List(Blue, Red, Transparent, Transparent),
         List(Yellow, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent)))
  show(padTo(shapeT, 3, 3))
  show(padTo(shapeL, 3, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(
    List(
      List(Transparent, Red)
    ),
    List(
      List(Transparent, Pink),
      List(Transparent, Transparent),
      List(Transparent)
    )
  ) == true)
  println(overlap(Nil, shapeT) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shiftNW(rotate(rotate(rotate(shapeL))), 1, 0),
               shiftSE(rotate(shapeL), 1, 0)))
}