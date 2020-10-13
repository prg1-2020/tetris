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
      case None           => '.'
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
      List(" SS", "SS ")
    )

  def make(spec: ShapeSpec): Shape = {

    def color(c: ColorSymbol): Color =
      Sym2Color.find(p => p._1.equals(c)) match {
        case Some((_, c)) => c
        case _            => Transparent
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
  // 目的： 整数nと、任意変数aを受け取り、長さnのaでできたリストを作成する
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n > 0) a :: duplicate(n - 1, a)
    else Nil
  }

  // 2. empty
  // 目的： rows行cols列の空のShapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }

  // 3. size
  // 目的： Shapeのサイズを(rows, cols)の形式で返す
  def size(sh: Shape): (Int, Int) = {
    (sh.length, sh.foldRight(0)((x, k) => max(x.length, k)))
  }

  // 4. blockCount
  // 目的： shapeの空でない部分を返す
  def blockCount(sh: Shape): Int = {
    sh.foldRight(0)((x, k) =>
      k + x.foldRight(0)((x1, k1) => if (x1 != Transparent) 1 + k1 else k1)
    )
  }

  // 5. wellStructured
  // 目的： shapeのcol, rowが一行以上, 各行の要素数が全て一致することを判定する
  def wellStructured(sh: Shape): Boolean = {
    val (rows, cols) = size(sh)
    rows > 0 && cols > 0 && sh.foldRight(true)((x, k) => k && cols == x.length)
  }

  // 6. rotate
  // 目的：shapeを反時計回りに90°回す
  // 契約：wellStructured が true
  def rotate(sh: Shape): Shape = {
    assert(wellStructured(sh))
    val (rows, cols) = size(sh)
    Range(0, cols).toList.map(i => {
      Range(0, rows).toList.map(j => {
        sh(j)(cols - i - 1) // できればインデックス使わずにやりたかったけど
      })
    })
  }

  // 7. shiftSE
  // 目的：shapeを右にx, 下にyずらす
  def shiftSE(sh: Shape, x: Int, y: Int): Shape = {
    val (rows, cols) = size(sh)
    (duplicate(y, duplicate(cols, Transparent)) ++ sh).map(i => {
      duplicate(x, Transparent) ++ i
    })
  }

  // 8. shiftNW
  // 目的：shapeを左にx,上にyずらす
  def shiftNW(sh: Shape, x: Int, y: Int): Shape = {
    val (rows, cols) = size(sh)
    (sh ++ duplicate(y, duplicate(cols, Transparent))).map(i => {
      i ++ duplicate(x, Transparent)
    })
  }

  // 9. padTo
  // 目的：shapeをrows行cols列に拡張する
  // 契約：size(shape)のrows,cols以上になるrows,cols
  def padTo(sh: Shape, rows: Int, cols: Int): Shape = {
    val (s_rows, s_cols) = size(sh)
    assert(cols >= s_cols && rows >= s_rows)
    shiftNW(sh, cols - s_cols, rows - s_rows)
  }

  // 10. overlap
  // 目的：shapeが重なっていたらtrue
  def overlap(sh1: Shape, sh2: Shape): Boolean = {
    def acc(lis1: Row, lis2: Row): Boolean = {
      (lis1, lis2) match {
        case (x::xs, y::ys) => (x != Transparent && y != Transparent) || acc(xs, ys)
        case _ => false
      }
    }
    val (rows1, cols1) = size(sh1)
    val (rows2, cols2) = size(sh2)
    (sh1, sh2) match {
      case (x::xs, y::ys) => acc(x, y) || overlap(xs, ys)
      case _ => false
    }
  }

  // 11. combine
  // 目的：shapeを結合する
  // 契約：shapeは重なりを持たない
  def combine(sh1: Shape, sh2: Shape): Shape = {
    assert(!overlap(sh1, sh2))
    def acc(lis1: Row, lis2: Row): Row = {
      (lis1, lis2) match {
        case (x::xs, y::ys) => ( if(x != Transparent) x else y ) :: acc(xs, ys)
        case _ => Nil
      }
    }
    val (rows1, cols1) = size(sh1)
    val (rows2, cols2) = size(sh2)
    val new1 = padTo(sh1, max(rows1, rows2), max(cols1, cols2))
    val new2 = padTo(sh2, max(rows1, rows2), max(cols1, cols2))
    (new1, new2) match {
      case (x::xs, y::ys) => acc(x, y) :: combine(xs, ys)
      case _ => Nil
    }
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
  println(
    duplicate(2, duplicate(3, Nil)) == List(
      List(Nil, Nil, Nil),
      List(Nil, Nil, Nil)
    )
  )

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(
    empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent))
  )
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(
    empty(4, 5) == List(
      List(Transparent, Transparent, Transparent, Transparent, Transparent),
      List(Transparent, Transparent, Transparent, Transparent, Transparent),
      List(Transparent, Transparent, Transparent, Transparent, Transparent),
      List(Transparent, Transparent, Transparent, Transparent, Transparent)
    )
  )

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(empty(5, 4)) == (5, 4))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(make(List("I  ", "II ", "III"))) == 6)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(
    wellStructured(
      List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))
    ) == true
  )
  println(
    wellStructured(
      List(List(Red, Red), List(Yellow, Yellow), List(Blue))
    ) == false
  )
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red), Nil)) == false)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeT))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(shapeJ)) == shapeJ.map(_.reverse).reverse)
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeJ, 2, 3))
  println(shiftSE(List(List(Blue, Transparent)), 0, 3) == 
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Blue, Transparent)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeJ, 2, 3))
  println(shiftNW(List(List(Blue, Transparent)), 0, 3) == 
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  println(padTo(List(List(Blue, Transparent)), 3, 3) == 
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeT, shiftSE(shapeI, 0, 1)) == false)
  println(overlap(shapeT, shiftSE(shapeI, 1, 1)) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shapeT, shiftSE(rotate(shapeZ), 1, 1)))
  println(combine(List(List(Red), List(Red)),
                  List(List(Transparent, Transparent), List(Transparent, Blue))) == 
    List(List(Red, Transparent), List(Red, Blue)))

}
