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
import java.beans.Transient

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
  // 目的：整数nと任意型aを受け取ってn個のaからなるリストを作る
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n <= 0) Nil
    else a :: duplicate[A](n - 1, a)
  }

  // 2. empty
  // 目的；空のShapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }

  // 3. size
  // 目的：行数と列数を返す
  def size(shape: Shape): (Int, Int) = {
    val rowSize = shape.length
    val columnSize =
      if (rowSize == 0) 0 else shape.map(_.length).foldLeft(-1)(max(_, _))
    (rowSize, columnSize)
  }

  // 4. blockCount
  // 目的：空でないブロックの数を返す
  def blockCount(shape: Shape): Int = {
    if (shape.length == 0) 0
    else
      shape
        .map(_.map(z => if (z == Transparent) 0 else 1).foldLeft(0)(_ + _))
        .foldLeft(0)(_ + _)
  }

  // 5. wellStructured
  // 目的：Shapeが真っ当であるか判定する
  def wellStructured(shape: Shape): Boolean = {
    val rowSize = shape.length
    val columnSize =
      if (rowSize == 0) 0 else shape.map(_.length).foldLeft(-1)(max(_, _))
    if (rowSize == 0 || columnSize == 0) false
    else shape.map(_.length == shape.head.length).foldLeft(true)(_ && _)
  }

  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す関数rotateを定義する。
  // 契約：wellstructuredである
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape))
    def rotSub(shape: Shape): Shape = {
      if (!wellStructured(shape)) Nil
      else
        shape.map(_.head).foldRight(Nil: Row)(_ :: _) :: rotSub(
          shape.map(_.tail).foldRight(Nil: Shape)(_ :: _)
        )
    }
    rotSub(shape.map(_.reverse).foldRight(Nil: Shape)(_ :: _))
  }

  // 7. shiftSE
  // 目的：受け取ったshapeをx,yだけずらしたshapeを返す
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    empty(y, size(shape)._2 + x) ++ shape.foldRight(Nil: Shape)((a, b) =>
      (duplicate(x, Transparent) ++ a) :: b
    )
  }

  // 8. shiftNW
  // 目的：受け取ったshapeをx,yだけずらしたshapeを返す
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    shape.foldRight(Nil: Shape)((a, b) =>
      (a ++ duplicate(x, Transparent)) :: b
    ) ++ empty(y, size(shape)._2 + x)
  }

  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す関数padToを定義せよ。
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape: Shape, rows: Int, cols: Int): Shape = {
    assert(size(shape)._1 <= rows && size(shape)._2 <= cols)

    shiftNW(
      shape,
      cols - size(shape)._2,
      rows - size(shape)._1
    )
  }

  // 10. overlap
  // 目的：二つのshapeが重なりを持つかを判断する関数overlapを定義せよ。
  def overlap(shape1: Shape, shape2: Shape): Boolean = {
    shape1
      .zip(shape2)
      .foldLeft(false)((a, b) =>
        a || b._1
          .zip(b._2)
          .foldLeft(false)((p, q) =>
            p || (q._1 != Transparent && q._2 != Transparent)
          )
      )
  }

  // 11. combine
  // 目的：二つのshapeを結合する関数combineを定義する
  // 契約：引数のshapeは重なりをもたない
  def combine(shape1: Shape, shape2: Shape): Shape = {
    assert(overlap(shape1, shape2) == false)
    val s1 = size(shape1)
    val s2 = size(shape2)
    val pad1 = padTo(shape1, max(s1._1, s2._1), max(s1._2, s2._2))
    val pad2 = padTo(shape2, max(s1._1, s2._1), max(s1._2, s2._2))

    pad1
      .zip(pad2)
      .foldRight(Nil: Shape)((a, b) =>
        a._1
          .zip(a._2)
          .foldRight(Nil: Row)((p, q) =>
            (if (p._1 != Transparent) p._1 else p._2) :: q
          ) :: b
      )
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
  println(duplicate(4, "hoge") == List("hoge", "hoge", "hoge", "hoge"))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(
    empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent))
  )
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 1) == List(List(Transparent), List(Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeJ) == (3, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeJ) == 4)

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
  println(wellStructured(shapeJ) == true)
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  println(rotate(rotate(rotate(rotate(shapeI)))) == shapeI)
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)
  println(rotate(rotate(rotate(rotate(shapeL)))) == shapeL)
  println(rotate(rotate(rotate(rotate(shapeO)))) == shapeO)
  println(rotate(rotate(rotate(rotate(shapeT)))) == shapeT)
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  println(rotate(rotate(rotate(rotate(shapeZ)))) == shapeZ)
  println(blockCount(rotate(shapeL)) == blockCount(shapeL))
  println(
    size(rotate(shapeL))._1 == size(shapeL)._2 && size(
      rotate(shapeL)
    )._2 == size(shapeL)._1
  )

  // rotate が満たすべき性質のテスト
// ブロックカウントが維持される
// 4回作用させると元に戻る
// rotateごとにsizeが逆になる

  // 7. shiftSE
  println("shiftSE")
  println(
    shiftSE(List(List(Blue)), 1, 2) ==
      List(
        List(Transparent, Transparent),
        List(Transparent, Transparent),
        List(Transparent, Blue)
      )
  )
  println(
    shiftSE(List(List(Red)), 3, 2) ==
      List(
        List(Transparent, Transparent, Transparent, Transparent),
        List(Transparent, Transparent, Transparent, Transparent),
        List(Transparent, Transparent, Transparent, Red)
      )
  )
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(
    shiftNW(List(List(Blue)), 1, 2) ==
      List(
        List(Blue, Transparent),
        List(Transparent, Transparent),
        List(Transparent, Transparent)
      )
  )
  println(
    shiftNW(List(List(Red)), 3, 2) ==
      List(
        List(Red, Transparent, Transparent, Transparent),
        List(Transparent, Transparent, Transparent, Transparent),
        List(Transparent, Transparent, Transparent, Transparent)
      )
  )
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(
    padTo(List(List(Blue)), 2, 3) ==
      List(
        List(Blue, Transparent, Transparent),
        List(Transparent, Transparent, Transparent)
      )
  )
  println(
    padTo(List(List(Blue)), 3, 3) ==
      List(
        List(Blue, Transparent, Transparent),
        List(Transparent, Transparent, Transparent),
        List(Transparent, Transparent, Transparent)
      )
  )
  show(padTo(shapeI, 6, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeT, shiftSE(shapeL, 1, 1)) == true)
  // 11. combine
  println("combine")
  println(
    combine(
      List(List(Red), List(Transparent)),
      List(List(Transparent), List(Blue))
    ) ==
      List(List(Red), List(Blue))
  )
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shapeI, shiftSE(shapeZ, 1, 1)))
}
