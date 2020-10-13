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
    rows > 0 && cols > 0 && sh.foldRight(true)((x, k) => 
      k && cols == x.length
    )
  }

  // 6. rotate
  // 目的：shapeを反時計回りに90°回す
  // 契約：wellStructured が true
  def rotate(sh: Shape): Shape = {
    assert(wellStructured(sh))
    val (rows, cols) = size(sh)
    Range(0, cols).toList.map(i => {
      Range(0, rows).toList.map(j => {
        sh(j)(cols-i-1)
      })
    })
  }

  // 7. shiftSE
  // 目的：

  // 8. shiftNW
  // 目的：

  // 9. padTo
  // 目的：
  // 契約：

  // 10. overlap
  // 目的：

  // 11. combine
  // 目的：
  // 契約：

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
  println(empty(4, 5) == List(
    List(Transparent, Transparent, Transparent, Transparent, Transparent),
    List(Transparent, Transparent, Transparent, Transparent, Transparent),
    List(Transparent, Transparent, Transparent, Transparent, Transparent),
    List(Transparent, Transparent, Transparent, Transparent, Transparent)
  ))

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
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
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

  // 7. shiftSE
  // println("shiftSE")
  // println(shiftSE(List(List(Blue)), 1, 2) ==
  //   List(List(Transparent, Transparent),
  //        List(Transparent, Transparent),
  //        List(Transparent, Blue)))
  // show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  // println("shiftNW")
  // println(shiftNW(List(List(Blue)), 1, 2) ==
  //   List(List(Blue, Transparent),
  //        List(Transparent, Transparent),
  //        List(Transparent, Transparent)))
  // show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  // println("padTo")
  // println(padTo(List(List(Blue)), 2, 3) ==
  //   List(List(Blue, Transparent, Transparent),
  //        List(Transparent, Transparent, Transparent)))
  // show(padTo(shapeI, 6, 2))

  // 10. overlap
  // println("overlap")
  // println(overlap(shapeI, shapeZ) == true)
  // println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)

  // 11. combine
  // println("combine")
  // println(combine(List(List(Red), List(Transparent)),
  //                 List(List(Transparent), List(Blue))) ==
  //   List(List(Red), List(Blue)))
  // show(combine(shiftSE(shapeI, 0, 1), shapeZ)

}
