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
  // 目的：

  def duplicate[T](n: Int, a: T): List[T] = {
    if (n == 0) Nil
    else a :: duplicate(n - 1, a)
  }

  // 2. empty
  // 目的：

  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }

  // 3. size
  // 目的：

  def size[T](mat: List[List[T]]): (Int, Int) = {
    (mat.length, mat.map(_.length).foldRight(0)(max(_, _)))
  }

  // 4. blockCount
  // 目的：

  def blockCount(shape: Shape): Int = {
    shape.map(_.map((x) =>
      if (x != Transparent) 1
      else 0
    ).foldRight(0)(_ + _)).foldRight(0)(_ + _)
  }

  // 5. wellStructured
  // 目的：

  def wellStructured[T](mat: List[List[T]]): Boolean = {
    mat match {
      case Nil => false
      case h :: mat => {
        val l = h.length;
        if (l == 0) false
        else mat.map(
          _.length == l
        ).foldRight(true)(_ && _)
      }
    }
  }

  // 6. rotate
  // 目的：
  // 契約：

  def transpose[T](matrix: List[List[T]]): List[List[T]] = {
    def cons_all(head: List[T], tail: List[List[T]]): List[List[T]] = {
      head match {
        case Nil => tail
        case h :: hs => {
          tail match {
            case Nil => (h :: Nil) :: cons_all(hs, Nil)
            case t :: ts => (h :: t) :: cons_all(hs, ts)
          }
        }
      }
    }
    matrix match {
      case Nil => Nil
      case r :: rs => cons_all(r, transpose(rs))
    }
  }

  def reverse[T](list: List[T]): List[T] = {
    return list.reverse
    def help(list: List[T], rev: List[T]): List[T] = {
      list match {
        case Nil => rev
        case x :: xs => help(xs, x :: rev)
      }
    }
    help(list, Nil)
  }

  def g_rotate[T](matrix: List[List[T]]): List[List[T]] = {
    reverse(transpose(matrix))
  }

  def rotate(shape: Shape): Shape = {
    g_rotate(shape)
  }



  // 7. shiftSE
  // 目的：

  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    val (_, c) = size(shape)
    empty(y, x + c) ++ shape.map(duplicate(x, Transparent) ++ _)
  }

  // 8. shiftNW
  // 目的：

  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    val (_, c) = size(shape)
    shape.map(_ ++ duplicate(x, Transparent)) ++ empty(y, c + x)
  }

  // 9. padTo
  // 目的：
  // 契約：

  def padTo(shape: Shape, rows: Int, cols: Int): Shape = {
    val (r, c) = size(shape)
    shiftNW(shape, cols - c, rows - r)
  }

  // 10. overlap
  // 目的：

  def overlap(shape0: Shape, shape1: Shape): Boolean = {
    shape0.zip(shape1).map(
      {case (s0, s1) => s0.zip(s1).map(
        {case (s0, s1) => s0 != Transparent && s1 != Transparent}
      ).foldRight(false)(_ || _)}
    ).foldRight(false)(_ || _)
  }

  // 11. combine
  // 目的：
  // 契約：

  def combine(shape0: Shape, shape1: Shape): Shape = {
    val (r0, c0) = size(shape0)
    val (r1, c1) = size(shape1)
    val (r, c) = (max(r0, r1), max(c0, c1))
    val s0 = padTo(shape0, r, c)
    val s1 = padTo(shape1, r, c)
    s0.zip(s1).map(
      {case (s0, s1) => s0.zip(s1).map(
        {case (s0, s1) => if (s0 == Transparent) s1 else s0}
      )}
    )
  }

  def filled(r: Row): Boolean = {
    r.map(_ != Transparent).foldRight(true)(_ && _)
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

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  
}
