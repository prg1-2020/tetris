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
  // 目的：n 個の x (型は任意)からなるリストを作る
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n == 0) Nil
    else a :: duplicate(n-1, a)
  }

  // 2. empty
  // 目的：与えられた行数、列数の空のshapeを作る
  def empty(n: Int, m: Int): Shape = {
    if (n == 0) Nil
    else duplicate(m, Transparent) :: empty(n-1, m)
  }

  // 3. size
  // 目的：受け取ったshapeのサイズを(行数, 列数)で返す
  def maxCols(shape: Shape): Int = {
    shape match {
      case Nil => 0
      case x :: xs => max(x.length, maxCols(xs))
    }
  }
  def size(shape: Shape): (Int, Int) = {
   val rows = shape.length
   val cols = maxCols(shape)
   (rows, cols)
  }

  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックを数える
  def blockCount(shape: Shape): Int = {
    // 4-1. blockCountRow
    // 目的：受け取ったrowに含まれる空でないブロックを数える
    def blockCountRow(row: Row): Int = {
      row match{
        case Nil => 0
        case x :: xs =>
        if (x == Transparent) blockCountRow(xs)
        else 1 + blockCountRow(xs)
      }
    }
    shape match {
      case Nil => 0
      case x :: xs => blockCountRow(x) + blockCount(xs)
    }
  }

  // 5. wellStructured
  // 目的：受け取ったshapeの行数、列数が1以上であり各行の要素数が等しいかどうかを判別する
  def wellStructured(shape: Shape): Boolean = {
    // 5-1. checkRect
    // 目的：受け取ったリストが横の長さ(列数)kの長方形であるか調べる
    def checkRect(s: Shape, k: Int): Boolean = {
      s match {
        case Nil => true
        case x :: xs => (x.length == k) && checkRect(xs, k)
      }
    }
    val (n, m) = size(shape)
    if (n == 0 | m == 0) false
    else checkRect(shape, m)
  }

  // 6. rotate
  // 目的：受け取ったshapeを反時計回り90°回転させる
  // 契約：受け取るshapeが全うである
  def rotate(shape: Shape): Shape = {
    // 6-1. subrotate
    // 目的：受け取ったlistの各要素をsの各行の先頭へ逆順に入れる
    def subrotate(s: Shape, list: Row): Shape = {
      s match {
        case Nil => duplicate(list.length,Nil)
        case x :: xs => {
          list match {
            case Nil => s
            case _ => (list.last :: x) :: subrotate(xs,list.init)  
          }
        }
      }
    }
    assert(wellStructured(shape)) //契約
    shape match {
      case Nil => Nil
      case x :: xs => shape.foldRight(duplicate(x.length,Nil): Shape)((y: Row, t: Shape) => subrotate(t,y))
    }
  }

  // 後で使うのでrotate_rev(逆回転)を定義しておく
  // 目的：受け取ったshapeを時計回り90°回転させる
  def rotate_rev(shape: Shape): Shape = {
    // 6-1. subrotate_rev
    // 目的:受け取ったlistの各要素をsの各行の先頭へ順に入れる
    def subrotate_rev(s: Shape, list: Row): Shape = {
      s match {
        case Nil => duplicate(list.length,Nil)
        case x :: xs => {
          list match{
            case Nil => s
            case _ => (list.head :: x) :: subrotate_rev(xs,list.tail)  
          }
        }
      }
    }
    assert(wellStructured(shape)) //契約
    shape match {
      case Nil => Nil
      case x :: xs => shape.foldLeft(duplicate(x.length,Nil): Shape)((t: Shape, y: Row) => subrotate_rev(t,y))
    }
  }

  // 7. shiftSE
  // 目的：shapeを右にxマス、下にyマスずらす
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    // 7-1. shiftRight
    // 目的：受け取った行の先頭に要素Transparentをn個追加する
    def shiftRight(s: Shape, n: Int): Shape = {
      // 7-2. subshift
      // 目的：受け取った行の先頭に要素Transparentをn個追加する
      def subshift(row: Row, m: Int): Row = {
        m match {
          case 0 => row
          case _ => Transparent :: subshift(row, m-1)
        }
      }
      s.map(t => subshift(t,n))
    }
    rotate_rev(shiftRight(rotate(shiftRight(shape, x)), y))
  }

  // 8. shiftNW
  // 目的：shapeを左にxマス、上にyマスずらす
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    rotate(rotate(shiftSE(rotate(rotate(shape)), x, y)))
  }

  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大する
  // 契約：rowsとcolsはshapeの行数、列数以上
  def padTo(shape: Shape, rows: Int, cols: Int): Shape = {
    val (n, m) = size(shape)
    assert(rows - n >= 0 && cols - m >= 0) //契約
    shiftNW(shape, cols - m, rows - n)
  }

  // 10. overlap
  // 目的：2つのshapeが重なりを持つか判別する
  def overlap(shape1: Shape, shape2: Shape): Boolean = {
    // 10-1. suboverlap
    // 目的: 2つのrowが重なりを持つか判別する
    def suboverlap(row1: Row, row2: Row): Boolean = {
      (row1, row2) match {
        case (x :: xs, y :: ys) => (x != Transparent && y != Transparent) | suboverlap(xs, ys)
        case _ => false
      }
    }
    (shape1, shape2) match {
      case (x :: xs, y :: ys) => suboverlap(x, y) | overlap(xs, ys)
      case _ => false
    }
  }

  // 11. combine
  // 目的：2つのshapeを結合する
  // 契約：2つのshapeが重なりをもたない
  def combine(shape1: Shape, shape2: Shape): Shape = {
    // 11-1. subcombine
    // 目的: 2つのrowを結合する
    def subcombine(row1: Row, row2: Row): Row = {
      (row1, row2) match {
        case (Nil, Nil) => Nil
        case (Nil, row2) => row2
        case (row1, Nil) => row1
        case (x :: xs, y :: ys) => {
          if(x == Transparent) y ::subcombine(xs, ys)
          else x :: subcombine(xs, ys)
        }
      }
    }
    // 11-2. combineX
    // 目的: いびつな形（長方形でない）になる結合を行う
    def combineX(s: Shape, t: Shape): Shape = {
      (s, t) match {
        case (Nil, Nil) => Nil
        case (Nil, t) => t
        case (s, Nil) => s
        case (x :: xs, y :: ys) => subcombine(x, y) :: combineX(xs, ys)
      }
    }
    assert(overlap(shape1, shape2) == false) //契約
    if(size(shape1) != size(shape2)) {
      val n = max(shape1.length, shape2.length)
      val m = max(maxCols(shape1), maxCols(shape2))
      combine(padTo(shape1, n, m), padTo(shape2, n, m))
    }
    else combineX(shape1, shape2)
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

  println(duplicate(5, "おはよう!") == List("おはよう!", "おはよう!", "おはよう!", "おはよう!", "おはよう!")) //自作テスト

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)

  println(empty(2, 0) == List(Nil, Nil)) //自作テスト

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))

  println(size(List(List(Transparent, Transparent), List(Transparent, Transparent, Transparent), List(Transparent))) == (3, 3)) //自作テスト

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)

  println(blockCount(List(List(Transparent, Transparent), List(Transparent, Transparent, Transparent), List(Transparent))) == 0) //自作テスト

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  
  println(wellStructured(List(List(Blue, Blue), List(Yellow, Yellow, Yellow), Nil)) == false) //自作テスト

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  println(rotate(shapeO) == shapeO) //自作テスト
  show(rotate(shapeZ)) //自作テスト

  // rotate が満たすべき性質のテスト
  println(wellStructured(List(List(Red), List(Blue, Yellow))) == false) //自作テスト
  println(wellStructured(shapeI) == true) //自作テスト

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))

  println(shiftSE(List(List(Red, Blue), List(Yellow, Transparent)), 2, 2) ==
    List(List(Transparent, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Red, Blue),
         List(Transparent, Transparent, Yellow, Transparent))) //自作テスト
  show(shiftSE(shapeJ, 2, 3)) //自作テスト

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  println(shiftNW(List(List(Red, Blue), List(Yellow, Transparent)), 2, 2) ==
    List(List(Red, Blue, Transparent, Transparent),
         List(Yellow, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent))) //自作テスト
  show(shiftNW(shapeJ, 2, 3)) //自作テスト

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))

  println(padTo(List(List(Red, Blue), List(Yellow, Transparent)), 4, 4) ==
    List(List(Red, Blue, Transparent, Transparent),
         List(Yellow, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent, Transparent))) //自作テスト
  show(padTo(shapeJ, 5, 8)) //自作テスト

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)

  println(overlap(shapeO, shapeL) == true) //自作テスト
  println(overlap(shapeL, shiftSE(shapeO, 1, 0)) == false) //自作テスト
  println(overlap(shiftSE(shapeI, 0, 1), shapeZ) == false) //自作テスト

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

  println(combine(List(List(Red),
                       List(Yellow),
                       List(Transparent, Blue)),
                  List(List(Transparent, Red),
                       List(Transparent, Yellow),
                       List(Blue))) ==
    List(List(Red, Red),
         List(Yellow, Yellow),
         List(Blue, Blue))) //自作テスト
  show(combine(shapeL, shiftSE(shapeO, 1, 0))) //自作テスト
}
