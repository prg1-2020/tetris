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
  // 目的： n 個の, 任意の型の値 a からなるリストを作る.
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n < 1) Nil
    else a :: duplicate(n - 1, a)
  }


  // 2. empty
  // 目的： rows 行 cols 列の空の shape を作る.
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }


  // 3. size
  // 目的： 受け取った shape のサイズの (行数, 列数) の形で返す.
  def size(shape: Shape): (Int, Int) = {
    def sizeAcc(shape: Shape, a: Int, b: Int): (Int, Int) = {
      shape match {
        case Nil => (a, b)
        case x :: xs => sizeAcc(xs, a + 1, max(b, x.length))
      }
    }
    sizeAcc(shape, 0, 0)
  }



  // 4. blockCount
  // 目的： 受け取った shape に含まれる空でないブロックの数を返す.
  def blockCount(shape: Shape): Int = {
    def blockCountRow(row: Row): Int = {
      row match {
        case Nil => 0
        case Transparent :: xs => blockCountRow(xs)
        case x :: xs => blockCountRow(xs) + 1
      }
    }
    shape match {
      case Nil => 0
      case x :: xs => blockCountRow(x) + blockCount(xs)
    }
  }



  // 5. wellStructured
  // 目的： 受け取った shape が行数・列数がともに 1 以上であり, 各行の要素数が全て等しいことを調べる.
  def wellStructured(shape: Shape): Boolean = {
    def wellStructuredAcc(shape: Shape, a: Int, b: Int): Boolean = {
      shape match {
        case Nil =>
          if (a == 0) false
          else true
        case x :: xs =>
          if ((x.length == 0) || ((x.length != b) && (b != 0))) false
          else wellStructuredAcc(xs, a + 1, x.length)
      }
    }
  wellStructuredAcc(shape, 0, 0)
  }



  // 6. rotate
  // 目的： 受け取った shape を反時計周りに 90 度回転させる
  // 契約： 引数は真っ当である
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape) == true)

    def headlist(s1:Shape) = s1.map(_.head)
    def restlist(s2:Shape):Shape = {
      val (m,n) = size(shape)
      if(n == 1) Nil : Shape else s2.map(_.tail)
    }
    def subrotate(subshape:Shape):Shape = {
      if (wellStructured(subshape) == false)  Nil
      else subrotate(restlist(subshape)) ++ List(headlist(subshape))
    }
    subrotate(shape)
  }



  // 7. shiftSE
  // 目的： shape をみぎに x 下に y ずらした shape を返す
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    def shiftE(shape1:Shape,x1:Int):Shape ={
      val addshapeE = duplicate(x1, Transparent) 
      shape1.map(addshapeE ++_)
    }
    def shiftS(shape2:Shape,x2:Int):Shape={
      val (n,m) = size(shape2)
      empty(x2,m) ++ shape2
   }
    shiftS(shiftE(shape,x),y)
  }
  
  
  // 8. shiftNW
  // 目的： shape を左に x 上に y ずらした shape を返す
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    def shiftW(shape1: Shape, x1: Int): Shape = {
      val addright = duplicate(x1, Transparent)
      shape1.map(_ ++ addright)
    }
    def shiftN(shape2: Shape, x2: Int): Shape = {
      val(shape_rows, shape_cols) = size(shape2)
      shape2 ++ empty(x2, shape_cols)
    }
    shiftN(shiftW(shape, x), y)
  }
  
  
  
  // 9. padTo
  // 目的： shape を rows 行 cols 列に拡大し た shape を返す
  // 契約： rows, cols は shape の行数・列数以上
  def padTo(shape: Shape, x: Int, y: Int): Shape = {
    val (shape_rows, shape_cols) = size(shape)
    assert(x >= shape_rows && y >= shape_cols)
    shiftNW(shape, y - shape_cols, x - shape_rows)
  }
  
  
  
  // 10. overlap
  // 目的： 重なりを持つか判断する
  def overlap(shape1: Shape, shape2: Shape): Boolean = {
    def blockoverlap(block1: Block, block2: Block): Boolean = {
      if (block1 != Transparent && block2 != Transparent) true else false
    }
    def rowoverlap(row1: Row, row2: Row): Boolean = {
      (row1, row2) match {
        case (Nil, Nil) => false
        case (Nil, r) => false
        case (r, Nil) => false
        case (b1 :: b1s, b2 :: b2s) => blockoverlap(b1, b2) || rowoverlap(b1s, b2s)
      }
    }
    val (n1,m1) = size(shape1)
    val (n2,m2) = size(shape2)
    val n = max(n1,n2)
    val m = max(m1,m2)
    val s1 = padTo(shape1, n, m)
    val s2 = padTo(shape2, n, m) 

    (s1,s2) match {
      case (Nil,Nil) => false
      case (s,Nil) => false
      case (Nil,s) => false
      case (x1 :: x1s, x2 :: x2s) => rowoverlap(x1, x2) || overlap(x1s, x2s)
    }
  }
  
  
  
  // 11. combine
  // 目的： 二つの shape を結合する
  // 契約： 引数の shape は重なりを持たない
  def combine(shape1: Shape, shape2: Shape): Shape = {
    assert(overlap(shape1,shape2) == false)
    val (n1,m1) = size(shape1)
    val (n2,m2) = size(shape2)
    val n = max(n1, n2)
    val m = max(m1, m2)
    val s1 = padTo(shape1, n, m)
    val s2 = padTo(shape2, n, m) 

    def blockcombine(block1: Block, block2: Block): Block = {
      (block1,block2) match {
        case (Transparent, b) => b
        case (b, Transparent) => b
      }
    }

    def rowcombine(row1: Row,row2: Row): Row = {
      (row1,row2) match{
        case (Nil, Nil) => Nil
        case (b1 :: b1s,b2 :: b2s) => blockcombine(b1, b2) :: rowcombine(b1s, b2s)  
      }
    }
    (s1,s2) match{
      case (Nil,Nil) => Nil
      case (x1 :: x1s , x2 :: x2s) => rowcombine(x1, x2) :: combine(x1s, x2s)
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
  println(duplicate(4, "Scala") == List("Scala", "Scala", "Scala", "Scala"))

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
  println(size(shapeS) == (2, 3))

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
