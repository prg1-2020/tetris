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
  // 目的：整数nと任意の型の値aを引数にとってn個のaから成るリストを返す
  def duplicate[A](n:Int, a:A):List[A] ={
    if (n >= 1) a::duplicate(n-1, a)
    else Nil
  } 


  // 2. empty
  // 目的：rows行cols列の空のShapeを返す
  def empty(rows:Int, cols:Int):Shape ={
    duplicate(rows, duplicate(cols, Transparent))
  }


  // 3. size
  // 目的：Shape型のshapeを引数に取り(行数、列数)を返す
  def size(shape:Shape):(Int, Int) ={
    def sizeCol(shape:Shape):Int ={
      shape match{
        case x::xs => max(x.length, sizeCol(xs))
        case Nil   => 0
      }
    }
    (shape.length, sizeCol(shape))
  }


  // 4. blockCount
  // 目的：受け取ったShape型のshapeに含まれる空でないブロック数を返す
  def blockCount(shape:Shape):Int ={
    def findNotTransRow(row:Row):Int ={
      row match{
        case x::xs => if (x!=Transparent) 1+findNotTransRow(xs) 
                      else findNotTransRow(xs)
        case Nil   => 0
      }
    }
    shape match{
      case x::xs => findNotTransRow(x)+blockCount(xs)
      case Nil   => 0
    }
  }



  // 5. wellStructured
  // 目的：引数のshapeが行数・列数が1以上かつ各行の要素数がすべて等しいかをBooleanで返す
  def wellStructured(shape:Shape):Boolean ={
    def moreOneCol(shape:Shape):Boolean ={
      shape != Nil
    }
    def moreOneRow(shape:Shape):Boolean ={
      shape match{
        case x::xs => (x!=Nil)&&moreOneRow(xs)
        case Nil   => true
      }
    }
    def sameBlock(shape:Shape):Boolean ={
      shape match{
        case x1::x2::xs => (x1.length==x2.length)&&sameBlock(x2::xs)
        case x2::Nil    => true
        case Nil        => true
      }
    }
    moreOneCol(shape)&&moreOneRow(shape)&&sameBlock(shape)
  }


  // 6. rotate
  // 目的：
  // 契約：



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
  //自作テスト
  println(duplicate(2, List(2, 3)) == List(List(2, 3), List(2, 3)))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  //自作テスト
  println(empty(2, 2) == List(List(Transparent, Transparent), List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  //自作テスト
  println(size(List(List(Transparent), List(Transparent), List(Transparent))) == (3, 1))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  //自作テスト
  println(blockCount(List(List(Transparent, Blue))) == 1)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  //自作テスト
  println(wellStructured(List(List(Transparent, Blue), List(Transparent, Red))) == true)
  /*
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
  show(combine(shiftSE(shapeI, 0, 1), shapeZ)
  */
}
