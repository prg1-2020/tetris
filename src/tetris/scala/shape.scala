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
  // 目的：整数nと任意の型の値aを受け取り,n個のaからなるリストを作る

  def duplicate[A](n: Int, a: A): List[A] = {
    if (n > 0) a :: duplicate(n - 1, a)
    else Nil
  }

  // 2. empty
  // 目的：rows行cols列の空のshapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }

  // 3. size
  // 目的：受け取ったshapeのサイズを(行数, 列数)の形で返す
  def size(x: Shape): (Int, Int) = {
    (x.length, x.foldRight(0)((y, r) => max(y.length, r)))
  }

  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  def blockCount(x: Shape): Int = {
    x.foldRight(0)((y, r) => r + y.foldRight(0)((z, s) => if(z == Transparent) s else s + 1))
  }

  // 5. wellStructured
  // 目的：受け取ったshapeがまっとうであるか判断する
  def wellStructured(x: Shape): Boolean = {
    val (a, b) = size(x)
    a >= 1 && b >= 1 && x.foldRight(true)((y, r) => r && b == y.length)
  }

  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：受け取ったshapeがまっとうである
  def rotate(x: Shape): Shape = {
    //契約
    assert(wellStructured(x))
    def rot(y: Shape): Shape = {
      if (wellStructured(y)) {
        y.map(_.last).foldLeft(Nil: Row)((l, r) => l :+ r) :: rot(y.map(_.init))
      }
      else Nil
    } 
    rot(x)
  }


  // 7. shiftSE
  // 目的：受け取ったshapeを右にx,下にyずらしたshapeを返す
  def shiftSE(s: Shape, x: Int, y: Int): Shape = {
    val (h,w) = size(s)
    val a = empty(1, x)
    val b = empty(y, x + w)
    val l1 = s.map(z => a.head ++ z)
    b ++ l1
  }


  // 8. shiftNW
  // 目的：受け取ったshapeを左にx,上にyずらしたshapeを返す
  def shiftNW(s: Shape, x: Int, y: Int): Shape = {
    val (h,w) = size(s)
    val a = empty(1, x)
    val b = empty(y, x + w)
    val l1 = s.map(z => z ++ a.head)
    l1 ++ b
  }



  // 9. padTo
  // 目的：受け取ったshapeをrows行,cols列に拡大したshapeを返す
  // 契約：rows,colsはshapeの行数・列数以上
  def padTo(s: Shape, rows: Int, cols: Int): Shape = {
    val (h,w) = size(s)
    val (x,y) = (cols - w, rows - h)
    //契約
    assert(x >= 0 && y >= 0)
    shiftNW(s, x, y)
  }



  // 10. overlap
  // 目的：２つのshapeが重なりを持つかを判断する
    def overlap(s1:Shape,s2:Shape):Boolean = {
    def ju(l1:Row,l2:Row):Boolean = {
      (l1,l2) match{
        case (x::xs,y::ys) => (x!=Transparent && y!=Transparent) || ju(xs,ys)
        case _             => false
      }
    }
    (s1,s2) match{
      case (x::xs,y::ys) => ju(x,y) || overlap(xs,ys)
      case _             => false
    }
  }



  // 11. combine
  // 目的：２つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
def combine (s0: Shape, s1 : Shape): Shape = {
  //契約
  assert(!overlap(s0, s1))
  val (h0, w0) = size(s0)
  val (h1, w1) = size(s1)
  val (h, w) = (max(h0, h1), max(w0, w1))

  def ketsugo_row(r0: Row, r1: Row): Row = {
    (r0, r1) match {
      case (x :: xs, y :: ys) => (if (x == Transparent) y else x) :: ketsugo_row(xs, ys)
      case _ => Nil
    }
  }

  def ketsugo_col(sh0: Shape, sh1: Shape): Shape = {
    (sh0, sh1) match {
      case (x :: xs, y :: ys) => ketsugo_row(x, y) :: ketsugo_col(xs, ys)
      case _ => Nil
    }

  }

  ketsugo_col(padTo(s0, h, w), padTo(s1, h, w))

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
  println(duplicate(4, Transparent) == List(Transparent, Transparent, Transparent,Transparent))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(1, 1) == List(List(Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeO) == (2, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeO) == 4)

  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red))) == true)
  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  println(rotate(List(List(Red, Blue), List(Yellow, Red))) == List(List(Blue, Red), List(Red, Yellow)))

  // rotate が満たすべき性質のテスト
  //4 回呼ぶと元の shape に戻る
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)
  //受け取った shape の形を (h, w) とすると、返す shape の形は (w, h)
  println(size(rotate(shapeZ)) == (3,2))
  //返す shape はまっとう
  println(wellStructured(rotate(shapeL)))

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  println(shiftSE(List(List(Red, Blue), List(Yellow, Red)), 2, 3)  ==
                  List(List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Red, Blue),
                       List(Transparent, Transparent, Yellow, Red)))


  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  println(shiftNW(List(List(Red, Blue), List(Yellow, Red)), 2, 3)  ==
                  List(List(Red, Blue, Transparent, Transparent),
                       List(Yellow, Red, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent)))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  println(padTo(List(List(Red, Blue), List(Yellow, Red)), 3, 4)  ==
                  List(List(Red, Blue, Transparent, Transparent),
                       List(Yellow, Red, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       ))

  
  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(List(List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Red, Blue),
                       List(Transparent, Transparent, Yellow, Red)),
                       List(List(Red, Blue, Transparent, Transparent),
                       List(Yellow, Red, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent))) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  println(combine(List(List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Red, Blue),
                       List(Transparent, Transparent, Yellow, Red)),
                       List(List(Red, Blue, Transparent, Transparent),
                       List(Yellow, Red, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent))) 
                    == List(List(Red, Blue, Transparent, Transparent),
                       List(Yellow, Red, Transparent, Transparent),
                       List(Transparent, Transparent, Transparent, Transparent),
                       List(Transparent, Transparent, Red, Blue),
                       List(Transparent, Transparent, Yellow, Red)) )
}