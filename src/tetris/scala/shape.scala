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
  // 目的：整数nと任意の型の値aを受け取り、n個のaからなるリストを作る
  def duplicate[A](n :Int, a :A): List[A] = {
    if(n<=0) Nil
    else a::duplicate(n-1, a)
  }


  // 2. empty
  // 目的rows行cols列の空のShapeを作る
  def empty(rows :Int, cols :Int): Shape = {
    if(rows<=0) Nil
    else duplicate(rows, duplicate(cols, Transparent))
  }

  // 3. size
  // 目的：受け取ったshapeのサイズを (行数, 列数) の形で返す
  def size(shape :Shape): (Int, Int) = {
    /*
    var max = 0
    for (i <- shape) {
      if(i.size>max) max = i.size
    }
    (shape.size, max)
    */
    def maxshape(shape :Shape, max :Int): Int = {
      shape match {
        case Nil => max
        case x::xs => if(x.size > max) maxshape(xs, x.size) else maxshape(xs, max)
      }
    }
    (shape.size, maxshape(shape, 0))
  }

  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  def blockCount(shape :Shape): Int = {
    var cnt = 0
    for (i <- shape) {
      for (j <- i) {
        if(j != Transparent) cnt = cnt + 1
      }
    }
    return cnt
  }

  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する
  def wellStructured(shape :Shape): Boolean = {
    if(shape == Nil || shape.head == Nil) return false
    var rows = shape.size
    var cols = shape.head.size
    for(i <- shape) {
      if(i.size != cols) return false
    }
    return true
  }

  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：引数のshapeがまっとうであること
  def rotate(shape :Shape): Shape = {
  //  if(!wellStructured(shape)) shape
  //  else
    def rotateshape(col :Int, shape :Shape): Shape = {
      def addrow(col :Int, shape :Shape): Row = {
        shape match {
          case Nil => Nil
          case x::xs => x.apply(col)::addrow(col, xs)
        }
      }
      if(col < 0) Nil
      else addrow(col, shape)::rotateshape(col-1, shape)
    }
    val (row, col) = size(shape)
    rotateshape(col-1, shape)
  }


  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらした shape を返す
  def shiftSE(shape :Shape, x :Int, y :Int): Shape = {
    def shiftE(shape :Shape, x :Int): Shape = {
      def addTransparentE(shape :Shape): Shape = {
        shape match {
          case Nil => Nil
          case x::xs => (Transparent::x)::addTransparentE(xs)
        }
      }
      if(x<=0) shape
      else shiftE(addTransparentE(shape), x-1)
    }
    def shiftS(shape :Shape, y :Int, rowT :Row): Shape = {
      if(y<=0) shape
      else rowT::shiftS(shape, y-1, rowT)
    }
    def setRow(col :Int): Row = {
      if(col<=0) Nil
      else Transparent::setRow(col-1)
    }
    val (row, col) = size(shape)
    val rowT = setRow(col+x)
    return shiftS(shiftE(shape, x), y, rowT)
  }



  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらした shape を返す
  def shiftNW(shape :Shape, x :Int, y :Int): Shape = {
    def shiftW(shape :Shape, x :Int): Shape = {
      def addTransparentW(shape :Shape): Shape = {
        shape match {
          case Nil => Nil
          case x::xs => (x:+Transparent)::addTransparentW(xs)
        }
      }
      if(x<=0) shape
      else shiftW(addTransparentW(shape), x-1)
    }
    def shiftN(shape :Shape, y :Int, rowT :Row): Shape = {
      if(y<=0) shape
      else shiftN(shape, y-1, rowT):+rowT
    }
    def setRow(col :Int): Row = {
      if(col<=0) Nil
      else Transparent::setRow(col-1)
    }
    val (row, col) = size(shape)
    val rowT = setRow(col+x)
    return shiftN(shiftW(shape, x), y, rowT)
  }


  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape :Shape, rows :Int, cols :Int): Shape = {
    val (row, col) = size(shape)
    // if(rows<row || cols<col) shape
    // else
    return shiftNW(shape, cols-col, rows-row)
  }



  // 10. overlap
  // 目的：２つの shape が重なりを持つかを判断する
  def overlap(shape1 :Shape, shape2 :Shape): Boolean = {
    def overlapRow(shape1 :Shape, shape2 :Shape): Boolean = {
      def overlapCol(row1 :Row, row2 :Row): Boolean = {
        if(row1 == Nil || row2 == Nil) false
        else if(row1.head == Transparent || row2.head == Transparent) overlapCol(row1.tail, row2.tail)
        else true
      }
      if(shape1 == Nil || shape2 == Nil) false
      else if(overlapCol(shape1.head, shape2.head)) true
      else overlapRow(shape1.tail, shape2.tail)
    }
    return overlapRow(shape1, shape2)
  }



  // 11. combine
  // 目的：２つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(shape1 :Shape, shape2 :Shape): Shape = {
    //if (overlap(shape1, shape2)) return shape1
    //else
    def combineRow(shape1 :Shape, shape2 :Shape): Shape = {
      def combineCol(row1 :Row, row2 :Row): Row = {
        if(row1 == Nil || row2 == Nil) Nil
        else if(row1.head != Transparent) row1.head::combineCol(row1.tail, row2.tail)
        else if(row2.head != Transparent) row2.head::combineCol(row1.tail, row2.tail)
        else Transparent::combineCol(row1.tail, row2.tail)
      }
      if(shape1 == Nil || shape2 == Nil) Nil
      else combineCol(shape1.head, shape2.head)::combineRow(shape1.tail, shape2.tail)
    }
    val (row1, col1) = size(shape1)
    val (row2, col2) = size(shape2)
    val col = if(col1 > col2) col1 else col2
    val row = if(row1 > row2) row1 else row2
    combineRow(padTo(shape1, row, col), padTo(shape2, row, col))
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

  println(duplicate(4, "mytest") == List("mytest", "mytest", "mytest", "mytest"))
  println(duplicate(5, 0) == List(0, 0, 0, 0, 0))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))

  println(empty(0, 0) == Nil)
  println(empty(2, 2) == List(List(Transparent, Transparent), List(Transparent, Transparent)))

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

  println(blockCount(shapeS) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)

  println(wellStructured(shapeL) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  println(wellStructured(rotate(shapeL)) == true)
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)

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
