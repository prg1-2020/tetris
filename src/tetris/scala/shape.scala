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
  // 目的：整数nと任意の型aを受け取り、n個のaからなるリストを作る。
  def duplicate[A](n:Int,x:A):List[A] = {
    if(n <= 0) Nil
    else x :: duplicate(n-1,x)
  }



  // 2. empty
  // 目的：row行col列の空のshapeを作る
  def empty(m:Int,n:Int) = {
    duplicate(m,duplicate(n,Transparent))
  }


  // 3. size
  // 目的：受け取ったshapeのサイズを返す
  def size[A](n:Shape) = {
    if(n.length == 0)(0,0)
    else (n.length,n.foldRight(0)((x,y) => max(x.length,y)))
  }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれるブロックの数を返す
  def blockCount(n:Shape) = {
    n.foldRight(0)((x,y) => y + x.foldRight(0)((a,b) => b + (if(a != Transparent) 1 else 0)))
  }


  // 5. wellStructured
  // 目的：受け取ったshapeの行数と列数がともに1以上であり、各行の要素数が等しいかどうかを判断する
  def wellStructured(n:Shape) = {
    if(n.foldRight(-1)((x,y) => (if(x.length == y || y == -1) x.length else 0)) >= 1) true else false
  }


  // 6. rotate
  // 目的：受け取ったsizeを反時計回りに90度回転したものを返す
  // 契約：受け取るShapeはWellStructuredである
  def rotate(shape:Shape):Shape = {
    assert(wellStructured(shape))
    val (r,c) = size(shape)
    (List.range(0,c))map{
      i => (List.range(0,r))map{
        j  => shape(j)(c - i - 1)
      }
    }
  }



  // 7. shiftSE
  // 目的：与えられた図形を右にx,下にy移したものを返す
  def shiftSE(shape:Shape,x:Int,y:Int):Shape = {
        val (r,c) = size(shape)
    (List.range(0,r + y))map{
      i => (List.range(0,c + x))map{
        j => if(i < y || j < x) Transparent else shape(i - y)(j - x)
      }
    }
  }


  // 8. shiftNW
  // 目的：与えられた図形を左にx,上にy移したものを返す
  def shiftNW(shape:Shape,x:Int,y:Int):Shape = {
    val (r,c) = size(shape)
    (List.range(0,r + y))map{
      i => (List.range(0,c + x))map{
        j => if(i >= r || j >= c) Transparent else shape(i)(j)
      }
    }
  }


  // 9. padTo
  // 目的：row行col列に拡大したShapeを返す
  // 契約：row,colはShapeのx,y以上
  def padTo(shape:Shape,r:Int,c:Int):Shape = {
    val (x,y) = size(shape)
    assert(x <= r && y <= c)
    shiftNW(shape,c - y,r - x)
  }


  // 10. overlap
  // 目的：2つのshapeが重なりを持つか判定する
  def overlap(shape1:Shape,shape2:Shape):Boolean = {
    def overlaprow(r1:Row,r2:Row):Boolean = {
      (r1,r2)match{
        case(_,Nil) => false
        case(Nil,_) => false
        case(x :: xs,y :: ys) => if((x != Transparent && y != Transparent) || overlaprow(xs,ys)) true else false
      }
    }
    (shape1,shape2) match{
        case(_,Nil) => false
        case(Nil,_) => false
      case(x :: xs,y :: ys) => if(overlaprow(x,y) || overlap(xs,ys)) true else false
    }
  }


  // 11. combine
  // 目的：2つのShapeを結合する
  // 契約：2つのShapeは共通部分を持たない
  def combine(shape1:Shape,shape2:Shape):Shape = {
    assert(!overlap(shape1,shape2))
    val (x1,y1) = size(shape1)
    val (x2,y2) = size(shape2)
    def max(a:Int,b:Int):Int = {
      if(a >= b) a else b
    }

    def combinerow(r1:Row,r2:Row):Row = {
      (r1,r2)match{
        case(_,Nil) => r1
        case(Nil,_) => r2
        case(x :: xs,y :: ys) => if(x == Transparent) y :: combinerow(xs,ys) else x :: combinerow(xs,ys)
      }
    }
    def combineall(block1:Shape,block2:Shape):Shape = {
      (block1,block2) match{
          case(Nil,Nil) => Nil
          case(x :: xs,Nil) => (List.range(0,max(y1,y2))).map{i => if(i >= combinerow(x,Nil).length) Transparent else combinerow(x,Nil)(i)} :: combineall(xs,Nil)
          case(Nil,y :: ys) => (List.range(0,max(y1,y2))).map{i => if(i >= combinerow(Nil,y).length) Transparent else combinerow(Nil,y)(i)} :: combineall(Nil,ys)
          case(x :: xs,y :: ys) => {
              (List.range(0,max(y1,y2))).map{i => if(i >= combinerow(x,y).length) Transparent else combinerow(x,y)(i)} :: combineall(xs,ys)
        }
      }
    }

    combineall(shape1,shape2)


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
  println(duplicate(5, "A") == List("A", "A", "A","A","A"))

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
  println(size(shapeT) == (2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeT) == 4)

  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue,Blue))) == true)


  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeS))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeT)))) == shapeT)
  println(wellStructured(rotate(shapeI)) == true)


  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  println(shiftSE(List(List(Red)), 1, 1) ==
    List(List(Transparent, Transparent),
         List(Transparent, Red)))
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  println(shiftNW(List(List(Red)), 1, 1) ==
  List(List(Red, Transparent),
        List(Transparent, Transparent)))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))

  println(padTo(List(List(Blue)), 1, 1) ==
  List(List(Blue, Transparent)))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)

  println(overlap(shapeI, shapeT) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent,Green))) ==
    List(List(Red,Green), List(Transparent,Transparent)))
}
