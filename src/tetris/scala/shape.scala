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
  // 目的：整数 n と任意の型の値 a を受け取り、n 個のa からなるリストを作る
  def duplicate[A](n:Int,a:A):List[A] = {
    if (n <= 0) Nil
    else a::duplicate(n-1,a)
  }

  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る
  def empty(r:Int,c:Int):Shape = {
    duplicate(r,duplicate(c,Transparent))
  }


  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の 形で返す
  def size(shape:Shape):(Int,Int) = {
    shape match {
      case Nil => (0,0)
      case _   => (shape.length,shape.map(_.length).foldLeft(0)(max))
    }
  }



  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロック の数を返す
  def blockCount(shape:Shape):Int = {
    def p(block:Block):Boolean = {
      block != Transparent
    }
    shape.foldLeft(Nil:Row)(_++_).filter(p(_)).length
  }


  // 5. wellStructured
  // 目的：受け取った shape が行数・列数がともに1以上であり、各行の要素数が全て等しいか判断する
  def wellStructured(shape:Shape):Boolean = {
    def q(n:Int,row:Row):Boolean = {
      row.length == n && n >= 1
    }
    shape match{
      case Nil     => false
      case x :: xs => shape.length == shape.filter(q(x.length,_)).length
    }
  }


  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転 させた shape を返す
  // 契約：引数の shape はまっとうである
  def rotate(shape:Shape):Shape = {
    assert(wellStructured(shape))
    def makenewlist(shape:Shape):Shape = {
      if(!wellStructured(shape)) Nil
      else shape.map(_.head).foldRight(Nil:Row)(_::_) :: makenewlist(shape.map(_.tail))
    }
    makenewlist(shape).reverse
  }



  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらしたshape を返す関数
  def shiftSE(shape:Shape,x:Int,y:Int):Shape = {
    val (a,b) = size(shape)
    def shifty(shape:Shape,y:Int):Shape = {
      y match{
        case 0 => shape
        case _ => duplicate(b,Transparent)::shifty(shape,y-1)
      }
    }
    def shiftx(shape:Shape,x:Int):Shape = {
      x match{
        case 0 => shape
        case _ => shiftx(shape,x-1).map(Transparent::_)
      }
    }
    shiftx(shifty(shape,y),x)
  }


  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらしたshape を返す
  def shiftNW(shape:Shape,x:Int,y:Int):Shape = {
    val (a,b) = size(shape)
    def shifty(shape:Shape,y:Int):Shape = {
      y match{
        case 0 => shape
        case _ => shifty(shape,y-1) :+ duplicate(b,Transparent)
      }
    }
    def shiftx(shape:Shape,x:Int):Shape = {
      x match{
        case 0 => shape
        case _ => shiftx(shape,x-1).map(_:+Transparent)
      }
    }
    shiftx(shifty(shape,y),x)
  }



  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大し た shape を返す
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape:Shape,x:Int,y:Int):Shape = {
    val (a,b) = size(shape)
    assert(x>=a && y>=b)
    shiftNW(shape,y-b,x-a)
  }



  // 10. overlap
  // 目的：2つの shape が重なりを持つかを判断する
  def overlap(s1:Shape,s2:Shape):Boolean = {
    def ol(list1:Row,list2:Row):Boolean = {
      (list1,list2) match{
        case (x::xs,y::ys) => (x!=Transparent && y!=Transparent) || ol(xs,ys)
        case _             => false
      }
    }
    (s1,s2) match{
      case (x::xs,y::ys) => ol(x,y) || overlap(xs,ys)
      case _             => false
    }
  }



  // 11. combine
  // 目的：2つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(s1:Shape,s2:Shape):Shape = {
    assert(!overlap(s1,s2))
    def cb(list1:Row,list2:Row):Row = {
      def which(x:Block,y:Block):Block = {
        if (x==Transparent) y
        else x
      }
      (list1,list2) match{
        case (Nil,Nil)     => Nil
        case (x::xs,Nil)   => x::xs
        case (Nil,y::ys)   => y::ys
        case (x::xs,y::ys) => which(x,y)::cb(xs,ys)
      }
    }
    val (a,b) = size(s1)
    val (p,q) = size(s2)
    val ns1 = padTo(s1,max(a,p),b)
    val ns2 = padTo(s2,max(a,p),q)
    (ns1,ns2) match{
      case (x::xs,y::ys) => cb(x,y)::combine(xs,ys)
      case _             => Nil
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
  println(duplicate(2, "!") == List("!", "!"))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 3) == List(List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Transparent)))

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
  println(blockCount(shapeS) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red, Transparent, Red), List(Yellow, Yellow, Transparent), List(Transparent, Blue, Blue))) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  println(rotate(List(List(Red, Transparent), List(Blue, Red), List(Transparent,Blue))) == List(List(Transparent, Red, Blue), List(Red, Blue, Transparent)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeI)))) == shapeI)
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)
  println(rotate(rotate(rotate(rotate(shapeL)))) == shapeL)
  println(rotate(rotate(rotate(rotate(shapeO)))) == shapeO)
  println(rotate(rotate(rotate(rotate(shapeT)))) == shapeT)
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  println(rotate(rotate(rotate(rotate(shapeZ)))) == shapeZ)
  println(blockCount(rotate(shapeL)) == blockCount(shapeL))
  println(size(rotate(shapeL))._1 == size(shapeL)._2 && size(rotate(shapeL))._2 == size(shapeL)._1)

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  println(shiftSE(List(List(Blue, Red), List(Transparent, Red)), 1, 1) ==
    List(List(Transparent, Transparent,Transparent),
         List(Transparent, Blue, Red),
         List(Transparent, Transparent, Red)))
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  println(shiftNW(List(List(Blue, Red), List(Transparent, Red)), 1, 1) ==
    List(List(Blue, Red,Transparent),
         List(Transparent, Red, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  println(padTo(List(List(Transparent,Red), List(Red, Transparent)), 2, 3) ==
    List(List(Transparent, Red, Transparent),
         List(Red, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shiftSE(shapeS,0,2), rotate(shapeZ)) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  println(combine(Nil, rotate(shapeZ)) == rotate(shapeZ))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shiftSE(shapeS,0,2), rotate(shapeZ)))
}
