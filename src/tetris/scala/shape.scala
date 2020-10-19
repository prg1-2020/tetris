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
  // 目的：n個のaからなるリストを返す
  def duplicate[A](n:Int,a:A):List[A]={
    n match{
      case 0 =>Nil
      case m =>a::duplicate(n-1,a)
    }
  }

  // 2. empty
  // 目的：row行cols列の空のshapeを返す
  def empty(rows:Int,cols:Int):Shape={
    duplicate(rows,duplicate(cols,Transparent))
  }

  // 3. size
  // 目的：サイズを返す
  def size(s:Shape):(Int,Int)={
    def sizeretu(s:Shape):Int={
      s match{
        case Nil => 0
        case x::xs =>max(x.length,sizeretu(xs))
      }
    }
    (s.length,sizeretu(s))
  }

  // 4. blockCount
  // 目的：空でないブロックを返す
  def blockCount(s:Shape):Int={
    def blockCountretu(l:Row):Int={
      l match{
        case Nil => 0
        case x1::xs1 => if (x1!=Transparent) blockCountretu(xs1)+1 else blockCountretu(xs1)
      }
    }
    s match{
      case Nil => 0
      case x::xs => blockCountretu(x)+blockCount(xs)
    }
  }

  // 5. wellStructured
  // 目的：まっとうかどうか返す
  def wellStructured(s:Shape):Boolean={
    def sizeretu(s:Shape):Int={
      s match{
        case Nil => 0
        case x::xs =>max(x.length,sizeretu(xs))
      }
    }
    val retu = sizeretu(s)
    def check(s1:Shape):Boolean={
      s1 match{
        case Nil => false
        case x::Nil => x.length==retu
        case x::xs => x.length==retu && check(xs)
      }
    }
    sizeretu(s)!=0 && s.length!=0 && check(s)
  }


  // 6. rotate
  // 目的：反時計回りに90度回転させる
  // 契約：shapeはまっとう。
  def rotate(s:Shape):Shape={
    assert(wellStructured(s)==true)
    val (m,n) = size(s)
    def tuika(s1:Shape,r:Row):Shape={
      (s1,r)match{
        case(Nil,Nil) => Nil
        case(Nil,x2::xs2) => Nil
        case(x1::xs1,Nil) => Nil
        case(x1::xs1,x2::xs2) => (x1.toList:+x2)::tuika(xs1,xs2)
      }
    }
    s.foldLeft(empty(n,0))((acc:Shape,x:Row)=>tuika(acc,x)).reverse
  }


  // 7. shiftSE
  // 目的：右にx、下にy移動する。
  def shiftE1(s:Shape):Shape={
    s match{
      case Nil => Nil
      case x::xs => (Transparent::x)::shiftE1(xs)
    }
  }
  def shiftS1(s:Shape):Shape={
    def n_row(x:Int):Row={
      x match{
        case 0 => Nil
        case x1 => Transparent::n_row(x-1)
      }
    }
    val (m,n) = size(s)
    n_row(n)::s
  }
  def shiftSE(s:Shape,x:Int,y:Int):Shape={
    x match{
      case 0 => y match{
        case 0 => s
        case y1 => shiftSE(shiftS1(s),x,y1-1)
      }
      case x1 => shiftSE(shiftE1(s),x1-1,y)
    }
  } 
  // 8. shiftNW
  // 目的：左にx、上にy移動する
  def shiftW1(s:Shape):Shape={
    s match{
      case Nil => Nil
      case x::xs => (x.toList:+Transparent)::shiftW1(xs)
    }
  }
  def shiftN1(s:Shape):Shape={
    def n_row(x:Int):Row={
      x match{
        case 0 => Nil
        case x1 => Transparent::n_row(x-1)
      }
    }
    val (m,n) = size(s)
    s.toList:+n_row(n)
  }
  def shiftNW(s:Shape,x:Int,y:Int):Shape={
    x match{
      case 0 => y match{
        case 0 => s
        case y1 => shiftNW(shiftN1(s),x,y1-1)
      }
      case x1 => shiftNW(shiftW1(s),x1-1,y)
    }
  }

  // 9. padTo
  // 目的：rows行cols列に拡大
  // 契約：rows、colsはshapeの行列以上
  def padTo(s:Shape,rows:Int,cols:Int):Shape={
    val (m,n) = size(s)
    assert(rows>=m && cols>=n)
    shiftNW(s,cols-n,rows-m)
  }


  // 10. overlap
  // 目的：２つのshapeが重なるかどうかの判定
  def overlap(s1:Shape,s2:Shape):Boolean={
    val (m1,n1) = size(s1)
    val (m2,n2) = size(s2)
    val (m_max,n_max) = (max(m1,m2),max(n1,n2))
    def row_check(r1:Row,r2:Row):Boolean={
      (r1,r2) match{
        case (Nil,Nil) => false
        case (x1::xs1,x2::xs2) => (x1!=Transparent && x2!=Transparent) || row_check(xs1,xs2)
      }
    }
    def check(sh1:Shape,sh2:Shape):Boolean={
      assert(size(sh1)==size(sh2))
      (sh1,sh2) match{
        case (Nil,Nil) =>false
        case (x1::xs1,x2::xs2) => row_check(x1,x2) || check(xs1,xs2)
      }
    }
    check(padTo(s1,m_max,n_max),padTo(s2,m_max,n_max))
  }
  // 11. combine
  // 目的：2つのshapeを結合する。
  // 契約：shapeが重ならない
  def combine(s1:Shape,s2:Shape):Shape={
    assert(overlap(s1,s2)==false)
    def row_combine(r1:Row,r2:Row):Row={
      (r1,r2) match{
        case (Nil,Nil) => Nil
        case (x1::xs1,Nil) => x1::xs1
        case (Nil,x2::xs2) => x2::xs2
        case (x1::xs1,x2::xs2) => if(x1==Transparent)x2::row_combine(xs1,xs2) else x1::row_combine(xs1,xs2)
      }
    }
    def sub_combine(sh1:Shape,sh2:Shape):Shape={
      (sh1,sh2) match{
        case (Nil,Nil) => Nil
        case (x1::xs1,Nil) => x1::xs1
        case (Nil,x2::xs2) => x2::xs2
        case (x1::xs1,x2::xs2) => row_combine(x1,x2)::sub_combine(xs1,xs2)
      }
    }
    val (m1,n1) = size(s1)
    val (m2,n2) = size(s2)
    val (m_max,n_max) = (max(m1,m2),max(n1,n2))
    sub_combine(padTo(s1,m_max,n_max),padTo(s2,m_max,n_max))
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
  println(duplicate(4, 19) == List(19,19,19,19))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 3) == List(List(Transparent,Transparent,Transparent),List(Transparent,Transparent,Transparent)))

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
  println(wellStructured(List(List(Red),List(Blue, Blue))) == false)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeT))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeS))))==shapeS)//4回転させたら元通りになる
  println(blockCount(rotate(shapeS))==blockCount(shapeS))//回転させてもブロックの数は変わらない
  println(wellStructured(rotate(shapeS))==true)//回転後もまっとう
  println(rotate(shapeS)!=shapeS)

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
