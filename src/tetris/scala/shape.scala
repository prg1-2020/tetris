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
def duplicate[A](n: Int, a: A): List[A] = {
        if (n <= 0) Nil 
        else a::duplicate(n-1, a)   
    }


  // 2. empty
  // 目的：rows行cols列のshapeを作る
  def empty(rows:Int,cols:Int):Shape={
    duplicate(rows,duplicate(cols,Transparent))
  }



  // 3. size
  // 目的：受け取ったshapeのサイズを(行数,列数)の形で返す
  def size(shape:Shape):(Int,Int)={
    shape match{
      case Nil => (0,0)
      case x :: xs => val (rows,cols)=size(xs) 
                      (rows+1,max(cols,x.length))
    }
  }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  def blockCount(shape:Shape):Int={
    def rowblockcount(row:Row):Int={
      row match{
        case Nil => 0
        case x :: xs => 
        if(x == Transparent) rowblockcount(xs)
        else rowblockcount(xs) + 1
      }
    }
    shape match{
      case Nil => 0
      case x :: xs => blockCount(xs) + rowblockcount(x)
    }
    
  }



  // 5. wellStructured
  // 目的：受け取ったshapeが全うであるか判断する
  def wellStructured(shape:Shape):Boolean={
    val (rows,cols) = size(shape)
    def wellStructured2(shape:Shape):Boolean={
     shape match{
       case Nil => true
       case x::xs => (x.length == cols) && wellStructured2(xs)
     }
    }
    if (rows <= 0) false
    if (cols <= 0) false
    else wellStructured2(shape)
  }



  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90°回転させる
  // 契約：引数のshapeはまっとうである
  def rotate(shape:Shape):Shape={
    assert(wellStructured(shape))
    def headList(shape:Shape):Shape={
      val rownil:Row = Nil
      if (wellStructured(shape) == false) Nil
      else (rownil :: shape.map(_.last):: headList(shape.map(_.init))).tail
    }
    headList(shape)
  }



  // 7. shiftSE
  // 目的：受け取ったshapeを右にx、下にyずらしたshapeを返す。
  def shiftSE(shape:Shape,x:Int,y:Int):Shape={
    def shiftE(shape:Shape,x:Int):Shape={
      if (x==0) shape
     else shiftE(shape,x-1).map(Transparent+:_)
    }
    def shiftS(shape:Shape,y:Int):Shape={
    val (a,b)=size(shape)
     if (y==0) shape
     else duplicate(b,Transparent) +: shiftS(shape,y-1) 
    }
   shiftS(shiftE(shape,x),y)
  }




  // 8. shiftNW
  // 目的：受け取ったshapeを左にx、上にyずらしたshapeを返す。
def shiftNW(shape:Shape,x:Int,y:Int):Shape={
    def shiftW(shape:Shape,x:Int):Shape={
      if (x==0) shape
     else shiftW(shape,x-1).map(_:+Transparent)
    }
    def shiftN(shape:Shape,y:Int):Shape={
    val (a,b)=size(shape)
     if (y==0) shape
     else shiftN(shape,y-1) :+ duplicate(b,Transparent) 
    }
   shiftN(shiftW(shape,x),y)
  }



  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す
  // 契約：rows,colsはshapeの行数、列数以上
def padTo(shape:Shape,rows:Int,cols:Int):Shape={
  val (a,b) = size(shape)
  assert((rows >= a) && (cols >= b))
  shiftNW(shape,cols-b,rows-a)
}



  // 10. overlap
  // 目的：2つのshapeが重なりを持つか判断する
def overlap(shape1:Shape,shape2:Shape):Boolean={
  def listoverlap(list1:Row,list2:Row):Boolean={
   (list1,list2)match{
     case (a::as,b::bs)=>((a!=Transparent)&&(b!=Transparent)) || listoverlap(as,bs)
     case _ => false
   }

  }
  (shape1,shape2)match{
    case (x::xs,y::ys) => listoverlap(x,y) || overlap(xs,ys)
    case _ => false
     }
  }

  




  // 11. combine
  // 目的：2つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
def combine(shape1:Shape,shape2:Shape):Shape={
  assert(overlap(shape1,shape2)==false)
  val (x1,y1) = size(shape1)
  val (x2,y2) = size(shape2)
  val x = List(x1,x2).max
  val y = List(y1,y2).max 
  val shape11 = padTo(shape1,x,y)
  val shape22 = padTo(shape2,x,y)
  def listcombine(list1:Row,list2:Row):Row={
    (list1,list2)match{
      case (Nil,Nil)=>Nil
      case (a::as,Nil)=>a::as
      case (Nil,b::bs)=>b::bs
      case (a::as,b::bs)=>
      if(a==Transparent) b::listcombine(as,bs) 
      else a::listcombine(as,bs)

    }
  }
  (shape1,shape2)match{
      case (Nil,Nil)=>Nil
      case (a::as,Nil)=>a::as
      case (Nil,b::bs)=>b::bs
      case (a::as,b::bs)=>listcombine(a,b)::combine(as,bs)
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
  println(duplicate(-1, shapeZ) == Nil)
  

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(-1, 0) == Nil)



  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(List(List(Transparent))) == (1, 1))


  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(List(List(Transparent))) == 0)


  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Transparent,Transparent))) == true)


  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  
  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeT))))==shapeT)

  val (t1,t2) = size(shapeT)
  println((t2,t1) == size(rotate(shapeT)))

  println(wellStructured(shapeT))


  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftNW(shapeZ,0,0))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeZ,0,0))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeI,4,1))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeI, shapeI) == true)


  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  println(combine(List(List(Transparent)),List(List(Transparent)))==List(List(Transparent)))
  
}
