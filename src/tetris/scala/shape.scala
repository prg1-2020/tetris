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
  // 目的：整数nと任意の型の値aを受け取りリストを作る
  def duplicate(n:Int,a:Any):List[Any]={
    if(n<=0) Nil else a::duplicate(n-1,a)
  }


  // 2. empty
  // 目的：rows,colsの空のshapeを作る
  def empty(rows:Int, cols:Int):List[Any]={
    val ls = duplicate(cols,Transparent)
    duplicate(rows,ls)
  }


  
  // 3. size
  // 目的：受け取ったshapeのサイズを(行数、列数)を返す
  def maxlist(list:List[Int]):Int={
      list match{
        case Nil => 0
        case x::xs => max(x,maxlist(xs))
      }
    }

  def size(shape:Shape):(Int,Int)={
    val a = shape.length
    val bs = shape.map(_.length)
    (a,maxlist(bs))
  }  



  // 4. blockCount
  // 目的：受け取ったshapeの空でないブロックを数える
  def blockCount(shape:Shape):Int={
    def blockInRow(row:List[Block]):Int={
      row match{ 
        case Nil => 0
        case x::xs => if(x == Transparent) 0 + blockInRow(xs) else 1 + blockInRow(xs)
      }
    }
    shape.map(blockInRow).foldRight(0)(_+_)
  }



  // 5. wellStructured
  // 目的：shapeの列行共に１以上で各行の要素数が等しいか返す
  def wellStructured(shape:Shape):Boolean={
    def tupleplus(x:Int,y:Int):Boolean = x >=1 && y >= 1
    val (n,m) = size(shape)
    val lengthlist = shape.map(_.length)
    val maxoflength = maxlist(lengthlist)
    def equalJudge(list:List[Int],n:Int):Boolean={
      list match{
        case Nil => true
        case x::xs => x == n && equalJudge(xs,n)
      }
    }
    equalJudge(lengthlist,maxoflength) && tupleplus(n,m)
  }
  


  // 6. rotate
  // 目的：shapeを反時計回りに90度回転させたshapeを返す
  // 契約：引数のshapeはまっとうである
  def rotate(shape:Shape)= {
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
  // 目的：右にｘ,下にｙずらしたshapeを返す
  def shiftSE(shape:Shape,x:Int,y:Int): Shape ={
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
  // 目的：左にx,上にｙずらしたshapeを返す
  def shiftNW(shape:Shape,x:Int,y:Int):Shape ={
    def shiftW(shape1:Shape,x1:Int):Shape ={
      val addshapeW = duplicate(x1, Transparent) 
      shape1.map(_++addshapeW)
    }
    def shiftN(shape2:Shape,x2:Int):Shape={
      val (n,m) = size(shape2)
      shape2 ++ empty(x2,m)
   }
   shiftN(shiftW(shape,x),y)
  }



  // 9. padTo
  // 目的：受け取ったshapeをrows,colsに拡張したshapeを返す
  // 契約：rows,colsがshapeの列行以上
  def padTo(shape:Shape,rows:Int,cols:Int):Shape={
    val (n,m) = size(shape)
    assert( rows>=n && cols>=m )
    shiftNW(shape,cols-m,rows-n)
  }



  // 10. overlap
  // 目的：2つのshapeが重なりを持つかを判断する
  def overlap(shape1:Shape, shape2:Shape):Boolean={

    def blockoverlap(block1:Block,block2:Block):Boolean={
      if(block1 != Transparent && block2 != Transparent)true else false
    }
    def rowoverlap(row1:Row,row2:Row):Boolean={
      (row1,row2) match{
        case (Nil,Nil) => false
        case (Nil,r)   => false
        case (r,Nil)   => false
        case (b1::b1s,b2::b2s) => blockoverlap(b1,b2) || rowoverlap(b1s,b2s)
      }
    }

    val (n1,m1) = size(shape1)
    val (n2,m2) = size(shape2)
    val n = max(n1,n2)
    val m = max(m1,m2)
    val s1 = padTo(shape1,n,m)
    val s2 = padTo(shape2,n,m) 

    (s1,s2) match{
      case (Nil,Nil) => false
      case (s,Nil) => false
      case (Nil,s) => false
      case (x1::x1s , x2::x2s) => rowoverlap(x1,x2) || overlap(x1s,x2s)
    }
    
  }



  // 11. combine
  // 目的：2つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
  def combine(shape1:Shape,shape2:Shape):Shape={
    assert(overlap(shape1,shape2) == false)

    val (n1,m1) = size(shape1)
    val (n2,m2) = size(shape2)
    val n = max(n1,n2)
    val m = max(m1,m2)
    val s1 = padTo(shape1,n,m)
    val s2 = padTo(shape2,n,m) 

    def blockcombine(block1:Block,block2:Block):Block={
      (block1,block2) match{
        case (Transparent,b) => b
        case (b, Transparent)   => b
      }
    }

    def rowcombine(row1:Row,row2:Row):Row={
      (row1,row2) match{
        case (Nil, Nil) => Nil
        case (b1::b1s,b2::b2s) => blockcombine(b1,b2) ::rowcombine(b1s,b2s)  
      }
    }
    (s1,s2) match{
      case (Nil,Nil) => Nil
      case (x1::x1s , x2::x2s) => rowcombine(x1,x2) :: combine(x1s,x2s)
    }
  }
}

// テスト
object ShapeTest extends App {
  import ShapeLib._

  // 関数を定義するたびに、コメント開始位置を後ろにずらす
  /*
  // 1. duplicate
  println("duplicate")
  println(duplicate(0, 42) == Nil)
  println(duplicate(1, true) == List(true))
  println(duplicate(3, "hi") == List("hi", "hi", "hi"))
  println(duplicate(-1, 5) == Nil)
  
  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(-1,5) == Nil)
  
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
  println(wellStructured(shapeT) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeL)) //added test

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shape = shapeT)))) == shapeT) //4回rotateしたらもとにもどる
  println(wellStructured(rotate(shape = shapeJ)))                   //rotateしたshapeもまっとうである
  println(blockCount(rotate(shape = shapeL)) == blockCount(shapeL)) //rotateしてもブロックの数は変わらない

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeJ,2,2)) //added test
  
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeL,2,2)) //added test

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeJ,4,4))   //added test

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeL,shiftSE(shapeZ,1,0)) == false) //added test
  
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shapeL,shiftSE(shapeZ,1,1))) //added test
}
