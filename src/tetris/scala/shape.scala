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
  // 目的：整数ｎと任意の型aを受け取り、ｎ個のaからなるリストを返す
  def duplicate[A](n:Int,a:A): List[A]={
    def s(n:Int,a:A,list:List[A]): List[A]={
      if (n>=1) s(n-1,a,a::list)
      else if (n==0) list
      else Nil
    }
    s(n,a,Nil)    
  }

  
  // 2. empty
  // 目的：r行 c列の空のshapeを作る
  def empty(r:Int,c:Int): List[Row]={
    if (r==0) Nil
    else if (r!=0&&c==0) duplicate(r,Nil)
    else   duplicate(r,duplicate(c,Transparent))
  }

  // length
  // 目的：受け取った listのサイズを返す
  def length[A](list:List[A]): Int={
      list match{
        case Nil => 0
        case x::xs => 1+length(xs)
      } 
    }

  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す
  def size(a:Shape): (Int,Int) ={
    a match{
      case Nil =>(0,0)  
      case r::rs =>(length(r::rs),length(r))
    }  
  }



  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す
  def blockCount(a:Shape): Int={
     // 目的：受け取った row に含まれる空でないブロックの数を返す
    def count(b:Row): Int={
      b match{
        case Nil =>0
        case x::xs => if (x==Transparent)  count(xs)
                      else 1+ count(xs)
      }
    }
    a match{
      case Nil =>0
      case r::rs => count(r)+ blockCount(rs)
    }
  }



  // 5. wellStructured
  // 目的：受け取ったshape行数・列数がともに１以上であり、各行の要素数が全て等しいか判断する
  def wellStructured(a:List[Row]): Boolean={
     a match{
     case Nil =>false
     case Nil::xs =>false
     case y::Nil =>true
     case x::y::xs => if(length(x)==length(y)) wellStructured(y::xs)
                      else false 
     }
  }
  
  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：受け取ったshapeはまっとう
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape)) //契約
    //目的: 行一つとshapeを受け取って, shapeの各行の先頭に受け取った行の要素を逆順に一つずつ足してできるshapeを返す 
    def subrotate(s: Shape,list:Row):Shape = {
      s match{
        case Nil => duplicate(list.length,Nil)
        case r :: rs => {
          list match{
            case Nil => s
            case _ => (list.last :: r) :: subrotate(rs,list.init)  
          }
        }
      }
    }
    
    shape match{
      case Nil => Nil
      case r :: rs => shape.foldRight(duplicate(r.length,Nil): Shape)((x: Row,t: Shape) => subrotate(t,x))
    }
  }  
  /*
  
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：引数のshapeが関数wellStructuredでtureを返す
  def rotate(shape:List[Row]): List[Row]={
      def r(shape:List[Row],b:List): List
      shape match{
        case Nil =>Nil
        case a::as =>as match{
                    case Nil=>Nil
                    case b::bs => r(as,) 
                  }
      }
    }
  */



  // 7. shiftSE
  // 目的：受け取ったshapeを右にｘ、下にｙずらしたshapeを返す
  def shiftSE(a:List[Row],x:Int,y:Int): List[Row]={
    def e(list:List[Row],x:Int):List[Row]={
      list match{
        case Nil=>Nil
        case a::as => duplicate(x,Transparent) ++ a :: e(as,x)
      }
    }
    def s(list:List[Row],y:Int): List[Row]={
      list match{
        case Nil=>Nil
        case a::as => empty(y,length(a)) ++ list
        }    
     }
     s(e(a,x),y)
  }



  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらしたshape を返す
  def shiftNW(a:List[Row],x:Int,y:Int): List[Row]={
    def w(list:List[Row],x:Int):List[Row]={
      list match{
        case Nil=>list
        case a::as=> a ++ duplicate(x,Transparent) ::  w(as,x)
      }
    }
    def n(list:List[Row],y:Int): List[Row]={
      list match{
        case Nil=>Nil
        case a::as=>list ++ empty(y,length(a))            
      }
    }
    n(w(a,x),y)
  }



  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape:Shape,r:Int,c:Int): Shape={
    val (y,x)=size(shape)
    assert(r>=y&&x>=x)
    shiftNW(shape,c-x,r-y)
  }



  // 10. overlap
  // 目的：２つの shape が重なりを持つかを判断する
  def overlap(shapeA:Shape,shapeB:Shape): Boolean={
    def overlap2(r1:Row,r2:Row): Boolean={
      (r1,r2) match{
      case (a::as,b::bs) => if(a!=Transparent&&b!=Transparent) true
                           else  overlap2(as,bs)
      case (Nil,b::bs) =>false
      case (a::as,Nil) =>false 
      case (Nil,Nil)=>false               
     }    
    }
    (shapeA,shapeB) match{
      case(a::as,b::bs) => if (overlap2(a,b)==true) true 
                           else overlap(as,bs)
      case(Nil,b::bs) =>false
      case(a::as,Nil) =>false
      case(Nil,Nil) =>false
    }
  }



  // 11. combine
  // 目的：２つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(shapeA:Shape,shapeB:Shape): Shape={
    assert(overlap(shapeA,shapeB)==false)
    def combine2(r1:Row,r2:Row): Row={
      (r1,r2) match{
      case (a::as,b::bs) => if(a==Transparent) b::combine2(as,bs)
                            else  a::combine2(as,bs)
      case (Nil,b::bs) => r2      
      case (a::as,Nil) => r1
      case (Nil,Nil)=> Nil           
     }
    }
    (shapeA,shapeB) match{
      case(a::as,b::bs) => combine2(a,b) :: combine(as,bs)
      case(Nil,b::bs) => shapeB
      case(a::as,Nil) => shapeA
      case(Nil,Nil) => Nil
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
  println(duplicate(5, Transparent) == List(Transparent,Transparent,Transparent,Transparent,Transparent ))
  
  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 2) == List(List(Transparent,Transparent),List(Transparent,Transparent)))
  
  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(List(List(Red,Red),List(Red,Yellow))) == (2, 2))
 
  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(List(List(Red,Red),List(Transparent,Yellow))) == 3)
  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Yellow, Yellow), List(Blue,Transparent))) == true)
  
  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  println(rotate(List(List(Yellow,Red), List(Red,Blue),List(Blue,Yellow))) == 
    List(List(Red,Blue,Yellow),List(Yellow,Red,Blue)))
  // rotate が満たすべき性質のテスト
   //rotateするごとにsizeの行数と列数が入れ替わる
    println(size(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue)))==(3,2))
    println(size(rotate(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))))==(2,3))
   //blockCountで返ってくる数は何回rotateしても変わらない
    println(blockCount(List(List(Yellow, Yellow), List(Blue,Transparent)))==blockCount(rotate(List(List(Yellow, Yellow), List(Blue,Transparent)))))
    println(blockCount(List(List(Yellow, Yellow), List(Blue,Transparent)))==blockCount(rotate(rotate(List(List(Yellow, Yellow), List(Blue,Transparent))))))
    println(blockCount(List(List(Yellow, Yellow), List(Blue,Transparent)))==blockCount(rotate(rotate(rotate(List(List(Yellow, Yellow), List(Blue,Transparent)))))))
   //4度回したら元に戻る
    println(rotate(rotate(rotate(rotate(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue)))))) ==
    List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue)))
   //shapeは真っ当
    println(wellStructured(rotate(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue)))) == true)

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  println(shiftSE(List(List(Red,Red),List(Yellow,Yellow)), 2, 2) ==
    List(List(Transparent, Transparent,Transparent,Transparent),
         List(Transparent, Transparent,Transparent,Transparent),
         List(Transparent, Transparent,Red,Red),
         List(Transparent, Transparent,Yellow,Yellow)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  println(shiftNW(List(List(Red,Red),List(Yellow,Yellow)), 2, 1) ==
    List(List(Red,Red,Transparent,Transparent),
         List(Yellow,Yellow,Transparent,Transparent),
         List(Transparent, Transparent,Transparent,Transparent))) 
  
  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  println(padTo(List(List(Blue,Red)), 3, 4) ==
    List(List(Blue, Red, Transparent,Transparent),
         List(Transparent, Transparent, Transparent,Transparent),
         List(Transparent,Transparent,Transparent,Transparent)))
  
  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeZ, shiftSE(shapeI, 2, 1)) == true)
  
  
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  println(combine(List(List(Yellow,Transparent), List(Transparent,Red)),
                  List(List(Transparent,Blue,Yellow), List(Blue,Transparent,Transparent),List(Blue,Red,Red))) ==
    List(List(Yellow,Blue,Yellow), List(Blue,Red,Transparent),List(Blue,Red,Red)))
  
}
