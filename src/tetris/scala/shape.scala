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
    if(n <= 0) Nil
    else a :: duplicate(n-1,a)
  } 

  // 2. empty
  // 目的：rows行cols列の空のshapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows,duplicate(cols,Transparent))
  }


  // 3. size
  // 目的：受け取ったshapeのサイズを(行数、列数)の形で返す
  def size(shape: Shape): (Int,Int) = {
    //目的:受け取ったshapeの最大の列数を返す
    def maxcols(s: Shape): Int = {
      s match {
        case Nil => 0
        case r :: rs => max(r.length,maxcols(rs))
      }
    }
    shape match {
      case Nil => (0,0)
      case t :: ts => (shape.length,max(t.length,maxcols(ts)))
    }
  }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  def blockCount(shape: Shape): Int = {
    //目的:受け取ったrowに含まれる空でないブロックの数を返す
    def subblockCount(row: List[Block]): Int = {
      row match {
        case Nil => 0
        case r :: rs => 
          r match {
            case Transparent => subblockCount(rs)
            case _ => 1 + subblockCount(rs)
          }
      }
    }
    shape match {
      case Nil => 0
      case t :: ts => subblockCount(t) + blockCount(ts) 
    }
  }

  // 5. wellStructured
  // 目的：受け取ったshapeが全うであるかを判断する(ただし、「まっとう」とは行数・列数がともに1以上であり、各行の要素数がすべて等しいことを指す)
  def wellStructured(shape: Shape): Boolean = {
    //目的:受け取ったshapeの各行の要素数がすべて等しいかどうか判定する
    def subwellStructed(s: Shape): Boolean = {
      //目的:受け取ったshapeの最大の列数を返す
      def maxcols(s1: Shape): Int = {
        s1 match {
          case Nil => 0
          case r :: rs => max(r.length,maxcols(rs))
        }
      }
      s match {
        case Nil => true
        case r :: rs => {
          if(rs == Nil) true
          else if(r.length == maxcols(rs)) true && subwellStructed(rs)
          else false
        }
      }
    }
    shape match {
      case Nil => false 
      case r :: rs => {
        if(r == Nil) false
        else subwellStructed(shape)
      }
    }
  }

  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：受け取ったshapeはまっとう
  def rotate(shape: Shape): Shape = {
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
    assert(wellStructured(shape)) //契約
    shape match{
      case Nil => Nil
      case r :: rs => shape.foldRight(duplicate(r.length,Nil): Shape)((x: Row,t: Shape) => subrotate(t,x))
    }
  }

  // 7. shiftSE
  // 目的：受け取ったshapeを右にx,下にyずらしたshapeを返す
  def shiftSE(shape: Shape,x: Int,y :Int): Shape = {
    val (a,b) = size(shape)
    val shiftdown = empty(y,b) ++ shape
    shiftdown match{
      case Nil => Nil
      case r :: rs => (duplicate(x,Transparent) ++ r) :: shiftSE(rs,x,0)
    }
  }

  // 8. shiftNW
  // 目的：受け取ったshapeを左にx,上にyずらしたshapeを返す
  def shiftNW(shape: Shape,x: Int,y :Int): Shape = {
    val (a,b) = size(shape)
    val shiftup = shape ++ empty(y,b)
    shiftup match{
      case Nil => Nil
      case r :: rs => (r ++ duplicate(x,Transparent)) :: shiftNW(rs,x,0)
    }
  }

  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す
  // 契約：rows,colsはshapeの行数・列数以上
  def padTo(shape: Shape,rows: Int,cols: Int) = {
    //目的:空白を追加してshapeをまっとうなものに整える
    def wellshape(shape: Shape,m: Int): Shape = {
      shape match{
        case Nil => Nil
        case r :: rs => (r ++ duplicate(m-r.length,Transparent)) :: wellshape(rs,m)
      }
    } 
    val (a,b) = size(shape)
    assert(rows >= a && cols >= b)
    val s1 = shiftNW(shape,cols-b,rows-a)
    wellshape(s1,cols)
  }

  // 10. overlap
  // 目的：2つのshapeが重なりを持つかを判断する
  def overlap(s1: Shape,s2: Shape): Boolean = {
    (s1,s2) match{
      case (Nil,_) => false
      case (_,Nil) => false
      case (r1 :: rs1,r2 :: rs2) =>{
        (r1,r2) match{
          case (Nil,_) => false || overlap(rs1,rs2)
          case (_,Nil) => false || overlap(rs1,rs2)
          case (t1 :: ts1,t2 :: ts2) => {
            if(t1 != Transparent && t2 != Transparent) true
            else false || overlap(ts1 :: rs1, ts2 :: rs2)
          }
        }
      }
    }
  }

  // 11. combine
  // 目的：2つのshapeを結合する.まっとうなshapeになるよう空白を補う
  // 契約：引数のshapeは重なりを持たない
  def combine(s1: Shape,s2: Shape): Shape ={
    assert(! overlap(s1,s2))
    //目的:2つのshapeを結合する
    def subcombine(s1: Shape,s2: Shape): Shape ={
      //目的:2つのRowを重ねる
      def rowcombine(r1: Row,r2: Row): Row = {
        (r1,r2) match{
          case (Nil,_) => r2
          case (_,Nil) => r1
          case (x :: xs,y ::ys) => {
            if(x != Transparent) x :: rowcombine(xs,ys)
            else y :: rowcombine(xs,ys)
          }
        }
      }
      (s1,s2) match{
        case (_,Nil) => s1
        case (Nil,_) => s2
        case (r1 :: rs1,r2 :: rs2) => rowcombine(r1,r2) :: subcombine(rs1,rs2)
      }
    }
    val combineshape = subcombine(s1,s2)
    val (a,b) = size(combineshape)
    padTo(combineshape,a,b)
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
  println(duplicate(2, List(Nil,Nil)) == List(List(Nil,Nil),List(Nil,Nil)))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(0,0) == Nil)

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(List(Nil,Nil)) == (2,0))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(List(List(Transparent,Red,Blue))) == 2)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Transparent,Red,Blue),Nil)) == false)


  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  println(rotate(List(List(Red,Blue),List(Yellow,Transparent))) == List(List(Blue,Transparent),List(Red,Yellow)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  println(wellStructured(rotate(shapeS)) == true)
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  println(size(rotate(shapeS)) == (3,2))

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  println(shiftSE(List(List(Blue,Red),List(Yellow,Yellow)),0,2) == List(List(Transparent,Transparent),List(Transparent,Transparent),List(Blue,Red),List(Yellow,Yellow)))
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  println(shiftNW(List(List(Blue,Red),List(Yellow,Yellow)),0,2) == List(List(Blue,Red),List(Yellow,Yellow),List(Transparent,Transparent),List(Transparent,Transparent)))
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  println(padTo(List(List(Blue,Red),List(Yellow)),2,2) == List(List(Blue,Red),List(Yellow,Transparent)))
  show(padTo(shapeI, 6, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeS, shiftSE(shapeZ,3,2)) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent,Blue))) == List(List(Red,Blue),List(Transparent,Transparent)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

}
