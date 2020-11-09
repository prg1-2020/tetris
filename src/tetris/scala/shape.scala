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
  // 目的：整数nと型aを受け取り、n個のaからなるリストを作る
  def duplicate[A](n:Int, a: A):List[A]={
    if (n<=0) Nil
    else a :: duplicate(n-1,a)
  }


  // 2. empty
  // 目的：rows行cols列の空のshapeを作る関数
  def empty (rows:Int, cols:Int):Shape={
    duplicate(rows,duplicate(cols,Transparent))
  }

   
  


  // 3. size
  // 目的：shapeのサイズを返す
  def size (shape : Shape): (Int, Int) = {
    (shape.length, shape.foldRight(0)((n, m) => max(n.length,m)))
  }




  // 4. blockCount
  // 目的：shape内のブロックの数を返す
  def blockCount(shape: Shape): Int = {
    shape.foldRight(0)((n,m) =>
      m + n.foldRight(0)((l, o) => if (l != Transparent) o + 1 else o)
      )
  }



  // 5. wellStructured
  // 目的：shapeが真っ当か判断する
  def wellStructured(shape: Shape) : Boolean ={
    val (rows, cols) = size(shape)
    rows >= 1 && cols >= 1 && shape.foldRight(true)((n,m) => m && cols == n.length)
  }


  // 6. rotate
  // 目的：shapeを時計回りに90度回転させたshapeを返す
  // 契約：
<<<<<<< HEAD

   def rotate(shape: Shape): Shape = {
     //目的: 行一つとshapeを受け取って, shapeの各行の先頭に受け取った行の要素を逆順に一つずつ足してできるshapeを返す 
     def newrotate(s: Shape,list:Row):Shape = {
       s match{
         case Nil => duplicate(list.length,Nil)
         case r :: rs => {
           list match{
             case Nil => s
             case _ => (list.last :: r) :: newrotate(rs,list.init)  
           }
         }
       }
     }
     assert(wellStructured(shape))
     shape match{
       case Nil => Nil
       case r :: rs => shape.foldRight(duplicate(r.length,Nil): Shape)((x: Row,t: Shape) => newrotate(t,x))
     }
    }


=======
  def rotate(shape: Shape): Shape = {
    //目的: 行一つとshapeを受け取って, shapeの各行の先頭に受け取った行の要素を逆順に一つずつ足してできるshapeを返す 
    def newrotate(s: Shape,list:Row):Shape = {
      s match{
        case Nil => duplicate(list.length,Nil)
        case r :: rs => {
          list match{
            case Nil => s
            case _ => (list.last :: r) :: newrotate(rs,list.init)  
          }
        }
      }
    }
    assert(wellStructured(shape)) //契約
    shape match{
      case Nil => Nil
      case r :: rs => shape.foldRight(duplicate(r.length,Nil): Shape)((x: Row,t: Shape) => newrotate(t,x))
    }
   }
>>>>>>> refs/remotes/origin/master


  // 7. shiftSE
  // 目的：shapeを右にx下にy動かす
  def shiftSE(shape: Shape,x: Int, y :Int) : Shape = {
    val (n,m) = size(shape)
    val newshiftSE = duplicate(y,duplicate(m,Transparent)) ++ shape
    newshiftSE match{
      case Nil => Nil
      case r :: rs => (duplicate(x,Transparent) ++ r) :: shiftSE(rs,x,0)
    }
  }



  // 8. shiftNW
  // 目的：shapeを左にx右にy動かす
  def shiftNW(shape: Shape,x: Int,y :Int): Shape = {
    val (n,m) = size(shape)
    val newshiftNW = shape ++ duplicate(y,duplicate(m,Transparent))
    newshiftNW match{
      case Nil => Nil
      case r :: rs =>(r ++ duplicate(x,Transparent)) :: shiftNW(rs,x,0)
    }
  }



  // 9. padTo
  // 目的：shapeを受け取り、rows行cols列に拡大したshapeを返す
  // 契約：wellStructured が true
  def padTo (shape: Shape, rows:Int, cols: Int) : Shape ={
    val (shaper,shapec) = size(shape)
    assert((rows >= shaper) && (cols >= shapec))
    shiftNW(shape,cols - shapec  , rows - shaper )
  }




  // 10. overlap
  // 目的：２つのshapeが重なりを持つか判断する
  def overlap(shape1 : Shape , shape2: Shape): Boolean={
    def newoverlap (list1: Row, list2: Row) : Boolean = {
      (list1,list2) match {
        case (x :: xs, y::ys) => ( x != Transparent && y!= Transparent)  || newoverlap(xs,ys)
        case (Nil , _)        =>  false
        case (_ , Nil)        =>  false
      }
    }
    (shape1,shape2) match{
        case (x :: xs, y :: ys) => newoverlap(x,y) || overlap(xs,ys)
        case (Nil, _)         =>  false
        case (_ ,Nil)         =>  false
      }
   }
   



  // 11. combine
  // 目的：２つのshapeを結合する
  // 契約：重なりを持たない
  def combine (shape1 : Shape , shape2:Shape): Shape ={
    assert(overlap(shape1,shape2) == false)
    def newoverlap(list1 : Row, list2: Row) :Row ={
      (list1, list2) match {
        case (x::xs , y::ys) => ( if(x != Transparent) x else y) :: newoverlap(xs, ys)
      }
    }
    val (rows1, cols1)  =size(shape1)
    val (rows2, cols2)  =size(shape2)
    val max_rows = max(rows1, rows2)
    val max_cols = max(cols1, cols2)
    val shape11  =padTo(shape1, max_rows,max_cols)
    val shape22  =padTo(shape2, max_rows,max_cols)

    (shape11, shape22) match{
      case (x :: xs, y::ys) => newoverlap(x,y) ::  combine(xs, ys)
      case (Nil,  _) => Nil
      case (_ , Nil) => Nil
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
  plintln(duplicate(0, 1) == Nil)

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  plintln(empty(2, 3) == List(List(Transparent, Transparent, Transparent),List(Transparent, Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(List(List(transparent))) == (1,1))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  plintln(blockCount(shapeO) == 4)


  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(shapeO) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeT))

  // rotate が満たすべき性質のテスト
  println(wellStructured(rotate(shapeZ)) == true)
  println(rotate(rotate(rotate(rotate(shapeZ)))) ==shapeZ)



  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeS, 3, 4))
  plintln(shiftSE(List(List(Blue)),1 ,0) ==
    List(List(Transparent, Blue)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeS, 3, 4))
  plintln(shiftNW(List(List(Blue)),1,0)
    List(List(Blue, Transparent)))
  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(pafTo(shapeS, 3, 2))
  println(padTo(List(List(Blue)),2,2)
    List(List(Blue, Tranaparent),
         List(Transparent, Transparent)))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeJ, shapeT) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ)
  */
}
