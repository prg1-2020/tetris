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
  // 目的：整数nと任意の型aを受け取り、n個のaのリストを作成する
  def duplicate[A](n:Int, a:A):List[A]={
    if(n<=0)Nil
    else a::duplicate[A](n-1, a)
  }

  // 2. empty
  // 目的：空のShapeを作成する
  def empty(x:Int, y:Int):Shape={
    duplicate(x, duplicate(y,Transparent))
  }

  // 3. size
  // 目的：Shapeの大きさを返す
    def size(shape:Shape):(Int, Int)={
      def totalsize(l1:(Int, Int), l2:(Int, Int)):(Int, Int)={
        (l1, l2) match{
          case((x1, x2), (y1, y2))=>(x1+y1, max(x2, y2))
        }
      }
      shape match{
        case Nil=>(0,0)
        case x::xs=>totalsize((1, x.length), size(xs))
      }
    }

  // 4. blockCount
  // 目的：Shapeのブロック数を返す
  def rowblockCount(row:Row):Int={
    row match{
      case Nil=>0
      case Transparent::rows=>rowblockCount(rows)
      case r::rows=>1+rowblockCount(rows)
    }
  }
  def blockCount(shape:Shape):Int={
    shape match{
      case Nil=>0
      case x::xs=>rowblockCount(x)+blockCount(xs)
    }
  }

  // 5. wellStructured
  // 目的：Shapeが真っ当かを判断
  def rowblockCount2(row:Row):Int={
    row match{
      case Nil=>0
      case r::rows=>1+rowblockCount2(rows)
    }
  }
  def wellStructured(shape:Shape):Boolean={
    def rowwellStructured(n:Int, shapea:Shape):Boolean={
      shapea match{
        case Nil=>true
        case s::ss=>if(rowblockCount2(s)==n)rowwellStructured(n, ss) else false
      }
    }
    shape match{
      case Nil=>false
      case x::xs=>if(rowblockCount2(x)==0)false else rowwellStructured(rowblockCount2(x), xs)
    }
  }

  // 6. rotate
  // 目的：shapeを反時計回りに90度回転させたものを返す
  // 契約：引数のshapeはまっとうである
  def rotate(shape:Shape):Shape={
    assert(wellStructured(shape))
    val (rows, cols)=size(shape)
    (List.range(0, cols))map{
      i=>(List.range(0, rows))map{
        j=>shape(j)(cols-1-i)
      }
    }
  }


  // 7. shiftSE
  // 目的：右にx,下にyだけずらしたshapeを返す
  def shiftSE(shape:Shape, x:Int, y:Int)={
    val (rows, cols)=size(shape)
    (List.range(0, rows+y))map{
      i=>(List.range(0, cols+x))map{
        j=>if(i>=y && j>=x)shape(i-y)(j-x) else Transparent
      }
    }
  }


  // 8. shiftNW
  // 目的：左にx,上にyだけずらしたshapeを返す
  def shiftNW(shape:Shape, x:Int, y:Int)={
    val (rows, cols)=size(shape)
    (List.range(0, rows+y))map{
      i=>(List.range(0, cols+x))map{
        j=>if(i<rows && j<cols)shape(i)(j) else Transparent
      }
    }
  }



  // 9. padTo
  // 目的：rows行cols列に拡大したshapeを返す(y行x列)
  // 契約：rows,colsはshapeの行数・列数以上
  def padTo(shape:Shape, r:Int, c:Int):Shape={
    val (rows, cols)=size(shape)
    assert(r>=rows && c>=cols)
    shiftNW(shape, c-cols, r-rows)
  }


  // 10. overlap
  // 目的：2つのshapeが重なりを持つかを判断する
  def overlap(shape1:Shape, shape2:Shape):Boolean={
    val (rows1, cols1)=size(shape1)
    val (rows2, cols2)=size(shape2)
    val minrows=if(rows1<rows2)rows1 else rows2
    val mincols=if(cols1<cols2)cols1 else cols2
    List.range(0, minrows).exists(i=>{
      List.range(0, mincols).exists(j=>shape1(i)(j)!=Transparent && shape2(i)(j)!=Transparent)
    })
  }


  // 11. combine
  // 目的：2つのshapeを結合したshapeを返す
  // 契約：引数のshapeは重なりを持たない
  def combine(shape1:Shape, shape2:Shape):Shape={
    assert(overlap(shape1, shape2)==false)
    val (rows1, cols1)=size(shape1)
    val (rows2, cols2)=size(shape2)
    (List.range(0, max(rows1, rows2)))map{
      i=>(List.range(0, max(cols1, cols2)))map{
        j=> if(i<rows1 && j<cols1 && shape1(i)(j)!=Transparent)shape1(i)(j)
            else if(i<rows2 && j<cols2 && shape2(i)(j)!=Transparent)shape2(i)(j)
            else Transparent
      }
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
  println(duplicate(2, "yes")==List("yes","yes"))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 1)==List(List(Transparent), List(Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeT)==(2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeT)==4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red, Red))) == true)

  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeO))
  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeO))))==shapeO)
  
  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeL, 1, 1))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeL, 1, 1))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeL, 8, 8))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeI, padTo(shapeL,5, 4))==false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shiftSE(shapeL, 1, 1), shapeI))
  
}
