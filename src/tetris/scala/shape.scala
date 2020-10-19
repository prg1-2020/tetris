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
  // 目的：整数 n と任意の型の値 a を受け取り、n 個のa からなるリスト
  def duplicate[A](n: Int, a: A):List[A]={
    if (n <= 0) Nil
    else a::duplicate(n-1,a)
  }


  // 2. empty
  // 目的：rows 行 cols 列の空の shape 
  def empty(rows: Int, cols: Int):Shape={
    duplicate(rows,duplicate(cols, Transparent))
  }


  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す関数
  def size(shape: Shape):(Int,Int)={
    (shape.length,shape.foldLeft(0)((cols,row)=>if(cols < row.length) row.length else cols))
  }


  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す関数
  def blockCount(shape: Shape):Int={
    shape.foldLeft(0)((z,n)=>z + n.count(c=>c!=Transparent))
  }

  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する関数
  def wellStructured(shape:Shape): Boolean={
    shape.length > 0 && shape.head.length > 0 && shape.foldLeft(true)((ret,row)=>ret && (shape.head.length == row.length))
  }

  //wellstructuredにする
  def wellStructurize(shape: Shape): Shape = {
    if (shape==Nil){
      List(List(Transparent))
    } else {
      val sz = size(shape)
      shape.foldRight(Nil:Shape)((row, ret) => (row++duplicate(sz._2-row.length,Transparent))::ret)
    } 
  }

  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転させた shape を返す関数
  // 契約：引数の shape はまっとうである
  def rotate(shape:Shape):Shape = {
    assert(wellStructured(shape))
    val sz = size(shape)
    (0 to sz._2-1).toList.foldLeft(Nil:List[Row])((rs, i)=>((shape.flatten.reverse.foldLeft((Nil:Row, 0))( (l,c) => if (l._2%sz._2==i) (c :: (l._1),l._2+1) else (l._1,l._2+1)))._1)::rs)
  }


  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらしたshape を返す
  def shiftSE(shape:Shape, x:Int, y:Int):Shape={
    empty(y,size(shape)._2+x)++shape.foldRight(Nil:Shape)((row, ret)=>(duplicate(x, Transparent)++row)::ret)
  }


  // 8. shiftNW
  // 目的：
  def shiftNW(shape:Shape, x:Int, y:Int):Shape={
    shape.foldRight(Nil:Shape)((row, ret)=>(row++duplicate(x, Transparent))::ret)++empty(y,size(shape)._2+x)
  }

  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape:Shape, rows:Int, cols:Int):Shape={
    val sz = size(shape)
    assert(sz._1 <= rows && sz._2 <= cols)
    shiftNW(wellStructurize(shape), cols-sz._2, rows-sz._1)
  }


  // 10. overlap
  // 目的：２つの shape が重なりを持つかを判断する
  def overlap(a:Shape, b:Shape):Boolean={
    a.zip(b).foldLeft(false)((f, t)=>f||t._1.zip(t._2).foldLeft(false)((g,h)=>g||(h._1!=Transparent && h._2!=Transparent)))
  }


  // 11. combine
  // 目的：２つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(a: Shape, b:Shape):Shape ={
    assert(overlap(a,b) == false)
    val sza = size(a)
    val szb = size(b)
    padTo(a,max(szb._1,sza._1),max(szb._2,sza._2)).zip(padTo(b,max(szb._1,sza._1),max(szb._2,sza._2))).foldRight(Nil:Shape)((t,f)=>t._1.zip(t._2).foldRight(Nil:Row)((h,g)=>(if (h._1!=Transparent) h._1 else h._2)::g)::f)
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
  println(duplicate(3, 4.1) == List(4.1,4.1,4.1))

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
  println(size(List(List(Blue,Blue), List(Blue,Blue,Blue))) == (2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(List(List(Blue,Blue), List(Blue,Blue,Blue))) == 5)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Blue,Blue), List(Blue,Blue,Blue))) == false)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
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
  println(shiftSE(List(List(Blue),List(Transparent,Red)), 1, 2) ==
    List(List(Transparent, Transparent,Transparent),
         List(Transparent, Transparent,Transparent),
         List(Transparent, Blue),
         List(Transparent, Transparent, Red)))
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  println(shiftNW(List(List(Transparent, Blue),List(Red)), 1, 2) ==
    List(List(Transparent, Blue, Transparent),
         List(Red, Transparent),
         List(Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  println(padTo(List(List(Blue,Transparent),List(Red), List(Transparent,Transparent,Transparent,Red)), 4, 5) ==
    List(List(Blue, Transparent, Transparent,Transparent,Transparent),
         List(Red, Transparent, Transparent,Transparent,Transparent),
         List(Transparent, Transparent, Transparent,Red,Transparent),
         List(Transparent, Transparent, Transparent,Transparent,Transparent)))
  show(padTo(shapeI, 6, 2))
  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeT, shiftSE(shapeL, 1, 1)) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  println(combine(List(
         List(Transparent, Blue, Transparent),
         List(Red, Transparent),
         List(Transparent, Transparent, Transparent),
         List(Transparent, Transparent, Blue)),
         List(
         List(Blue, Transparent),
         List(Transparent, Transparent, Red),
         List(Transparent, Transparent, Transparent, Transparent)
         )) == List(
         List(Blue, Blue, Transparent,Transparent),
         List(Red, Transparent,Red,Transparent),
         List(Transparent, Transparent, Transparent,Transparent),
         List(Transparent, Transparent, Blue,Transparent))
         )
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
}
