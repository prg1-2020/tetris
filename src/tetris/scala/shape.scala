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
import scala.math.abs

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
  def duplicate[A](n: Int, a: A): List[A] ={
    n match{
      case 0 => Nil
      case _ => a :: duplicate(n-1, a)
    }
  }



  // 2. empty
  // 目的：rows行 cols列の空のshapeを作る
  def empty(rows: Int, cols: Int): Shape ={
    duplicate(rows, duplicate(cols, Transparent))
  }


  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す
  def size(shape: Shape): (Int, Int)={
    //アキュミュレータ：(rows, cols)：この関数を呼び出す前までに数えてきたサイズ
    def sizeAcc(shape: Shape, rows: Int, cols: Int): (Int,Int) ={
      shape match{
        case Nil => (rows, cols)
        case x::xs => sizeAcc(xs, rows + 1, max(cols, x.length))
      }
    }
    sizeAcc(shape, 0, 0)
  }


  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す
  def blockCount(shape: Shape): Int ={
    //目的：受け取った row に含まれる空でないブロックの数を返す
    def blockCountRow(row: Row): Int ={
      row match{
        case Nil => 0
        case Transparent::xs => blockCountRow(xs)
        case x::xs => blockCountRow(xs) + 1
      }
    }
    shape match{
      case Nil => 0
      case x::xs => blockCountRow(x) + blockCount(xs)
    }
  }



  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する。
  //      ただし、「まっとう」とは行数・列数がともに1以上であり、
  //      各行の要素数が全て等しいことを指す。
  def wellStructured(shape: Shape): Boolean ={
    //アキュミュレーター：(rows, cols)：この関数を呼び出す前までの(元の)shape のサイズ rows:行　cols:列
    def wellStructuredAcc(shape: Shape, rows: Int, cols: Int): Boolean={
      shape match{
        case Nil => if(rows==0) false else true
        case x::xs =>
          if(x.length==0) false
          else if(x.length!=cols && cols!=0) false
          else wellStructuredAcc(xs, rows+1, x.length)
      }
    }
    wellStructuredAcc(shape, 0, 0)
  }


  // 6. rotate
  // 目的：shape を反時計回りに90度回転させたものを返す
  // 契約：まっとうである
  def rotate(shape: Shape): Shape ={
    assert(wellStructured(shape))
    var (rows: Int, cols: Int) = size(shape)
    

    //目的：listの先頭を返す　存在しない場合はNilを返す
    def headS(list: Shape): Row ={
      if (list.headOption == None) Nil
      else list.head
    }
    //目的：listの先頭以外を返す　存在しない場合はNilを返す
    def tailS(list: Shape): Shape ={
      if (list.headOption == None) Nil
      else list.tail
    }
    //目的：rowの先頭をRow型で返す　存在しない場合はNilを返す
    def headR(list: Row): Row={
      if (list.headOption == None) Nil
      else List(list.head)
    }
    //目的：rowの先頭以外をShape型で返す　存在しない場合はNilを返す
    def tailR(list: Row): Shape ={
      if (list.lastOption == None) Nil
      else List(list.tail)
    }

    //目的： row を shape に変換　ただし row == Nil の時、List(Nil) ではなく Nil を返す
    def mkshape(row: Row): Shape={
      if (row == Nil) Nil
      else List(row)
    }

    //アキュムレータ：shape(分解してshapeConpの適切な位置に配置)、pivot(どの行に着目するか)、shapeOver(pivotより上の行,pivot==1 の時shape)、shapeUnder(pivot以下の行, pivot==1の時Nil)、shapeConp(最終的に完成するshape)
    def rotateAcc(shape: Shape, pivot: Int, shapeOver: Shape, shapeUnder: Shape, shapeConp: Shape): Shape ={
      shape match {
        case Nil => shapeConp
        case x :: xs =>
          shapeUnder match{
            case Nil => rotateAcc(tailR(x) ++ xs, 2, tailR(x) ++ Nil, xs, mkshape(headR(x)) ++ shapeConp )
            case xU :: xsU =>
              if (pivot<rows) rotateAcc(shapeOver ++ (tailR(xU) ++ xsU), pivot + 1, shapeOver ++ tailR(xU) , xsU, mkshape(headS(shapeConp) ++ headR(xU)) ++ tailS(shapeConp))
              else rotateAcc(shapeOver ++ tailR(xU), 1, shapeOver ++ tailR(xU), xsU, mkshape(headS(shapeConp) ++ headR(xU)) ++ tailS(shapeConp))
        }
      }      
    }

    rotateAcc(shape, 1, shape, Nil, Nil)

  }



  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらした shape を返す
  def shiftSE(shape: Shape, right: Int, down: Int): Shape ={
    //目的：受け取った row に tp の分だけ左側に Transparent を付け加える
    def addTransparent(row: Row, tp: Int): Row ={
      if(tp == 0) row
      else if(tp < 0) addTransparent(row, tp + 1) ++ List(Transparent)
      else Transparent :: addTransparent(row, tp - 1)
    }

    if(down > 0){
      var (rows: Int, cols: Int) = size(shape)
      empty(down, cols + abs(right)) ++ shiftSE(shape, right, 0)
    }

    else if (down < 0){
      var (rows: Int, cols: Int) = size(shape)
      shiftSE(shape, right, 0) ++ empty(-down, cols + abs(right))
    }

    else{
      shape match{
        case Nil => Nil
        case x::xs => addTransparent(x, right) :: shiftSE(xs, right, 0)
      }
    }
  }


  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらした shape を返す
  def shiftNW(shape: Shape, left: Int, up: Int): Shape ={
    shiftSE(shape, -left, -up)
  }



  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大し た shape を返す
  // 契約：rows, cols は shape の行数・列数以上
  def padTo(shape: Shape, rows: Int, cols: Int): Shape ={
    var (shape_r, shape_c) = size(shape)
    assert(shape_r <= rows, shape_c <= cols)
    shiftNW(shape, cols - shape_c, rows - shape_r)
  }



  // 10. overlap
  // 目的：2つの shape が重なりを持つかを判断する
  def overlap(shape1: Shape, shape2: Shape): Boolean ={
    //目的：2つの row が重なりを持つかを判断する
    def overlapRow(row1: Row, row2: Row): Boolean ={
      (row1, row2) match{
        case (Nil, b) => false
        case (a, Nil) => false
        case (x1 :: xs1, x2 :: xs2) => (x1 != Transparent && x2 != Transparent) || overlapRow(xs1, xs2)
      }
    }
    (shape1,shape2) match{
      case (Nil, b) => false
      case (a, Nil) => false
      case (x1 :: xs1, x2 :: xs2) => overlapRow(x1,x2) || overlap(xs1, xs2)
    }
  }



  // 11. combine
  // 目的：2つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(shape1: Shape, shape2: Shape): Shape ={
    assert(!overlap(shape1, shape2))
    //目的：2つの row を結合する （ combine で assert があるので契約は要らない）
    def combineRow(row1: Row, row2: Row): Row ={
      (row1, row2) match{
      case (Nil, b) => b
      case (a, Nil) => a
      case (Transparent :: xs1, Transparent :: xs2) => Transparent :: combineRow(xs1, xs2)
      case (x1 :: xs1, Transparent :: xs2) => x1 :: combineRow(xs1, xs2)
      case (Transparent :: xs1, x2 :: xs2) => x2 :: combineRow(xs1, xs2)
      case _ => Nil
      }
      
    }
    (shape1, shape2) match{
      case (Nil, b) => b
      case (a, Nil) => a
      case (x1 :: xs1, x2 :: xs2) => List(combineRow(x1, x2)) ++ combine(xs1, xs2)
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
  println(duplicate(2, Nil) == List(Nil, Nil))
  println(duplicate(2, true) == List(true, true))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(0, 0) == Nil)
  println(empty(1, 2) == List(List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeO) == (2, 2))
  println(size(shapeJ) == (3, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeO) == 4)
  println(blockCount(List(List(Transparent, Red), List(Blue), List(Transparent))) == 2)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red), Nil)) == false)
  println(wellStructured(shapeS) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  println(rotate(List(List(Transparent))) == List(List(Transparent)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(combine(shiftSE(shapeL, 2, 0), shapeJ)))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeL)))) == shapeL)
  var (a: Int, b: Int) = size(rotate(shapeL))
  println((b, a) == size(shapeL))
  println(blockCount(shapeL) == blockCount(rotate(shapeL)))
  println(wellStructured(rotate(shapeL)))

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  println(shiftSE(List(List(Red)), 0 , 1) == List(List(Transparent), List(Red)))
  show(shiftSE(shapeI, 1, 2))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  println(shiftNW(List(List(Red)), 0, 1) == List(List(Red), List(Transparent)))
  show(shiftNW(shapeI, 1, 2))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  println(padTo(List(List(Red)), 1, 2) == List(List(Red, Transparent)))
  show(padTo(shapeI, 6, 2))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeJ, shapeO) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  println(combine(List(List(Blue)), List(List(Transparent))) == List(List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

}
