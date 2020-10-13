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
  // 目的：整数nとaとを受け取り、n個のaからなるリストを出力する
  // 多相性をもつ関数として定義する
  def duplicate[A](n:Int, a:A):List[A] ={
    if (n<=0) Nil
    else a::duplicate(n-1, a)
  }

  // 2. empty
  // 目的：2つの整数rowsとcolsとを受け取り、rows行col列の空のshapeを出力する
  def empty(rows:Int,cols:Int):Shape ={
    duplicate[Row](rows,duplicate[Block](cols,Transparent))
  }

  // 3. size
  // 目的：行数は.lengthで計算する。列数は畳み込みにより計算する
  def size(X:Shape):(Int,Int)={
    (X.length, X.foldLeft(0)((x:Int,W:Row)=>max(x,W.length))) 
  }

  // 4. blockCount
  // 目的：Shape型のデータを受け取り、空でないブロックの数を出力する
  // 補助関数-Row型のデータに含まれる空でないブロックの数を出力する-を定義する
  // アキュムレーター1:空でないブロックの数をカウントする
  def Subf(R:Row,acc1:Int):Int ={
    R match {
      case Nil => acc1
      case x :: xs => if (x!=Transparent) Subf(xs,acc1+1) else Subf(xs,acc1)
    }
  }
  // アキュムレーター2:空でないブロックの数をカウントする
  def blockCount(X:Shape):Int ={
    def blockCountAcc(Y:Shape,acc2:Int):Int ={
      Y match {
        case Nil => acc2
        case y :: ys => blockCountAcc(ys,Subf(y,0)+acc2)
      }
    }
    blockCountAcc(X,0)
  }



  // 5. wellStructured
  // 目的：Shape型のデータを受け取り、"まっとう"かどうかを判断する関数をつくる
  // 補助関数-Shape型のデータと整数aに対して、各行の要素数がaであるかどうかを判断する-を導入する
  def f(a:Int,Y:Shape):Boolean ={
    Y match {
      case y :: ys => if (y.length==a) f(a,ys) else false
      case Nil => true
    }
  }
  def wellStructured(X:Shape):Boolean ={
    X match {
      case Nil => false
      case x :: xs => if (x==Nil) false else{
        val k=x.length 
        f(k,xs) }
    }
  }

  // 6. rotate
  // 目的：
  // 契約：



  // 7. shiftSE
  // 目的：



  // 8. shiftNW
  // 目的：



  // 9. padTo
  // 目的：
  // 契約：



  // 10. overlap
  // 目的：



  // 11. combine
  // 目的：
  // 契約：



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

  println(duplicate(3, 3.14) == List(3.14, 3.14, 3.14))
  
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

  println(size(shapeO) == (2, 2))
  println(size(shapeJ) == (3, 2))

  
  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)

  println(blockCount(shapeJ) == 4)
  println(blockCount(shapeO) == 4)

  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)

  println(wellStructured(List(List(Red, Red), Nil, List(Blue, Blue))) == false)
  println(wellStructured(shapeO) == true)
  println(wellStructured(shapeJ) == true)

  /*
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト


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
  show(combine(shiftSE(shapeI, 0, 1), shapeZ)
  */
}
