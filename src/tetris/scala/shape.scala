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
  // 目的：n個のx(任意の型)からなるリストを作る
  // 0未満は0と扱う
  // 定義：
  def duplicate[A](n:Int, x:A):List[A] = {
    if (n < 1) Nil
    else x :: duplicate[A](n-1,x)
  }

  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る。
  // 0未満は0と扱う
  // 定義：
  def empty(row:Int,col:Int):Shape = {
    duplicate[Row](row, duplicate[Block](col,Transparent))
  }



  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の 形で返す。
  // 定義
  def size(mino:Shape):(Int,Int) = {
    val r = mino.length
    val c = mino.foldLeft(0)((init, x) => if(init<x.length) x.length else init)
  (r, c)
  }
  
  

  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロック の数を返す。
  // 定義：
  def blockCount(mino:Shape):Int = {
    mino.foldLeft(0)(
      (init,xs) => init + xs.foldLeft(0)(
        (init,x) => if(x == Transparent) init  else init+1
        )
      )
  }



  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する。「まっとう」とは行数・列数がともに1以上であり、各行の要素数が全て等しいことを指す。
  // 定義:
  def wellStructured(mino:Shape):Boolean = {
    mino.foldLeft(-1)((init,x) => if(init == -1 || x.length == init) x.length else 0) > 0 &&
    mino.length > 0
  }

  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転 させた shape を返す
  // 契約：引数の shape はまっとうである
  
  // 目的: 2つの Shape を横に結合する
  // 契約: 両方とも Nil でなければ縦の長さが同じ
  def renketsu(mino1: Shape, mino2: Shape): Shape = {
    if (mino1 != Nil && mino2 != Nil) assert(mino1.length == mino2.length)
    (mino1, mino2) match {
      case (Nil, _) => mino2
      case (_, Nil) => mino1
      case (x :: xs, y :: ys) => x ++ y :: renketsu(xs, ys)
    }
  }
  
  // 本体
  def rotate(mino:Shape):Shape = {
    assert(wellStructured(mino))
    mino.foldLeft[Shape](Nil)((init,xs) => 
    renketsu(init,xs.foldLeft[Shape](Nil)((init2,x) => List(List(x)) ++ init2))
    )
  }
  
  // テスト用　rotateのsize(行数列数逆版)
  def rotatesize(mino:Shape):(Int,Int) = {
    val r = mino.length
    val c = mino.foldLeft(0)((init, x) => if(init<x.length) x.length else init)
  (c, r)
  }
  
  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらしたshape を返す
  // 契約：引数の shape はまっとうである
  def shiftSE(mino:Shape, x:Int, y:Int):Shape = {
    assert(wellStructured(mino))
    var col = mino.head.length
    empty(y,col+x) ++ mino.map(xs => duplicate(x, Transparent) ++ xs)
  }



  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらしたshape を返す
  // 契約：引数の shape はまっとうである
  def shiftNW(mino:Shape, x:Int, y:Int):Shape = {
    assert(wellStructured(mino))
    var col = mino.head.length
    mino.map(xs => xs ++ duplicate(x, Transparent)) ++ empty(y,col+x)
  }

 
  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大し た shape を返す
  // 契約：rows, cols は shape の行数・列数以上,shapeはまっとう
  def padTo(mino:Shape, row:Int, col:Int):Shape = {
    assert(row >= mino.length && col >= mino.head.length && wellStructured(mino))
    shiftNW(mino, col - mino.head.length, row - mino.length)
  }

  // 10. overlap
  // 目的：2つの shape が重なりを持つかを判断する
  def overlap(mino1:Shape, mino2:Shape):Boolean = {
    def overlaprow(row1: Row, row2: Row):Boolean = {
      (row1, row2) match {
        case (Nil, _) => false
        case (_, Nil) => false
        case (x :: xs, y :: ys) => (x != Transparent && y != Transparent) || overlaprow(xs, ys)
      }
    }
    (mino1, mino2) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (x :: xs, y :: ys) => overlaprow(x, y) || overlap(xs, ys)
    }
  }

  // 11. combine
  // 目的：2つの shape を結合する
  // 契約：引数の shape は重なりを持たない
  def combine(mino1:Shape, mino2:Shape):Shape = {
    assert(overlap(mino1,mino2) == false)
    def combinerow(row1: Row, row2: Row):Row = {
      (row1, row2) match {
        case (Nil, _) => row2
        case (_, Nil) => row1
        case (x :: xs, y :: ys) => if (x == Transparent) y :: combinerow(xs,ys) else x :: combinerow(xs,ys)
      }
    }
    (mino1, mino2) match {
      case (Nil, _) => mino2
      case (_, Nil) => mino1
      case (x :: xs, y :: ys) => combinerow(x, y) :: combine(xs, ys)
    }
  }



}

// テスト
object ShapeTest extends App {
  import ShapeLib._

  // 関数を定義するたびに、コメント開始位置を後ろにずらす
  // 1. duplicate
  println(">>duplicate<<")
  println(duplicate(0, 42) == Nil)
  println(duplicate(1, true) == List(true))
  println(duplicate(3, "hi") == List("hi", "hi", "hi"))
  // 自作
  println(duplicate(2, 7) == List(7,7))

  // 2. empty
  println(">>empty<<")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  // 自作
  println(empty(2, 2) == List(
    List(Transparent, Transparent),
    List(Transparent, Transparent)
  ))

  // 3. size
  println(">>size<<")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  // 自作
  println(size(shapeL) == (3, 2))
  println(size(List(
    List(Transparent, Transparent),
    List(Transparent)
  )) == (2, 2))

 
  // 4. blockCount
  println(">>blockCount<<")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  // 自作  
  println(blockCount(List(
    List(Transparent, Red),
    List(Transparent)
  )) == 1)
 
 
  // 5. wellStructured
  println(">>wellStructured<<")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  // 自作
  println(wellStructured(List(
    List(Transparent, Red),
    List(Transparent),
    List(Red, Transparent)
  )) == false)
  println(wellStructured(List(
    List(Transparent, Red),
    Nil,
    List(Red, Transparent)
  )) == false)
 
  // 6. rotate
  println(">>rotate<<")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  
  // 自作
  show(rotate(shapeS))
  show(rotate(shapeL))
  // rotate が満たすべき性質のテスト
  println(shapeT == rotate(rotate(rotate(rotate(shapeT)))))
  println(wellStructured(shapeZ))
  println(blockCount(shapeZ) == blockCount(rotate(shapeZ)))
  println(size(shapeL) == rotatesize(rotate(shapeL)))

  // 7. shiftSE
  println(">>shiftSE<<")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  // 自作
  show(shiftSE(shapeL,2,1))
  
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  // 自作
  show(shiftNW(shapeL,2,1))
  
  // 9. padTo
  println(">>padTo<<")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  // 自作
  show(padTo(shapeL,5,2))
  
  
  // 10. overlap
  println(">>overlap<<")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  // 自作
  println(overlap(shapeI, shiftNW(shapeZ, 1, 1)) == true)

  // 11. combine
  println(">>combine<<")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  // 自作
  show(combine(shapeI, shiftSE(shapeZ, 1, 1)) )

}
