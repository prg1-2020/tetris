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
  // 目的：a(A型)がｎ個入ったリストを作る
  def duplicate[A](n: Int, a: A): List[A] = {
    if(n <= 0) Nil
    else a :: duplicate(n - 1, a)
  }



  // 2. empty
  // 目的：rows行cols列のShapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }
  



  // 3. size
  // 目的：受け取ったShape型を（行数、列数）で返す
  def size(s: Shape): (Int, Int) = {
    val r = s.length
    val c = {
      if(r == 0) 0
      else ((List.range(0, r)) map { i =>
        s(i).length
      }).max

    }
    (r, c)
  }



  // 4. blockCount
  // 目的：受け取ったShape型に含まれる空でないブロックの数を返す
  def blockCount(s: Shape): Int = {
    def subCount(r: Row): Int = {
      r match {
        case Nil => 0
        case x :: xs => {
          val subC = subCount(xs)
          if(x == Transparent) subC
          else 1 + subC
        }
      }
    }
    s match {
      case Nil => 0
      case x :: xs => subCount(x) + blockCount(xs)
    }
  }



  // 5. wellStructured
  // 目的：受け取ったShape型がまっとうであるかを判定する
  def wellStructured(s: Shape): Boolean = {
    size(s) match {
      case (x, y) => {
        if(x == 0 || y == 0) false
        else {
          var boo = true
          var i = 0
          while(i < x){
            boo = boo && (s(i).length == y)
            i = i + 1
          }
          boo
        }
      }
    }
  }



  // 6. rotate
  // 目的：受けとったshape型を反時計回りに90度回転したshape型を返す
  // 契約：wellStructuredが真かどうか
  def rotate(s: Shape): Shape = {
    assert(wellStructured(s))
    val (rows, cols) = size(s)
    (List.range(0, cols)) map {
      i => (List.range(0, rows)) map {
        j => s(j)(cols - 1 - i)
      }
    }
  }



  // 7. shiftSE
  // 目的：受け取ったshape型を右にx、下にｙずらしたshape型を返す
  def shiftSE(s: Shape, x: Int, y: Int): Shape = {
    val (rows, cols) = size(s)
    (List.range(0, rows + y)) map { i =>
      (List.range(0, cols + x)) map { j =>
        if(i >= y && j >= x) s(i - y)(j - x)
        else Transparent
      }
    }
  }



  // 8. shiftNW
  // 目的：受け取ったshape型を左にｘ、上にyずらしたshape型を返す
  def shiftNW(s: Shape, x: Int, y: Int): Shape = {
    val (rows, cols) = size(s)
    (List.range(0, rows + y)) map { i => 
      (List.range(0, cols + x)) map { j =>
        if(i < rows && j < cols) s(i)(j)
        else Transparent
      }
    }
  }



  // 9. padTo
  // 目的：受け取ったshape型をrow行cols列に拡大したshape型を返す
  // 契約：rows >= o_rows && cols >= o_cols
  def padTo(s: Shape, rows: Int, cols: Int): Shape = {
    val (o_rows, o_cols) = size(s)
    assert(rows >= o_rows && cols >= o_cols)
    shiftNW(s, cols - o_cols, rows - o_rows)
  }


  // 10. overlap
  // 目的：2つのshape型の左上を重ねたとき、色付きの重なりが存在するか否かを返す
  def overlap(s1: Shape, s2: Shape): Boolean = {
    val (rows1, cols1) = size(s1)
    val (rows2, cols2) = size(s2)
    def min(x: Int, y: Int): Int = {
      if(x <= y) x
      else y
    }

    var boo = false
    var i = 0
    while(i < min(rows1, rows2)) {
      var j = 0
      while(j < min(cols1, cols2)) {
        boo = boo || (s1(i)(j) != Transparent) && s2(i)(j) != Transparent
        j = j + 1
      }
      i = i + 1
    }
    boo
  }



  // 11. combine
  // 目的：２つのshape型を結合したshape型を返す
  // 契約：引数のshape型２つは重なりをもたない
  def combine(s1: Shape, s2: Shape): Shape = {
    assert(overlap(s1, s2) == false)
    val (rows1, cols1) = size(s1)
    val (rows2, cols2) = size(s2)
    (List.range(0, max(rows1, rows2))) map { i =>
      (List.range(0, max(cols1, cols2))) map { j =>
        if(i < rows1 && j < cols1 && s1(i)(j) != Transparent) s1(i)(j)
        else if(i < rows2 && j < cols2 && s2(i)(j) != Transparent) s2(i)(j)
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
  println(duplicate(4, Nil) == List(Nil, Nil, Nil, Nil))  
  
  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(1, 1) == List(List(Transparent)))
  
  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeL) == (3, 2))
  println(size(List(List(Red), List(Blue, Blue))) == (2, 2))
  
  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeL) == 4)
  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(shapeL) == true)
  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeL))
  
  // rotate が満たすべき性質のテスト
  println(blockCount(rotate(shapeZ)) == blockCount(shapeZ))
  println(size(rotate(rotate(shapeZ))) == size(shapeZ))
  

  
  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeZ, 2, 1))
  
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeL, 0, 3))
  
  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeL, 4, 3))
  
  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeZ, List(List(Transparent), List(Blue))) == false)
  
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shapeZ, shiftSE(shapeZ, 2, 0)))
  
  }
