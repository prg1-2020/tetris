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
import scala.math.min

import sdraw._
import java.awt.Transparency

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
  // 目的：整数 n と任意の型の値 a を受け取り、n 個のa からなるリストを作る
  def duplicate[A](n: Int, x: A): List[A] = {
    if(n <= 0) Nil
    else x::duplicate(n - 1, x)
  }


  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る
  def empty(row: Int, col: Int): Shape = {
    if(col <= 0) Nil
    else if(row <= 0) Nil
    else duplicate(col, Transparent)::empty(row - 1, col)
  }


  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す
  def size(sh: Shape): (Int, Int) = {
    def rSize(sh: Shape):Int = {
      sh match {
        case Nil => 0
        case x::xs => max(x.length, rSize(xs))
      }
    }

    (sh.length, rSize(sh))
  }


  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す
  def blockCount(sh: Shape): Int = {
    def rowCount(row: Row): Int = {
      row match {
        case Nil => 0
        case x::xs => if(x != Transparent) 1 + rowCount(xs) else rowCount(xs)
      }
    }
    sh match{
      case Nil => 0
      case x::xs => rowCount(x) + blockCount(xs)
    }
  }


  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する
  def wellStructured(sh: Shape): Boolean = {
    def minMax(sh: Shape): (Int, Int) = {
      sh match {
        case Nil => (100, -1)
        case x::xs => {
          val st = minMax(xs)
          assert(x.length < 100 && x.length > -1)
          (min(x.length, st._1), max(x.length, st._2))
        }
      }
    }
    
    val rt = minMax((sh))
    if((sh.length < 1) || (rt._1 != rt._2) || (rt._1 < 1)) false
    else true
  }


  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転させた shape を返す
  // 契約：wellStructuredであること
  def rotate(sh: Shape): Shape = {
    assert(wellStructured(sh))
    def subSum(ro: Row, sh: Shape): Shape = {
      (ro, sh) match {
        case (Nil, Nil) => Nil
        case (x::xs, Nil) => List(x)::subSum(xs, Nil)
        case (ro_x::ro_xs, sh_x::sh_xs) => (ro_x::sh_x)::subSum(ro_xs, sh_xs)
        case (_, _) => Nil
      }
    }

    sh.foldRight(Nil: Shape)((ro, sh) => subSum(ro.reverse, sh))
  }


  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらしたshape を返す
  def shiftSE(sh: Shape, x: Int, y: Int): Shape = {
    val si: (Int, Int) = size(sh)

    def subShiftSE(ro: Row, x: Int): Row = {
      if(x > 0){
        Transparent::subShiftSE(ro, x - 1)
      }else ro
    }

    def makeEmpty(x: Int): Row = {
      if(x > 0) Transparent::makeEmpty(x - 1)
      else Nil
    }

    if(y > 0) makeEmpty(si._2 + x)::shiftSE(sh, x, y -1)
    else{
      sh match {
        case Nil => Nil
        case ro::ssh => subShiftSE(ro, x)::shiftSE(ssh, x, y)
      }
    }
  }


  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらしたshape を返す
  def shiftNW(sh: Shape, x: Int, y: Int): Shape = {
    val si: (Int, Int) = size(sh)

    def subShiftSE(ro: Row, x: Int): Row = {
      if(x > 0){
        subShiftSE(ro, x - 1) ++ List(Transparent)
      }else ro
    }

    def makeEmpty(x: Int): Row = {
      if(x > 0) Transparent::makeEmpty(x - 1)
      else Nil
    }

    if(y > 0) shiftNW(sh, x, y -1) ++ List(makeEmpty(si._2 + x))
    else{
      sh match {
        case Nil => Nil
        case ro::ssh => subShiftSE(ro, x)::shiftNW(ssh, x, y)
      }
    }
  }


  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す
  // 契約：shapeが拡大したものより大きさが小さいこと
  def padTo(sh: Shape, rows: Int, cols: Int): Shape = {
    val si: (Int, Int) = size(sh)
    assert(si._1 <= rows && si._2 <= cols)
    shiftNW(sh, cols - si._2, rows - si._1)
  }


  // 10. overlap
  // 目的：２つの shape が重なりを持つかを判断する
  def overlap(sh1: Shape, sh2: Shape): Boolean = {
    val si_1: (Int, Int) = size(sh1)
    val si_2: (Int, Int) = size(sh2)
    val mSi: (Int, Int) = (max(si_1._1, si_2._1), max(si_1._2, si_2._2))

    val aSh1 = padTo(sh1, mSi._1, mSi._2)
    val aSh2 = padTo(sh2, mSi._1, mSi._2)

    def subCheck(ro1: Row, ro2: Row): Boolean = {
      (ro1, ro2) match {
        case (Nil, Nil) => false
        case (x1::xs1, x2::xs2) => {
          if(x1 != Transparent && x2 != Transparent) true
          else subCheck(xs1, xs2) || false
        }
        case (_, _) => false
      }
    }
    
    def check(sh1: Shape, sh2: Shape): Boolean = {
      (sh1, sh2) match {
        case (Nil, Nil) => false
        case (x1::xs1, x2::xs2) => {
          if(subCheck(x1, x2)) true
          else check(xs1, xs2) || false
        } 
        case (_, _) => false
      }
    }

    check(aSh1, aSh2)
  }


  // 11. combine
  // 目的：２つの shape を結合する
  // 契約：２つの shape が重ならないこと
  def combine(sh1: Shape, sh2: Shape): Shape = {
    assert(!overlap(sh1, sh2))
    val si_1: (Int, Int) = size(sh1)
    val si_2: (Int, Int) = size(sh2)
    val mSi: (Int, Int) = (max(si_1._1, si_2._1), max(si_1._2, si_2._2))

    val aSh1 = padTo(sh1, mSi._1, mSi._2)
    val aSh2 = padTo(sh2, mSi._1, mSi._2) 

    def rowCombine(ro1: Row, ro2: Row): Row = {
      (ro1, ro2) match {
        case (Nil, Nil) => Nil
        case (x1::xs1, x2::xs2) => {
          if(x1 == Transparent) x2::rowCombine(xs1, xs2)
          else x1::rowCombine(xs1, xs2)
        }
        case (_, _) => Nil
      }
    }

    def mCombine(sh1: Shape, sh2: Shape): Shape = {
      (sh1, sh2) match {
        case (Nil, Nil) => Nil
        case (x1::xs1, x2::xs2) => rowCombine(x1, x2)::mCombine(xs1, xs2)
        case (_, _) => Nil
      }
    }

    mCombine(aSh1, aSh2)
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
  //自作
  println(duplicate(3, "7") == List("7", "7", "7"))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  //自作
  println(wellStructured(shapeS) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  println(wellStructured(rotate(shapeI)) == true)

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  //自作
  show(rotate(shiftSE(shapeJ, 2, 3)))


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
  //自作
  show(padTo(shapeL, 10, 10))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  //自作
  println(overlap(shapeO, shapeI) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

}
