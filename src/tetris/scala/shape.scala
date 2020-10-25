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
import java.awt.Transparency
import java.security.KeyStore.TrustedCertificateEntry

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
  // 目的：整数 n と任意の型の値 a を受け取り、n 個の a からなるリストを作る
  def duplicate[A](n : Int, a : A): List[A] = {
      if(n == 0)  Nil
      else  a :: duplicate(n-1, a)
  }


  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る
  def empty(rows : Int, cols : Int): Shape = {
    if(rows == 0 && cols == 0) Nil
    else duplicate(rows, duplicate(cols, Transparent : Color))
  }


  // 行のうち要素数が最大なものの要素数を返す関数、3.sizeと5.wellstructuredで用いる。
  def maxRowLength(shape : Shape):  Int = {
    shape match {
    case Nil => 0
    case x::xs => max(x.length,maxRowLength(xs))
    }
  }
  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す関数
  def size(shape : Shape): (Int,Int) = {
      (shape.length,maxRowLength(shape))
  }



  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す
  def blockCount(shape : Shape): Int = {
    def blockCountPerRow(row : Row): Int = {
      row match {
      case Nil =>  0
      case x :: xs => if(x == Transparent) blockCountPerRow(xs)
                      else 1 + blockCountPerRow(xs)
      }
    }
    shape match {
      case Nil => 0
      case x :: xs => blockCountPerRow(x) + blockCount(xs)
    }
  }


  // 5. wellStructured
  // 目的：受け取ったshapeが行数・列数がともに１以上であり、各行の要素数が全て等しいかどうか判定する。
  def wellStructured(shape : Shape): Boolean = { 
    if(blockCount(shape) == 0) false
    else shape match {
      case Nil => true //この行は理論上は要らないが注意されるのでつけた。
      case row :: Nil => true
      case row1 :: row2 :: rows => if(row1.length != row2.length) false
      else  wellStructured(row2::rows)
    } 
  }



  // 6. rotate
  // 目的：受け取った shape を反時計回りに 90 度回転させた shape を返す。
  // 契約：引数の shape はまっとうである
  def rotate(shape: Shape): Shape = {
    def allRowReverse(subShape: Shape): Shape ={
      subShape match {
        case Nil => Nil
        case r :: rs => r.reverse +: allRowReverse(rs)
      }
    }
    allRowReverse(shape).transpose
  }



  // 7. shiftSE
  // 目的：受け取った shape を右に x, 下に y ずらしたshape を返す
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    def addNilToHead(shape: Shape,x: Int): Shape = {
      shape match {
          case Nil => Nil
          case r :: rs => List.fill(x)(Transparent:Color) ++ r :: addNilToHead(rs,x)
      }
    }
    empty(y,maxRowLength(shape)+x) ++ addNilToHead(shape,x)
  }



  // 8. shiftNW
  // 目的：受け取った shape を左に x, 上に y ずらしたshape を返す
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    def addNilToLast(shape: Shape,x: Int): Shape = {
      shape match {
          case Nil => Nil
          case r :: rs => r ++ List.fill(x)(Transparent:Color) :: addNilToLast(rs,x)
      }
    }
    addNilToLast(shape,x) ++ empty(y,maxRowLength(shape)+x)
  }



  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す
  // 契約：rows, cols は shape の行数・列数以上
    def padTo(shape : Shape, rows: Int, cols: Int):Shape = {
      val(x,y):(Int,Int) = size(shape)
      shiftNW(shape,cols-y,rows-x)
    }



  // 10. overlap
  // 目的：２つの shape が重なりを持つかを判断する
  def overlap(shape1: Shape, shape2: Shape):Boolean = {
    def overlapRow(row1: Row, row2: Row):Boolean = {
      (row1, row2) match {
        case (Nil,_) => false
        case (_,Nil) => false
        case (x::xs,y::ys) => if(x != Transparent && y != Transparent) true 
        else overlapRow(xs,ys)
      }
    }
    (shape1,shape2) match {
      case (Nil,_) => false
      case (_,Nil) => false
      case (r1::rs1,r2::rs2) => if(overlapRow(r1,r2)) true
      else overlap(rs1,rs2)
    }
  }



  // 11. combine
  // 目的：二つのshapeを結合させる
  // 契約：引数の shape は重なりを持たない
  def combine(shape1:Shape,shape2:Shape): Shape = {
    def subCombine(shape1:Shape,shape2:Shape): Shape = {
      if(overlap(shape1,shape2)) Nil else{
        def combineRow(r1:Row,r2:Row): Row = {
          (r1,r2) match {
            case (Nil,Nil) => Nil
            case (Nil,ys) => ys
            case (xs,Nil) => xs
            case (x::xs,y::ys) => if(x==Transparent) y::combineRow(xs,ys)
            else x::combineRow(xs, ys)
          }
        }
        (shape1,shape2) match {
          case (Nil,Nil) => Nil
          case (Nil,rs2) => rs2
          case (rs1,Nil) => rs1
          case (r1::rs1,r2::rs2) => combineRow(r1,r2) :: combine(rs1,rs2)
        }
      }
    }
    subCombine(subCombine(shape1,shape2),empty(subCombine(shape1,shape2).length,maxRowLength(subCombine(shape1,shape2))))
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
  println(duplicate(5, 5) == List(5, 5, 5, 5, 5))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 2) == List(List(Transparent, Transparent),List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(empty(3, 3)) == (3, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeT) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red))) == true)
  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeZ)))) == shapeZ)
  println(size(shapeT) == size(rotate(rotate(shapeT))))
  println(blockCount(shapeJ) == blockCount(rotate(shapeJ)))
  
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
  println(overlap(shapeI, shiftNW(shapeI, 10, 3)) == true)

  
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  println(combine(shapeO, shapeJ)==Nil)
}