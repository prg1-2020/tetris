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
  // 目的：整数 n と任意の型の値 a を受け取り, n 個の a からなるリストを返す
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n == 0) Nil
    else a :: duplicate(n - 1, a)
  }




  // 2. empty
  // 目的：rows 行 cols 列の空の shape を返す
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows,duplicate(cols, Transparent))
  }



  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の形で返す
  def size(shape: Shape): (Int,Int) = {
    def sizeAcc(shape:Shape,rows: Int,cols :Int): (Int, Int) = {
      shape match{
        case Nil => (rows,cols)
        case x :: xs => sizeAcc(xs,rows,max(cols,x.length))
      }
    }
    sizeAcc(shape,shape.length,0)
  }



  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロックの数を返す
  def blockCount(shape: Shape): Int = {
    def colCount(cols: Row): Int = {
      cols match{
        case Nil => 0
        case x :: xs =>{
          if(x != Transparent){
            1 + colCount(xs)
          }
          else colCount(xs)
        }
      }
    }

    def shapeCount(shape: Shape): Int = {
      shape match{
        case Nil => 0
        case x :: xs => colCount(x) + shapeCount(xs)
      }
    }

    shapeCount(shape)
  }



  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるか返す
    def wellStructured(shape: Shape): Boolean = {
    def wellStructuredAcc(shape: Shape,p: Boolean,n: Int): Boolean = {
      shape match{
        case Nil => p
        case x :: xs => wellStructuredAcc(xs,p && x.length == n,n)
      }
    }

    val (rows, cols) = size(shape)
    var p = rows >= 1 && cols >= 1
    shape match{
      case Nil => false
      case x :: xs => wellStructuredAcc(xs,p,x.length)
    }
  }




  // 6. rotate
  // 目的：受け取ったshape を反時計回りに90 度回転させたshape を返す
  // 契約：引数のshape はまっとうである
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape) == true) //契約
    def allReverse(shape: Shape): Shape = {
      shape match{
        case x :: xs => x.reverse :: allReverse(xs)
        case Nil => Nil
      }
    }
    def ColToRow(shape: Shape,n: Int): Row = {
      shape match{
        case x :: xs => x(n) :: ColToRow(xs,n)
        case Nil => Nil
      }
    }
    val (x,y) = size(shape)
    def transpose(shape: Shape,n: Int): Shape = {
      if(n < y) ColToRow(shape,n) :: transpose(shape,n+1)
      else Nil
    }
    transpose(allReverse(shape),0)
  }

  // 7. shiftSE
  // 目的：受け取ったshape を右に x ,下に y ずらした shape を返す
  def shiftSE(shape: Shape,x: Int, y: Int): Shape = {
    val (rows, cols) = size(shape)
    (empty(y,cols) ++ shape).map(row =>duplicate(x,Transparent) ++ row)
  }

  // 8. shiftNW
  // 目的：受け取ったshape を左に x, 上に y ずらした shape を返す
  def shiftNW(shape: Shape,x: Int, y: Int): Shape = {
    val (rows, cols) = size(shape)
    (shape ++ empty(y,cols)).map(row =>row ++ duplicate(x,Transparent))
  }


  // 9. padTo
  // 目的：受け取った shape を rows 行 cols 列に拡大した shape を返す関
  // 契約：rows, cols はshape の行数・列数以上
  def padTo(shape: Shape,rows: Int, cols: Int): Shape = {
    val (x, y) = size(shape)
    assert(rows >= x && cols >= y) // 契約
    shiftNW(shape, cols - y, rows - x)
  }



  // 10. overlap
  // 目的：２つのshape が重なりを持つかを判断する
  def overlap(shape1: Shape,shape2: Shape): Boolean = {
    def overlap_row(row1: Row, row2: Row,p1 : Boolean): Boolean = {
      (row1, row2) match{
        case(x :: xs, y :: ys) => overlap_row(xs,ys,p1 && (x == Transparent || y == Transparent))
        case _ => p1
      } 
    }
    def overlap_acc(shape1: Shape,shape2: Shape,p2: Boolean): Boolean = {
      (shape1,shape2) match{
        case(x :: xs, y :: ys) => overlap_acc(xs,ys,p2 && overlap_row(x,y,p2))
        case _ => p2
      }
    }
    !overlap_acc(shape1,shape2,true)
  }

  // 11. combine
  // 目的：２つのshape を結合して返す
  // 契約：引数のshape は重なりを持たない
  def combine(shape1: Shape,shape2: Shape): Shape = {
    assert(overlap(shape1,shape2) == false) //契約
    def row_combine(row1: Row,row2: Row): Row = {
      (row1, row2) match{
        case(x :: xs, y :: ys) =>{
          if(y == Transparent) x :: row_combine(xs,ys)
          else y :: row_combine(xs,ys)
        }
        case(Nil,y::ys) => row2
        case(x::xs,Nil) => row1
        case(Nil,Nil) => Nil
      }
    }
    val (x1,y1) = size(shape1)
    val (x2,y2) = size(shape2)   
    def make_combine(shape1: Shape,shape2: Shape): Shape = {
      (shape1,shape2) match{
        case(x :: xs, y :: ys) => row_combine(x,y):: make_combine(xs,ys)
        case(x :: xs,Nil) => row_combine(x,duplicate(y2,Transparent)):: make_combine(xs,Nil)
        case(Nil,y::ys) => row_combine(duplicate(y1,Transparent),y):: make_combine(Nil,ys)
        case(Nil,Nil) => Nil
      }
    }
    make_combine(shape1,shape2)
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
  println(duplicate(5, ":(") == List(":(",":(",":(",":(",":("))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 2) == List(List(Transparent,Transparent),List(Transparent,Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeO) == (2, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeS) == 4)


  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(List(List(Red, Red, Red), List(Transparent, Red, Transparent))) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeT))
  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeT)))) == shapeT)//4回転で元に戻る
  println(blockCount(rotate(shapeJ)) == blockCount(shapeJ))//個数の不変性
  
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
  show(shiftNW(shapeL, 3, 1))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeT, 2, 4))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeL, shiftSE(shapeT, 1, 1)) == false)

  // 11. combine
  println("combine")
  
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(shapeL, shiftSE(shapeT, 1, 1)))
}