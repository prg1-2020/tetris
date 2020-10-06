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
  // 目的：return a list of size n filled with x
  def duplicate[A](n: Int, x: A): List[A] = {
    assert(n >= 0)
    if(n == 0)Nil
    else x :: duplicate(n-1, x)
  }


  // 2. empty
  // 目的：return a list of row x cols filled with "Transparent"
  def empty(r: Int, c: Int) = duplicate(r,duplicate(c,Transparent))



  // 3. size
  // 目的：return the size of shape in the form of (row, col)
  def size[A](shape: Shape) = {
    if(shape.length == 0)(0,0)
    (shape.length, shape.foldRight(0)((x,r)=>max(x.length,r)))
  }


  // 4. blockCount
  // 目的：return the number of nonempty blocks in shape
  def blockCount(shape: Shape) = {
    shape.foldRight(0)((x1, r1)=>{
      r1+x1.foldRight(0)((x2,r2)=>r2+(if(x2 == Transparent)0 else 1))
    })
  }


  // 5. wellStructured
  // 目的：return whether or not shape has the same number of elements in each row
  def wellStructured(shape: Shape) = {
    val (rows,cols) = size(shape)
    rows > 0 && cols > 0 &&  shape.foldRight(true)((x,r)=>x.length == cols && r)
  }



  // 6. rotate
  // 目的：rotate shape counterclockwise by 90 degrees
  // 契約：parameter shape is well structured
  def rotate(shape: Shape) = {
    assert(wellStructured(shape))
    val (rows,cols) = size(shape)
    (List.range(0, cols)) map { i =>
      (List.range(0,rows)) map { j =>
        shape(j)(cols-1-i)
      }
    }
  }


  // 7. shiftSE
  // 目的：return shape moved by (x,y)
  def shiftSE(shape: Shape, x: Int, y: Int) = {
    val (rows,cols) = size(shape)
    ((List.range(0, rows+y)) map { i =>
      (List.range(0,cols+x)) map { j =>
         if(i>=y && j>=x)shape(i-y)(j-x)
         else Transparent
      }
    })
  }


  // 8. shiftNW
  // 目的：return shape moved by (-x, -y)
  def shiftNW(shape: Shape, x: Int, y: Int) = {
    val (rows,cols) = size(shape)
    (List.range(0, rows+y)) map { i =>
      (List.range(0,cols+x)) map { j =>
        if(i<rows && j<cols)shape(i)(j)
        else Transparent
      }
    }
  }


  // 9. padTo
  // 目的：return shape expanded to size r x c
  // 契約：r >= rows && c >= cols
  def padTo(shape: Shape, r: Int, c: Int) ={
    val (rows, cols) = size(shape)
    assert(r >= rows && c>=cols)
    shiftNW(shape, c-cols, r-rows)
  }



  // 10. overlap
  // 目的：return whether two shapes overlap or not
  def overlap(shape1: Shape, shape2: Shape) = {
    val (rows_1, cols_1) = size(shape1)
    val (rows_2, cols_2) = size(shape2)
    val mi_rows = if(rows_1 < rows_2) rows_1 else rows_2
    val mi_cols = if(cols_1 < cols_2) cols_1 else cols_2
    List.range(0,mi_rows).exists(i => {
      List.range(0,mi_cols).exists(j => shape1(i)(j) != Transparent && shape2(i)(j) != Transparent)
    })
  }


  // 11. combine
  // 目的：combine two shapes into one and return
  // 契約：two shapes must not overlap
  def combine(shape1: Shape, shape2: Shape) = {
    assert(!overlap(shape1, shape2))
    val (rows_1, cols_1) = size(shape1)
    val (rows_2, cols_2) = size(shape2)
    val mx_rows = max(rows_1, rows_2)
    val mx_cols = max(cols_1, cols_2)
    List.range(0,mx_rows).map(i => {
      List.range(0,mx_cols).map(j => {
        if(i<rows_1 && j<cols_1 && shape1(i)(j) != Transparent)shape1(i)(j)
        else if(i<rows_2 && j<cols_2 && shape2(i)(j) != Transparent)shape2(i)(j)
        else Transparent
      })
    })
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

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  assert(empty(0,0) == Nil)

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


  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  assert(rotate(List(List(Transparent,Red,Transparent),List(Transparent,Blue,Blue),List(Red,Red,Transparent)))
        == List(List(Transparent,Blue,Transparent),List(Red,Blue,Red),List(Transparent,Transparent,Red))
  )

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
    show(combine(shiftSE(shapeI, 0, 1), shapeZ))
}
