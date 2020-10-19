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
  // 目的：n個のaからなるリストを作る
  def duplicate[A](n:Int,a:A): List[A] = {
    if (n <= 0) Nil
    else a :: duplicate(n-1,a)
  }


  // 2. empty
  // 目的：rows行cols列の空のshapeを作る
  def empty(rows:Int,cols:Int): Shape = {
    duplicate(rows,duplicate(cols,Transparent))
  }



  // 3. size
  // 目的：shapeのサイズを（行数,列数）の形で返す
  def size(s:Shape): (Int,Int) = {
    (s.length,s.foldRight(0)((a,b) => max(a.length,b)))
    //列数は畳み込みで最大値を計算する
  }



  // 4. blockCount
  // 目的：shapeに含まれる空でないブロックの数を求める
  def blockCount(s:Shape): Int = {
    s.foldRight(0)((a,b) => b+a.foldRight(0)((c,d)=> if(c == Transparent) d else d+1 ))
  }



  // 5. wellStructured
  // 目的：受け取ったshapeがまっとうであるかを判断する
  def wellStructured(s:Shape): Boolean = {
    val (a,b) = size(s)
    a >= 1 && b >= 1 && s.foldRight(true)((c, d) => d && b == c.length)
  }



  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを求める
  // 契約：引数のshapeはまっとうである 
  def rotate(s: Shape): Shape = {
    assert(wellStructured(s))
    val (rows,cols) = size(s)
    //List.rangeとmap関数を用いてsizeが(clos,rows)のShapeを作成し、引数のshapeから要素を入れていく
    //戻り値のshapaの(i,j)マスは引数のshapeの(j,cols-i-1)マスに等しい
    (List.range(0,cols)) map { i => (List.range(0,rows)) map { j => s(j)(cols - i - 1)}}
  }


  // 7. shiftSE
  // 目的：受け取ったshapeをみぎにx,下にyずらしたshapeを求める
  def shiftSE(s:Shape,x: Int,y: Int): Shape ={
    val (rows,cols) = size(s)
    //sizeが(rows+y,clos+x)のshapeを作成。何もないところはTransparentでうめる。
    (List.range(0,rows+y)) map { i => 
      (List.range(0,cols+x)) map { j => 
        if (i >= y && j >= x) s(i-y)(j-x)
        else Transparent
      }
    }
  }


  // 8. shiftNW
  // 目的：受け取ったshapeを左にx,上にyずらしたshapeを求める。
  def shiftNW(s:Shape,x: Int,y: Int): Shape ={
    val (rows,cols) = size(s)
    (List.range(0,rows+y)) map { i => 
      (List.range(0,cols+x)) map { j => 
        if (i >= rows || j >= cols) Transparent
        else s(i)(j)
      }
    }
  }


  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを求める
  // 契約：rows,colsは引数のshapeのそれ以上
  def padTo(s:Shape,rows:Int,cols:Int): Shape ={
    val (s_rows, s_cols) = size(s)
    assert(rows >= s_rows && cols >= s_cols)
    shiftNW(s,cols - s_cols,rows - s_rows)
  }



  // 10. overlap
  // 目的：受け取った2つのshaoeが重なりを持つかを判断する
  def overlap(_s1:Shape,_s2:Shape): Boolean ={
    //補助関数
    //目的:受け取った2つのshapeの重なりがある部分をRed,ない部分をTransparentとするshapeを返す
    def overlap_sub(s1:Shape,s2:Shape): Shape ={
      val (rows1,cols1) = size(s1)
      val (rows2,cols2) = size(s2)
      val max_rows = max(rows1,rows2)
      val max_cols = max(cols1,cols2)
      val S1 = shiftNW(s1,max_rows,max_cols)
      val S2 = shiftNW(s2,max_rows,max_cols)
      (List.range(0,max_rows)) map { i =>
        (List.range(0,max_cols)) map { j =>
          if (S1(i)(j) != Transparent && S2(i)(j) != Transparent) Red
          else Transparent
        }
      }
    }

    blockCount(overlap_sub(_s1,_s2)) > 0
  }



  // 11. combine
  // 目的：受け取った2つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
  def combine(s1:Shape,s2:Shape): Shape ={
    assert(overlap(s1,s2) == false)
    val (rows1,cols1) = size(s1)
    val (rows2,cols2) = size(s2)
    val max_rows = max(rows1,rows2)
    val max_cols = max(cols1,cols2)
    val S1 = shiftNW(s1,max_rows,max_cols)
    val S2 = shiftNW(s2,max_rows,max_cols)
    (List.range(0,max_rows)) map { i =>
      (List.range(0,max_cols)) map { j =>
        if(S1(i)(j) != Transparent) S1(i)(j)
        else if(S2(i)(j) != Transparent) S2(i)(j)
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
  //自作テスト
  println(duplicate(2, 123) == List(123,123))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  //自作テスト
  println(empty(2, 2) == List(List(Transparent, Transparent),List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(shapeZ)
  //自作テスト
  println(size(shapeO) == (2,2)) 

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  //自作テスト
  println(blockCount(shapeO) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  //自作テスト
  println(wellStructured(shapeO) == true)

  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  //自作テスト
  println(rotate(List(List(Red,Blue),List(Blue,Red))) == List(List(Blue,Red),List(Red,Blue)))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)
  println(wellStructured(rotate(shapeJ)) == true)
  println(blockCount(rotate(shapeJ)) == blockCount(shapeJ))


  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  //自作テスト
  show(shiftSE(shapeZ,1,2))
  //期待される出力
  /*
  ....
  ....
  .ZZ.
  ..ZZ
  */
  
  
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  //自作テスト
  show(shiftNW(shapeZ,1,2))
  //期待される出力
  /*
  ZZ..
  .ZZ.
  ....
  ....
  */

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  //自作テスト
  show(shiftNW(shapeJ,1,2))
  //期待される出力
  /*
  .J.
  .J.
  JJ.
  ...
  ...
  */


  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  //自作テスト
  println(overlap(shapeI,shapeJ) == true)

  
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  //自作テスト
  show(combine(shapeI,shiftSE(shapeZ,1,1)))
  //期待される出力
  /*
  I...
  IZZ.
  I.ZZ.
  I...
  */
}
