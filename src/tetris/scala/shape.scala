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
  // 目的：整数nと任意の型の値aを受け取りn個のaのリストを返す
  def duplicate[A](n: Int, a: A): List[A] = {
    if (n <= 0) Nil
    else a :: duplicate(n-1, a)
  }



  // 2. empty
  // 目的：row行cols列の空のshapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }



  // 3. size
  // 目的：受け取ったshapeのサイズを(行数, 列数)の形で返す
  def size(s: Shape): (Int, Int) = {
    (s.length, s.foldRight(0)((a, b) => max(a.length, b)))
  }



  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  def blockCount(s: Shape): Int = {
    s.foldRight(0)((a, b) => b + a.foldRight(0)((c, d) => if(c != Transparent) d+1 else d ))
  }



  // 5. wellStructured
  // 目的：受け取ったshapeがまっとうであるかを判断する
  def wellStructured(x: Shape): Boolean = {
     val (r, c) = size(x)
     r >= 1 && c >= 1 && x.foldRight(true)((a, b) => b && c == a.length)
   }


  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：引数のshapeはまっとうである。
  def rotate(sh: Shape): Shape = {
     assert(wellStructured(sh))
     val (rows, cols) = size(sh)
     Range(0, cols).toList.map(i => {
       Range(0, rows).toList.map(j => {
         sh(j)(cols-i-1)
       })
    })
  }



  // 7. shiftSE
  // 目的：受け取ったshapeを右にx、左にyずらしたshapeを返す
  def shiftSE(s: Shape, x: Int, y: Int): Shape = {
    val(r, c) = size(s)
    (List.range(0, r +y)) map{
      a => (List.range(0, c +x)) map{
        b => if(y <= a && x <= b) s(a-y)(b-x)
        else Transparent
      }
    }
  }
    



  // 8. shiftNW
  // 目的：受け取ったshapeを左にx、上にyずらしたshapeを返す
  def shiftNW(s: Shape, x: Int, y: Int): Shape = {
    val(r, c) = size(s)
    (List.range(0, r +y)) map{
      a => (List.range(0, c +x)) map{
        b => if(a<r && b<c) s(a)(b)
        else Transparent
      }
    }
  }



  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す
  // 契約：rows,colsはshapeの行数・列数以上
  def padTo(s: Shape, rows: Int, cols: Int) = {
    val (r, c) = size(s)
    assert(r<= rows && c<= cols)
    shiftNW(s, cols -c, rows - r)
  }



  // 10. overlap
  // 目的：2つのshapeが重なりを持つかを判断する
  def overlap(a: Shape, b: Shape): Boolean = {
    val (r1, c1) = size(a)
    val (r2, c2) = size(b)
    def sub(r1:Row, r2: Row):Boolean = {
      (r1, r2) match {
        case (Nil, Nil) => false
        case (ri, Nil) => false
        case (Nil, r2) => false
        case (x::xs, y::ys) => {
          if ((x != Transparent)&&(y != Transparent)) true
          else false || sub(xs, ys)
        }
      }
    }
    (a, b) match{
      case (Nil, Nil) => false
      case (a, Nil) => false
      case (Nil, b) => false
      case (x::xs, y::ys) => sub(x,y) || overlap(xs, ys)
    }
  }





  // 11. combine
  // 目的：2つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
  def combine(x:Shape, y:Shape):Shape={
     assert(!overlap(x,y))
     def subcombine(r1:Row, r2:Row):Row={
       (r1,r2) match{
         case (Nil, Nil) => Nil
         case (r1, Nil) => r1
         case (Nil, r2) => r2
         case (a::as, b::bs) => {
           if (a!=Transparent) a::subcombine(as,bs)
           else b::subcombine(as,bs)
         }
       }
     }
     (x,y) match{
       case (Nil, Nil) => Nil
       case (x, Nil) => x
       case (Nil, y) => y
       case (p::ps, q::qs) => subcombine(p,q)::combine(ps,qs)
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
  //自作
  println(duplicate(4, "a") == List("a", "a", "a", "a"))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  //自作
  println(empty(2,2) == List(List(Transparent, Transparent), List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  //自作
  println(size(shapeT) == (2, 3))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  //自作
  println(blockCount(shapeT) == 4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  //自作
  println(wellStructured(shapeT) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  //自作
  show(rotate(shapeT))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeT)))) == shapeT)
  println(wellStructured(rotate(shapeT)) == true)



  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  //自作
  show(shiftSE(shapeT, 1, 3))

  // 8. shiftNW
  println("shiftNW")
  /*println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))*/
  show(shiftNW(shapeI, 1, 2))
  //自作
  show(shiftNW(shapeT, 2, 3))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  //自作
  show(padTo(shapeT, 5, 6))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  //自作
  println(overlap(shapeS, shiftSE(shapeS, 2, 0)) == false)


  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  //自作
  show(combine(shiftSE(shapeO, 2, 1), shapeJ))

}
