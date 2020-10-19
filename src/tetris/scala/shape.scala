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
  // 目的：n個のxからなるListの作成
  def duplicate[A](n: Int, x: A): List[A] = {
    if(n <= 0) Nil
    else x::duplicate(n-1, x)
  }



  // 2. empty
  // 目的：N*Mの空のshapeの作成 
  def empty(n: Int, m: Int): Shape = {
    duplicate(n, duplicate(m, Transparent))
  }


  // 3. size
  // 目的：s:Shapeのsizeを返す
  def size(s: Shape): (Int, Int) = {
    assert(wellStructured(s))
    if(s.length == 0)
      (0, 0)
    else
      (s.length, s.head.length)
  }



  // 4. blockCount
  // 目的：透明でない部分の個数を返す。
  def blockCount(s: Shape): Int = {
    if(s.length == 0)
      0
    else
      s.map(_.map(x => if(x==Transparent) 0 else 1).foldLeft(0)(_+_)).foldLeft(0)(_+_)
  }



  // 5. wellStructured
  // 目的:s:Shapeがemptyで無い長方形状かを調べる -> Boolean
  def wellStructured(s: Shape): Boolean = {
    if(s.length == 0)
      false
    else
      s.head.length>0 && s.map(_.length==s.head.length).foldLeft(true)(_&&_)
  }



  // 6. rotate
  // 目的：s:Shapeを90°回転させる。 Shape -> Shape
  // 契約：sはwellStructed
  def rotate(s: Shape): Shape = {
    assert(wellStructured(s))
    def t(thisS: Shape): Shape = {
      if(thisS.head.length == 0)
        thisS
      else
        thisS.map(_.head)::t(thisS.map(_.tail))
    }
    t(s).map(_.reverse).filter(_.nonEmpty)
  }



  // 7. shiftSE
  // 目的：s:Shapeを右にx, 下にyずらす。 Int, Int -> Shape
  def shiftSE(s: Shape, x: Int, y: Int): Shape = {
    (duplicate(y, duplicate(s.head.length, Transparent)) ++ s).map(duplicate(x, Transparent) ++ _)
  }



  // 8. shiftNW
  // 目的：目的：s:Shapeを左にx, 上にyずらす。 Int, Int -> Shape
  def shiftNW(s: Shape, x: Int, y: Int): Shape = {
    (s ++ duplicate(y, duplicate(s.head.length, Transparent))).map(_ ++ duplicate(x, Transparent))
  }


  // 9. padTo
  // 目的：shapeをTransparent関係なくrows x colsに拡大する。
  // 契約：rows ≥ row o' shape, cols ≥ coln o' shape
  def padTo(s: Shape, rows: Int, cols: Int): Shape = {
    val sr = s.length
    val sc = s.head.length
    assert(rows >= sr && cols >= sc)
    shiftNW(s, cols-sc, rows-sr)
  }


  // 10. overlap
  // 目的：2つのshapeの左上を重ねた時に重なりを持つか。 -> Boolean
  def overlap(s1: Shape, s2: Shape): Boolean = {
    //同型へ。ブロックのあるマスをtrueにする。
    val maxr = max(s1.length, s2.length)
    val maxc = max(s1.head.length, s2.head.length)
    val b1 = padTo(s1, maxr, maxc).map(_.map(x => if(x==Transparent) false else true))
    val b2 = padTo(s2, maxr, maxc).map(_.map(x => if(x==Transparent) false else true))

    //すべての要素で両図形でtrueが存在するブロックがあればtrueを返す。
    !(b1 corresponds b2){(r,c)=> (r corresponds c){(block1, block2) => ! (block1 && block2)}}
  }


  // 11. combine
  // 目的：2つのshapeを結合する。
  // 契約：!overlap
  def combine(s1: Shape, s2: Shape): Shape = {
    def tuple2DtoList[T](t: (T,T)): List[T] = List(t._1, t._2)
    assert(!overlap(s1, s2))
    val maxr = max(s1.length, s2.length)
    val maxc = max(s1.head.length, s2.head.length)
    
    def f(this1: Shape, this2: Shape): Shape = {
      (this1,this2) match{
        case (t1::ts1, t2::ts2) =>
          (t1.zip(t2)).map(tuple2DtoList).map((lis)=>if(lis.head==Transparent) lis.last else if(lis.last==Transparent) lis.head else Transparent) :: f(ts1, ts2)
        case (Nil, Nil) =>
          Nil
      }
    }

    f(padTo(s1, maxr, maxc), padTo(s2, maxr, maxc))
    
  }



}

// テスト
object ShapeTest extends App {
  import ShapeLib._

  // 関数を定義するたびに、コメント開始位置を後ろにずらす
/*
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
*/
  // rotate が満たすべき性質のテスト
  println(shapeZ.head.head == rotate(shapeZ).last.head)
  def test(a:(Int,Int), b:(Int,Int)): Boolean ={
    (a,b) match{
      case ((a1,a2),(b1,b2)) =>
        a1 == b2 && a2 == b1
      case _ =>
        false
    }
  }
  println(test(size(shapeT), size(rotate(shapeT))))

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))

  show(shiftSE(shapeT, 2, 3))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  show(shiftSE(shapeZ, 2, 1))

  show(shiftSE(shiftNW(shapeZ, 2, 1), 2, 1))

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

  println(overlap(shapeT, shapeO))
  println(overlap(shapeT, shiftSE(shapeO, 2, 1)))

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

  show(combine(shiftSE(rotate(shapeS),0,1), shapeZ))
}
