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
  // 目的：整数 n と任意の型の値 a を受け取り、n 個の a からなるリストを作る
  def duplicate[A](n:Int, a:A):List[A]={
    if (n<=0) Nil
    else a::duplicate(n-1,a)
  }


  // 2. empty
  // 目的：rows 行 cols 列の空の shape を作る
  def empty(r:Int, c:Int):Shape={
    duplicate(r,duplicate(c,Transparent))
  }



  // 3. size
  // 目的：受け取った shape のサイズを (行数, 列数) の 形で返す
  def size(s:Shape):(Int, Int)={
    val r = s.length
    val c = {
      if (r==0) 0
      else s(0).length
    }
    (r, c)
  }


  // 4. blockCount
  // 目的：受け取った shape に含まれる空でないブロック の数を返す
  def blockCount(s:Shape):Int={
    def rCount(r:Row):Int={
      r match{
        case Nil => 0
        case x::xs => {
          if (x == Transparent) rCount(xs)
          else rCount(xs) + 1
        }
      }
    }
    s match{
      case Nil => 0
      case x::xs => rCount(x) + blockCount(xs)
    }
  }



  // 5. wellStructured
  // 目的：受け取った shape がまっとうであるかを判断する
  def wellStructured(s:Shape):Boolean={ 
    size(s) match{
      case (x, y) => {
        if (x == 0 || y == 0){
          false
        }
        else {
          var ret = true
          var i = 0
          while (i < x){
            ret = ret && (s(i).length == y)
            i = i + 1
          }
          ret
        }
      }
    }
  }



  // 6. rotate
  // 目的：受け取ったshape を反時計回りに90 度回転させたshape を返す
  // 契約：引数のshape はまっとうである
  def rotate(s:Shape):Shape={
    assert(wellStructured(s))
    def transpose(r:Row):Shape={
      r match{
        case Nil  => Nil
        case b::bs => List(b)::transpose(bs)
      }
    }
    def cplus(cx:Shape, cy:Shape):Shape={
      //同じ長さの列同士(またはNilと)の結合
      (cx, cy) match{
        case (Nil, Nil) => Nil
        case (cx, Nil) => cx
        case (Nil, cy) => cy
        case (x::xs, y::ys) => (x++y)::cplus(xs,ys)
      }
    }
    (s.map((r:Row) => transpose(r).reverse)).foldRight(Nil:Shape)((s1:Shape, s2:Shape) => cplus(s1,s2))
  }



  // 7. shiftSE
  // 目的：受け取ったshape を右にx, 下にy ずらしたshape を返す
  def shiftSE(s:Shape,x:Int, y:Int):Shape={
    def makelower(s:Shape):Shape={
      s match{
        case Nil => Nil
        case r::rs => (duplicate(x, Transparent) ++ r)::makelower(rs)
      }
    }
    val (sr,sc) = size(s)
    empty(y,x+sc) ++ makelower(s)
  }


  // 8. shiftNW
  // 目的：受け取ったshape を左にx, 上にy ずらしたshape を返す
  def shiftNW(s:Shape, x:Int, y:Int):Shape={
    def makeupper(s:Shape):Shape={
      s match{
        case Nil => Nil
        case r::rs => (r ++ duplicate(x, Transparent))::makeupper(rs)
      }
    }
    val (sr,sc) = size(s)
    makeupper(s) ++ empty(y,x+sc)
  }


  // 9. padTo
  // 目的：受け取ったshape をrows 行cols 列に拡大したshape を返す
  // 契約：rows, cols はshape の行数・列数以上
  def padTo(s:Shape, r:Int, c:Int):Shape={
    val (sr,sc) = size(s)
    assert((r>=sr)&&(c>=sc))
    shiftNW(s, c-sc, r-sr)
  }



  // 10. overlap
  // 目的：２つのshape が重なりを持つかを判断する
  def overlap(s1:Shape, s2:Shape):Boolean={
    val (r1,c1) = size(s1)
    val (r2,c2) = size(s2)
    def rpile(r1:Row, r2:Row):Boolean={
      (r1,r2) match{
        case (Nil, Nil) => false
        case (r1, Nil) => false
        case (Nil, r2) => false
        case (x::xs, y::ys) => {
          if ((x!=Transparent)&&(y!=Transparent)) true
          else false || rpile(xs,ys)
        }
      }
    }
    (s1,s2) match{
      case (Nil, Nil) => false
      case (s1, Nil) => false
      case (Nil, s2) => false
      case (a::as, b::bs) => rpile(a,b) || overlap(as,bs)
    }
  }


  // 11. combine
  // 目的：２つのshape を結合する
  // 契約：引数のshape は重なりを持たない
  def combine(s1:Shape, s2:Shape):Shape={
    assert(!overlap(s1,s2))
    def rcombine(r1:Row, r2:Row):Row={
      (r1,r2) match{
        case (Nil, Nil) => Nil
        case (r1, Nil) => r1
        case (Nil, r2) => r2
        case (x::xs, y::ys) => {
          if (x!=Transparent) x::rcombine(xs,ys)
          else y::rcombine(xs,ys)
        }
      }
    }
    (s1,s2) match{
      case (Nil, Nil) => Nil
      case (s1, Nil) => s1
      case (Nil, s2) => s2
      case (a::as, b::bs) => rcombine(a,b)::combine(as,bs)
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
  println(duplicate(2, 3.0) == List(3.0, 3.0))

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(2, 1) == List(List(Transparent),List(Transparent)))
  
  // 3. size
  println("size")
  println(size(Nil)== (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeJ) == (3, 2))
  
  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeJ) == 4)
  
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(wellStructured(shapeT) == true)
  
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  show(rotate(shapeJ))

  // rotate が満たすべき性質のテスト
  // 4回転で元どおりになる
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  // rotateさせた後もまっとう
  println(wellStructured(rotate(shapeS)))
  // 縦横が入れ替わる
  println(size(shapeS))
  println(size(rotate(shapeS)))
  // shape内の空でないブロックの数は変わらない
  println(blockCount(shapeT) == blockCount(rotate(shapeT)))


  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  show(shiftSE(shapeS, 2, 3))
  
  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeS, 3, 2))
  
  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeT, 4, 3))
  
  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(shapeS, shiftSE(shapeS, 2, 0)) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  show(combine(combine(shapeI, shiftSE(shapeZ, 1, 1)), shiftSE(shapeJ, 0, 2)))
}
