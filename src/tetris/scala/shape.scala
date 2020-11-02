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
  def duplicate[T](n: Int, a: T): List[T] = {
      if(n <= 0) Nil
      else a :: duplicate(n-1, a)
    }

  // 2. empty
  // 目的：空の shape を作る
def empty(x: Int, y : Int): Shape = {
    duplicate(x, duplicate(y, Transparent))
}

  // 3. size
  // 目的：shape のサイズを(行数, 列数)の形で返す
def size(shape: Shape): (Int, Int) = {
    //目的 : 2つの shape のサイズを足し合わせる
    def addSize(l1: (Int, Int), l2: (Int, Int)) : (Int, Int) = {
        (l1, l2) match {
            case ((x1, x2), (y1, y2)) => (x1 + y1, max(x2, y2))
        }
    }
    shape match {
        case Nil => (0, 0)
        case x :: xs => addSize((1, x.length), size(xs))
    }
}

  // 4. blockCount
  // 目的：shape に含まれる空でないブロックの数を返す
def blockCount(shape: Shape): Int = {
    shape match {
        case Nil => 0
        case x :: xs =>  rowBlockCount(x) + blockCount(xs)
    }
}
  //目的 : rowに含まれる空でないブロックの数を返す
def rowBlockCount(r: Row): Int = {
    r match {
        case Nil => 0
        case Transparent :: rs => rowBlockCount(rs)
        case _ :: rs => 1 +  rowBlockCount(rs)
    }
}




  // 5. wellStructured
  // 目的：shape がまっとうであるかを判断する
def wellStructured(shape: Shape): Boolean = {
    val (x: Int, y: Int) = size(shape)
    def rwellStructured(s: Shape): Boolean = {
        s match {
            case Nil => true
            case l :: ls => if (l.length != y) false else rwellStructured(ls)
        }
    }
    if(x <= 0 || y <= 0) false else rwellStructured(shape)
}



  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：引数のshapeはまっとうである
def rotate(shape : Shape): Shape = {
       assert(wellStructured(shape) == true)
       //目的：rowの各要素をshapeの各要素のリストに結合させる
       def connect0(n: Row, m: Shape): Shape = {
            (n, m) match {
                case (n1::ns, m1::ms) => (n1 :: m1) :: connect0(ns, ms) 
                case (n1::ns, Nil) => List(n1) :: connect0(ns, Nil)
                case (Nil, Nil) => Nil
            }
       }
       shape match {
           case Nil => Nil
           case x :: Nil => connect0(x.reverse, Nil)
           case x :: xs => connect0(x.reverse, rotate(xs))
      }     
}


  // 7. shiftSE
  // 目的：shapeを右にx、下にyずらしたshapeを作る
def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    size(shape) match {
        case (a, b) => empty(y, x + b) ++ connect(empty(a, x), shape)
        case _ => Nil
    }
}

// 目的 : 2つの shape の各要素List[Block]を結合させる
def connect(n: Shape, m: Shape): Shape = {
            (n, m) match {
                case (n1::ns, m1::ms) => n1 ++ m1 :: connect(ns, ms) 
                case (_, Nil) => Nil
                case (Nil, _) => Nil
            }
}


  // 8. shiftNW
  // 目的：shapeを左にx、上にyずらしたshapeを作る
def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    size(shape) match {
        case (a, b) => connect(shape, empty(a, x)) ++ empty(y, x + b)
        case _ => Nil
    }
}



  // 9. padTo
  // 目的：shape をrows 行 cols 列に拡大する
  // 契約：rows, cols は shape の行数・列数以上
def padTo(shape: Shape, r: Int, c: Int): Shape = {
    val (x: Int, y: Int) = size(shape)
    assert(x <= r && y <= c)
    shiftNW(shape, c - y, r - x)
}


  // 10. overlap
  // 目的：2つの shape が重なりを持つか判断する
def overlap(shape1: Shape, shape2: Shape): Boolean = {
    // 目的 : 2つの row が重なりを持つか調べる
    def rOverlap(row1: Row, row2: Row): Boolean = {
        (row1, row2) match {
            case (Nil, _) => false
            case (_, Nil) => false
            case (r1 :: rs1, r2 :: rs2) => 
                if (r1 != Transparent && r2 != Transparent) true 
                else if (rOverlap(rs1, rs2) == true) true
                else false
          }
    }
    (shape1, shape2) match {
        case (Nil, _)  => false
        case (_ , Nil) => false
        case (x1 :: xs1, x2 :: xs2) => 
            if (rOverlap(x1, x2) == true) true
            else overlap(xs1, xs2)
    }
}

  // 11. combine
  // 目的：2つの shape を結合する
  // 契約：引数の shape は重なりを持たない
def combine(s1: Shape, s2: Shape): Shape = {
    assert(overlap(s1, s2) == false)
    val (rsize1: Int, csize1: Int) = size(s1)
    val (rsize2: Int, csize2: Int) = size(s2)
    val rsizeMax = max(rsize1, rsize2)
    val csizeMax = max(csize1, csize2)
    val news1 = padTo(s1, rsizeMax, csizeMax)
    val news2 = padTo(s2, rsizeMax, csizeMax)
    //目的: 再帰用の関数
    def combine0(shape1: Shape, shape2: Shape): Shape = {
        (shape1, shape2) match {
            case (Nil, Nil) => Nil
            case (x1 :: xs1, x2 :: xs2) => rcombine(x1, x2) :: combine0(xs1, xs2)
        }
    }
    //目的: 2つのブロックの結合
    def bcombine(block1: Block, block2: Block): Block = {
        (block1, block2) match {
            case (Transparent, Transparent) => Transparent
            case (Transparent, _) => block2
            case (_, Transparent) => block1
        }
    } 
    //目的: 2つの列の結合
    def rcombine(row1: Row, row2: Row): Row = {
        (row1, row2) match {
            case (Nil, Nil) => Nil
            case (r1 :: rs1, r2 :: rs2) => bcombine(r1, r2) :: rcombine(rs1, rs2)
        }
    }
    combine0(news1, news2)
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
  println(duplicate(2, (1, 2, 3)) == List( (1, 2, 3), (1, 2, 3) ))


  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(empty(3, 2) == List(List(Transparent, Transparent), List(Transparent, Transparent), List(Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(size(shapeT) == (2, 3)) 
  
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
  println(wellStructured(List(List(Red, Red))) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))
  println(rotate(rotate(List(List(Red, Blue, Transparent), List(Transparent, Yellow, Blue)))) == List(List(Blue, Yellow, Transparent), List(Transparent, Blue, Red))) 

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeS)))) == shapeS)
  println(rotate(shapeS) != shapeS)
  println(rotate(List(List(Red, Red, Transparent), List(Transparent, Red, Red))) == List(List(Transparent, Red), List(Red, Red), List(Red, Transparent)))
  println(rotate(rotate(List(List(Red, Blue, Transparent), List(Transparent, Yellow, Blue)))) == List(List(Blue, Yellow, Transparent), List(Transparent, Blue, Red)))
  println(blockCount(rotate(shapeS)) == blockCount(shapeS))
  println(wellStructured(rotate(shapeS)) == true)


  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))
  println(shiftSE(List(List(Blue, Transparent), List(Blue, Blue), List(Transparent, Transparent)), 2, 0) ==
             List(List(Transparent, Transparent, Blue, Transparent), 
                    List(Transparent, Transparent, Blue, Blue), 
                    List(Transparent, Transparent, Transparent, Transparent)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  println(shiftNW(List(List(Blue, Transparent), List(Blue, Blue), List(Transparent, Transparent)), 2, 0) ==
             List(List(Blue, Transparent, Transparent, Transparent), 
                    List(Blue, Blue, Transparent, Transparent), 
                    List(Transparent, Transparent, Transparent, Transparent)))
  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
          List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  println(padTo(List(List(Red, Transparent, Red), List(Red, Transparent, Transparent)), 3, 3) == 
             List(List(Red, Transparent, Red), 
                    List(Red, Transparent, Transparent), 
                    List(Transparent, Transparent, Transparent)))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  println(overlap(List(List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Blue)), 
  List(List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Blue))) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  println(combine(List(List(Transparent)), List(List(Red))) == List(List(Red)))
  
}
