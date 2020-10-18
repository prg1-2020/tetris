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
  // 目的：整数nと任意の型の値aを受け取り、n個のaからなるリストを作る
  def duplicate[A](n: Int, a: A): List[A] = {
    if(n <= 0) Nil
    else a :: duplicate(n-1, a)
  }


  // 2. empty
  // 目的：rows行cols列の空のshapeを作る
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }


  // 3. size
  // 目的：受け取ったshapeのサイズを(行数, 列数)の形で返す
  //行数はshapeのlength
  //列数はmax(max(max(Init, row0.length), row1.length), row2.length) 各行の列数の最大値
  def size(shape: Shape): (Int, Int) = {
    (
      shape.length
    ,
      shape.foldLeft(0)((r: Int, x: Row) => max(r, x.length))
    )
  }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空でないブロックの数を返す
  //各行の各マスについてTransparentでないマスの数を合計する
  def blockCount(shape: Shape): Int = {
    shape.foldLeft(0)((r: Int, x: Row) => //各行について繰り返す
      r + x.foldLeft(0)((r: Int, x: Block) => //各マスについて繰り返す
        r + (if(x != Transparent) 1 else 0)
      )
    )
  }


  // 5. wellStructured
  // 目的：受け取ったshapeがまっとうであるかを判断する
  //「まっとう」とは行数・列数がともに１以上であり、各行の要素数が全て等しいことを指す
  //(最初の行がある)(最初の行の要素数が1以上)(最初の行の要素数と他の行の要素数が等しい)を順に確認する
  def wellStructured(shape: Shape): Boolean = {
    shape match {
      case Nil => false
      case x :: xs => //最初の行あり
        val cols = x.length
        (cols >= 1) && //最初の行の要素数が1以上
          xs.foldLeft(true)((ans: Boolean, row: Row) => ans && (row.length == cols)) //最初の行の要素数がcolsなのは自明なので省略
    }
  }


  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに90度回転させたshapeを返す
  // 契約：shapeはまっとうである
  /*
  1 2 3    3 6 9 C
  4 5 6 => 2 5 8 B
  7 8 9    1 4 7 A
  A B C
  */
  //各行の先頭は回転後のNilに一番近い部分になる
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape))

    /*
    //目的：各行の先頭を並べたheadsと残りのtailsを作り(heads,tails)で返す
    //契約：各行の要素数は1以上(行数は0でもよい)
    def separateHead(shape: Shape): (Row, Shape) = {
      shape match {
        case Nil => (Nil, Nil)
        case row :: rowTail =>
          val (headsPrev, tailsPrev) = separateHead(rowTail)
          row match {
            case h :: tail => (h :: headsPrev, tail :: tailsPrev)
          }
      } 
    }
    */

    /*
    全てのマスは1回だけheadsとして取り出されaccに結合されるのでマス数がnの場合、計算量はO(n)
    tailsへの操作はheadsの分離結合と同じ回数しか行われないためそれで計算量がマス数の定数倍を超えることはない
    */
    def rotateAcc(shape: Shape, acc: Shape):Shape = {
      if(shape match {case x :: xs => x == Nil}) acc
      else {
        /*
        val heads = shape.foldRight(Nil: Row)((row: Row, ans: Row) => (row match {case h :: tail => h}) :: ans)
        val tails = shape.foldRight(Nil: Shape)((row: Row, ans: Shape) => (row match {case h :: tail => tail}) :: ans)
        */
        // val (heads, tails) = separateHead(shape) //上の2行を１回でできるようにしただけ
        val (heads, tails) = 
          shape.foldRight((Nil: Row, Nil: Shape))(
            (row: Row, ans: (Row, Shape)) =>
              row match {case h :: tail => (h :: ans._1, tail :: ans._2)}
          ) //foldRightにまとめられた
        rotateAcc(tails, heads :: acc)
      }
    } 
    rotateAcc(shape, Nil)
  }


  // 7. shiftSE
  // 目的：受け取ったshapeを右にx,下にyずらしたshapeを返す
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {
    if(y>0) duplicate(size(shape)._2 + x, Transparent) :: shiftSE(shape, x, y-1)
    else if(x>0) shiftSE(shape, x-1, y).map(Transparent :: _)
    else shape
  }


  // 8. shiftNW
  // 目的：受け取ったshapeを左にx,上にyずらしたshapeを返す
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {
    shape.map(_ ++ duplicate(x, Transparent)) ++ empty(y, size(shape)._2 + x)
    //Nilが返ってきたときListの連結++は何も起こらないのでOK
  }


  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す
  // 契約：rows, colsはshapeの行数・列数以上
  def padTo(shape: Shape, rows: Int, cols: Int) = {
    val (r, c) = size(shape)
    assert(r <= rows && c <= cols)
    shiftNW(shape, cols - c, rows - r)//x ,yとr, cは逆なので注意
  }


  // 10. overlap
  // 目的：２つのshapeが重なりを持つかを判断する
  //padToで同sizeにした後、平らにして、全マス比較する
  def overlap(s1: Shape, s2: Shape): Boolean = {
    val (r1, c1) = size(s1)
    val (r2, c2) = size(s2)
    val rMax = max(r1, r2)
    val cMax = max(c1, c2)
    val p1 = padTo(s1, rMax, cMax).flatten
    val p2 = padTo(s2, rMax, cMax).flatten

    //各マスについて両方Transparentでないマスがあるか探す
    //Transparentをfalseに色付きをtrueにしたときマスごとにandをとり全体でorをとったものを返せばよい
    /*
    0 0 1 1
    & & & &
    0 1 0 1
    0|0|0|1 => 1 一番最後のマスが両方1なので1(重なりあり)
    */
    /*
    (
      p1.foldLeft(0)((bits: Int, block: Block) => (bits<<1) | (if(block!=Transparent) 1 else 0)) //bit列に変換する
    &
      p2.foldLeft(0)((bits: Int, block: Block) => (bits<<1) | (if(block!=Transparent) 1 else 0))
    ) != 0 //どこか1ならtrue,全て0ならfalseなのでbitのorになる
    */
    p1.zip(p2).foldLeft(false)((ans: Boolean, tuple: (Block, Block)) => ans || ((tuple._1!=Transparent) && (tuple._2!=Transparent)))
    //zipを使ったものに修正
  }


  // 11. combine
  // 目的：２つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
  def combine(s1: Shape, s2: Shape): Shape = {
    assert(!overlap(s1, s2))
    //各マスのorをとればいいのでa || b を a?a:bに書き換えて使う
    val (r1, c1) = size(s1)
    val (r2, c2) = size(s2)
    val rMax = max(r1, r2)
    val cMax = max(c1, c2)
    val p1 = padTo(s1, rMax, cMax)
    val p2 = padTo(s2, rMax, cMax)
    //今回は平らにしない
    //zipを使う
    p1.zip(p2) //この段階では行の組なのでもう一度zip
    .map((tuple: (Row, Row)) => tuple._1.zip(tuple._2)) //ブロックの組になった
    .map(_.map((tuple: (Block, Block)) => if(tuple._1 != Transparent) tuple._1 else tuple._2)) //各マスに対してa?a:b
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

  println(duplicate(0, "no") == Nil)

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))

  println(empty(2, 3) == List(List(Transparent, Transparent, Transparent), List(Transparent, Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))

  println(size(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == (3, 2))

  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)

  println(blockCount(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == 5)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)

  println(wellStructured(empty(2, 3)) == true)

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  println(rotate(List(List(Red, Transparent), List(Blue, Yellow))) == List(List(Transparent, Yellow), List(Red, Blue)))

  // rotate が満たすべき性質のテスト
  println(rotate(rotate(rotate(rotate(shapeJ)))) == shapeJ)
  println(blockCount(rotate(shapeZ)) == blockCount(shapeZ))
  println(size(rotate(shapeL))._1 == size(shapeL)._2 && size(rotate(shapeL))._2 == size(shapeL)._1)

  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))

  println(shiftSE(List(List(Blue)), 0, 2) ==
    List(List(Transparent),
         List(Transparent),
         List(Blue)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))

  println(shiftNW(List(List(Blue)), 0, 0) ==
    List(List(Blue)))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))

  println(padTo(List(List(Blue),List(Red)), 3, 2) ==
    List(List(Blue, Transparent),
         List(Red, Transparent),
         List(Transparent, Transparent)))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)

  println(overlap(shapeL, shiftSE(shapeJ, 1, 0)) == true)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

  println(" ")
  show(combine(shapeI, shiftSE(shapeZ, 1, 1)))

}
