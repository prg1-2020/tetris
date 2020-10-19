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
  // 目的：nこの任意の型を持つaからなるリストを作成する
    def duplicate[A](n:Int, x:A):List[A]={
        if (n<=0)  Nil
        else x::duplicate[A](n-1,x)
    }


  // 2. empty
  // 目的：rows行cols列のshapeを作る
    def empty(rows:Int ,cols:Int): Shape ={
        if (rows<=0)  Nil 
        else duplicate(cols,Transparent)::empty(rows-1,cols)
    }


  // 3. size
  // 目的：受け取ったshapeのサイズを(列、行)の形で返す
    def size(shape:Shape):(Int,Int)={

        def get_cols(shape:Shape , cols:Int) :Int ={
            shape match{
                case Nil => cols
                case x::xs => {
                    val cols_now = max(cols,x.length)
                    get_cols(xs,cols_now)
                }
            }
        }
        val rows = shape.length
        val cols = get_cols(shape,0)

        (rows,cols)
    }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれる空以外のブロックの数を返す
    def blockCount(shape:Shape):Int={
        def not_transprant_len(shape:Row,count:Int):Int ={ 
            shape match{
                case Nil => count
                case x::xs=>{
                    if (x == Transparent) not_transprant_len(xs,count)
                    else {
                        val count_now = count +1
                        not_transprant_len(xs,count_now)
                    }
                }
            }
        }

        def count_re(shape:Shape,count:Int):Int ={
            shape match{
                case Nil => count
                case x::xs => {
                    val count_now = not_transprant_len(x,count)
                    count_re(xs,count_now)
                }
            }
        }
        count_re(shape,0)
    }


  // 5. wellStructured
  // 目的：受け取ったshapeが行数・列数ともに１以上で各行の要素数が等しいか確かめる
    def wellStructured(shape:Shape):Boolean={
        def get_cols(shape:Shape , cols:Int) :Int ={
            shape match{
                case Nil => cols
                case x::xs => {
                    val cols_now = max(cols,x.length)
                    get_cols(xs,cols_now)
                }
            }
        }
        

        def matto1(shape:Shape):Int={
            val rows = shape.length
            val cols = get_cols(shape,0)
            if(cols>0 && rows>0) 0
            else 1
        }
        val cols_now = get_cols(shape,0)
        def matto2(shape:Shape,bool:Int):Int={
            shape match{
                case Nil => bool
                case x::xs => {
                    if (x.length == cols_now) matto2(xs,0)
                    else matto2(xs,1)
                }
            }
        }
        val condition1 = matto1(shape)
        val condition2 = matto2(shape,0)
        
        if(condition1+condition2 == 0) true
        else false
    }



  // 6. rotate
  // 目的：受け取ったshapeを反時計回りに９０度回転させる
  // 契約：引数のshapeは真っ当である（wellStructured(shape) == true)
  def rotate(shape:Shape):Shape={
      assert(wellStructured(shape))
    //先頭要素のみをリストにする。回転させるので
     def make_head(shape:Shape): Row={
         shape match{
             case Nil => Nil
             case x::xs => x.head::make_head(xs)
         }
     }
      //残りの部分 後ろの関数で使う
      def remain(shape:Shape):Shape={
          shape match {
              case Nil => Nil
              case x::xs => x.tail ::remain(xs)
          }
      }

      def stack_head(shape:Shape):Shape={
          if (wellStructured(shape)){
              stack_head(remain(shape)) ++ List(make_head(shape))
          }
          else Nil
      }

      stack_head(shape)

  }


  // 7. shiftSE
  // 目的：受け取ったshapeを右にx、下にyずらしたshapeを返す
def shiftSE(shape:Shape,x:Int,y:Int):Shape={
    val (rows,cols) = size(shape)
    val shape_half = empty(y,cols)++shape
    shape_half.map(block => duplicate[Block](x,Transparent)  ++ block)
}


  // 8. shiftNW
  // 目的：受け取ったshapeを左にx、上にyずらしたshapeを返す
def shiftNW(shape:Shape,x:Int,y:Int):Shape={
    val (rows,cols) = size(shape)
    val shape_half = shape++empty(y,cols)
    shape_half.map(block =>block ++ duplicate[Block](x,Transparent))
  }


  // 9. padTo
  // 目的：受け取ったshapeをrows行,cols列に拡大したshapeを返す
  // 契約：rows,colsはshapeの行数列数以上
  def padTo(shape:Shape,rows_to:Int,cols_to:Int):Shape = {
      val (rows_shape,cols_shape) = size(shape)
      assert(rows_to >= rows_shape && cols_to >= cols_shape)
      val (y,x) = (rows_to - rows_shape, cols_to-cols_shape)
      shiftNW(shape,x,y)
  }

  // 10. overlap
  // 目的：2つのshapeが重なるかを調べる
  def overlap(shape1:Shape,shape2:Shape):Boolean = {
      val (rows1,cols1) = size(shape1)
      val (rows2,cols2) = size(shape2)
      //大きさを大きい方に揃えたい
      /*方針
      ①PadToで大きさ揃える
    ②flattenで平坦化する(①により場所が同じが担保
    ③zipメソッドでtupleのリストにする
    ④再帰回してbooleanを判定する    
      */
      val (rows_max,cols_max) = (max(rows1,rows1),max(cols1,cols2))
      val new_shape1 = padTo(shape1,rows_max,cols_max).flatten
      val new_shape2 = padTo(shape2,rows_max,cols_max).flatten

      val zipped_shape = new_shape1.zip(new_shape2)

      def check_re(zipped:List[(Block,Block)],flag:Boolean):Boolean={
          if (flag) true
          else {
              zipped match{
                  case Nil => false
                  case x::xs=> {
                      val (blo1,blo2) = x
                      if(blo1 != Transparent && blo2 != Transparent) check_re(xs,true)
                      else check_re(xs,false)
                  }
              }
          }
      }
      check_re(zipped_shape,false)

  }


  // 11. combine
  // 目的：２つのshapeを結合する
  // 契約：shapeは重なりを持たない
  def combine(shape1:Shape,shape2:Shape):Shape={
      assert(overlap(shape1,shape2) == false)

      val (rows1,cols1) = size(shape1)
      val (rows2,cols2) = size(shape2)
      val (rows_max,cols_max) = (max(rows1,rows1),max(cols1,cols2))
      val new_shape1 = padTo(shape1,rows_max,cols_max)
      val new_shape2 = padTo(shape2,rows_max,cols_max)

      def combine_block(blo1:Block,blo2:Block):Block={
          (blo1,blo2) match{
              case (Transparent,block) => block
              case (block,Transparent) => block
          }
      }

      def combine_row(row1:Row,row2:Row):Row={
          (row1,row2) match{
              case (Nil,Nil)=> Nil
              case (x::xs,y::ys) => combine_block(x,y)::combine_row(xs,ys)
          }
      }

      def combine_re(shape1:Shape,shape2:Shape):Shape={
          (shape1,shape2) match{
              case (Nil,Nil) => Nil
              case (x::xs,y::ys) => combine_row(x,y)::combine_re(xs,ys)
          }
      }
      combine_re(new_shape1,new_shape2)
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
//my
  println(duplicate(-1,"nihao") == Nil)

  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
//my
  println(empty(0,0) == Nil)
  println(empty(2,3) == List(List(Transparent, Transparent, Transparent),List(Transparent, Transparent, Transparent)))

  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ)== (2, 3))
  println(size(shapeS) == (2,3))
//my
println(size(shapeT) == (2,3))
println(size(shapeO)==(2,2))
  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(blockCount(shapeT) == 4)
//my
  println(blockCount(shapeL)== 4)
  println(blockCount(shapeS)==4)

  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  //my
  println(wellStructured(shapeT) == true)
  println(show(rotate(shapeS)))

  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  show(rotate(shapeI))
  show(rotate(shapeZ))

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
  show(shiftSE(shapeT,2,4))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  show(shiftNW(shapeZ,2,4))

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  show(padTo(shapeT,4,5))

  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  
  println(overlap(shapeT,shiftSE(shapeS,2,0)) == false)

  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))

  show(combine(shapeT,shiftSE(shapeS,2,0)))
  
}
