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
  // 目的：整数nと任意の型の値aを受け取りn個のaで構成されたリストを返す
  def duplicate[A](n:Int,a:A):List[A]={
    if(n<=0) Nil
    else a::duplicate[A](n-1,a)
  }




  // 2. empty
  // 目的：rows行cols列の空のshapeを返す
  def empty(rows:Int,cols:Int):Shape={
    duplicate[Row](rows,duplicate[Block](cols,Transparent))
  }



  // 3. size
  // 目的：shapeを受け取りそのサイズを返す
  def sizeAcc(list:Shape,rows:Int,cols:Int):(Int,Int)={
    list match {
      case Nil =>(rows,cols)
      case x::xs=>sizeAcc(xs,rows+1,max(cols,x.length))
}
}
  def size(list:Shape):(Int,Int)={
    sizeAcc(list,0,0)
    
  }


  // 4. blockCount
  // 目的：受け取ったshapeに含まれている空でないブロックの数を返す
  def blockCountRow(list:Row):Int={
    val list1 = list.filter(p => p!=Transparent )
    list1.length
  }
  def blockCount(list:Shape):Int={
    val list2 = list.map[Int](p=> blockCountRow(p))
    list2.foldLeft(0)((r,x)=>r+x)
  }


  // 5. wellStructured
  // 目的：受け取ったshapeがまっとうであるか判断する
  def wellstructuredAcc(list:Shape)(n:Int):Boolean={
    list match{
      case Nil => true
      case x::xs=>if(n== x.length)wellstructuredAcc(xs)(n)else false
    }

  }
  def wellStructured(list:Shape):Boolean={
    list match{
      case Nil => false
      case x::xs=> if (blockCount(list)>0) wellstructuredAcc(xs)(x.length)else false

    }

  }
  
  // 6. rotate
  // 目的：Shapeを受け取り、それを反時計回りに90度回転させたshapeを返す
  // 契約：受け取るshapeはまっとうである
  //  def rotateAcc(list:Shape,newlist:Shape):Shape={//listの先頭要素のみを取り出し、それらをリストにしてnewlistに加える
  //}
  def extract_top(list:Shape,list_top:Row):Row={//listの要素であるRow型のリストの先頭要素のみを取り出しそれらをつなげ、新たなRow型のリストを作る
    list match{
      case Nil=>list_top.reverse
      case x::xs=> x match{
        case Nil=>list_top.reverse
        case y::ys=>extract_top(xs,y::list_top)     
      }
    }
  }
  def extract_following(list:Shape,list_following:Shape):Shape={//listの要素であるRow型のリストの先頭以外の要素を取り出しそれらをつなげ、新たなリストを作る

    list match{
      case Nil=>list_following.reverse
      case x::xs=> x match{
        case Nil=>list_following.reverse
        case y::ys=>extract_following(xs,ys::list_following)     
      }
    }
  }
  def rotateAcc(list:Shape,list_new:Shape):Shape={//top,follo
    list match{
      case Nil =>list_new
      case x::xs=>if(x==List()) list_new else rotateAcc(extract_following(list,Nil),extract_top(list,Nil)::list_new)
    }
  }
  
  def rotate(list:Shape):Shape={
    assert(wellStructured(list)==true)
    rotateAcc(list,Nil)
  }

  // 7. shiftSE
  // 目的：受け取ったshapeを右にx下にyだけ動かしたshapeを返す
  
  //受け取ったrowの最初にn個のtransparentを追加する
  def addTransparent(list:Row,n:Int):Row={
    list++ duplicate(n,Transparent)
    }
  //受け取ったrowの最後にn個のtransparentを追加する
    def addTransparentRev(list:Row,n:Int):Row={
    duplicate(n,Transparent)++list
    }
  def shiftSE(list:Shape,x:Int,y:Int)={
    val (h,w)=size(list)
    empty(y,x+w)++list.map(row=>addTransparentRev(row,x))
  }
  
  // 8. shiftNW
  // 目的：受け取ったshapeを左にx上にyだけ動かしたshapeを返す
  def shiftNW(list:Shape,x:Int,y:Int):Shape={
    val (h,w)=size(list)
    list.map(row=>addTransparent(row,x))++empty(y,x+w)
  }


  // 9. padTo
  // 目的：受け取ったshapeをrows行cols列に拡大したshapeを返す
  // 契約：rows, colsはshapeの行数,列数以上
  def padTo(list:Shape,m:Int,n:Int):Shape={
    val (h,w)=size(list)
    assert(h<=m && w<=n)
    shiftNW(list,n-w,m-h)

  }



  // 10. overlap
  // 目的：二つのshapeが重なりを持つか判定する
  def sizeMax(list1:Shape,list2:Shape):(Int,Int)={
    val (x1,y1)=size(list1)
    val (x2,y2)=size(list2)
    (max(x1,x2),max(y1,y2))
  }
  
  def overlapRow(list1:Row,list2:Row):Boolean={//padToでサイズを同じにして代入する
    (list1,list2) match{
      case(Nil,Nil)=>false
      case(x::xs,y::ys)=>if(x!=Transparent && y!=Transparent) true else(overlapRow(xs,ys))
  }
  }
  def overlapSameSize(list1:Shape,list2:Shape):Boolean={
    (list1,list2) match{
      case (Nil,Nil) => false
      case (x::xs,y::ys)=> if(overlapRow(x,y)==true)true else overlapSameSize(xs,ys)
    }
  }

  def overlap(list1:Shape,list2:Shape):Boolean={
    val (x,y)=sizeMax(list1,list2)
    val listA=padTo(list1,x,y)
    val listB=padTo(list2,x,y)
    overlapSameSize(listA,listB)

  }



  // 11. combine
  // 目的：2つのShapeを結合する
  // 契約：二つのshapeは重なりを持たないd

  //def combine(list1:Shape,list2:Shape):Shape={}


}


// テスト
object ShapeTest extends App {
  import ShapeLib._

  // 関数を定義するたびに、コメント開始位置を後ろにずらす
  
  // 1. duplicate
  /*
  println("duplicate")
  println(duplicate(0, 42) == Nil)
  println(duplicate(1, true) == List(true))
  println(duplicate(3, "hi") == List("hi", "hi", "hi"))
  println(s"自作テスト${ duplicate(3,List(1,2,3))== List(List(1,2,3),List(1,2,3),List(1,2,3))}")


  // 2. empty
  println("empty")
  println(empty(1, 3) == List(List(Transparent, Transparent, Transparent)))
  println(empty(3, 1) == List(List(Transparent), List(Transparent), List(Transparent)))
  println(empty(0, 2) == Nil)
  println(empty(2, 0) == List(Nil, Nil))
  println(s"自作テスト${empty(-1,-1)== Nil}")
  // 3. size
  println("size")
  println(size(Nil) == (0, 0))
  println(size(shapeI) == (4, 1))
  println(size(shapeZ) == (2, 3))
  println(s"自作テスト ${size(List(List(Red),List(Blue,Red,Blue)))==(2,3)}")


  // 4. blockCount
  println("blockCount")
  println(blockCount(Nil) == 0)
  println(blockCount(shapeI) == 4)
  println(blockCount(shapeZ) == 4)
  println(s"自作テスト${blockCount(List(List(Red),List(Blue,Red,Blue,Red,Transparent)))==5}")
  println(s"自作テスト${blockCount(List(Nil, Nil))==0}")
  // 5. wellStructured
  println("wellStructured")
  println(wellStructured(Nil) == false)
  println(wellStructured(List(Nil, Nil)) == false)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue, Blue))) == true)
  println(wellStructured(List(List(Red, Red), List(Yellow, Yellow), List(Blue))) == false)
  println(wellStructured(shapeI) == true)
  println(wellStructured(shapeZ) == true)
  println(s"自作テスト${wellStructured(List(Nil,List(Red)))==false}")

  println("test extract_top")
  println(extract_top(Nil,Nil))
  println(extract_top(List(List(Red),List(Blue),List(Green)),Nil))
  println(extract_top(List(
  List(Red,Yellow,Red),
  List(Blue,Blue,Green),
  List(Green,Red,Pink)
  ),Nil)==List(Red,Blue,Green))

  println("test extract_followng")
  println(extract_following(extract_following(List(
  List(Red,Yellow,Red),
  List(Blue,Blue,Green),
  List(Red,Red,Pink)
  ),Nil),Nil))
  println(extract_following(List(List(Red),List(Blue),List(Green)),Nil))
  println(extract_following(List(List(), List(), List()),Nil))
*/
  // 6. rotate
  println("rotate")
  println(rotate(List(List(Red), List(Blue))) == List(List(Red, Blue)))
  println(rotate(List(
    List(Red,Yellow,Red,Red),
    List(Blue,Blue,Green,Green),
    List(Green,Red,Pink,Pink),
    List(Green,Red,Pink,Pink)
  ))==List(
    List(Red,Green,Pink,Pink),
    List(Red,Green,Pink,Pink),
    List(Yellow,Blue,Red,Red),
    List(Red,Blue,Green,Green)
  ))
  show(rotate(shapeI))
  show(rotate(shapeZ))

  // rotate が満たすべき性質のテスト
  

  //何度適用してもblockCountは保たれる
  println(blockCount(rotate(shapeL))==blockCount(shapeL))
  //4回適用すれば元の形に戻る
  println(rotate(rotate(rotate(rotate(shapeT))))==shapeT)
  //何度適用してもwellStructureは保たれる
  println(wellStructured(rotate(shapeZ))==wellStructured(shapeZ))
  //偶数回適用した場合サイズは保たれる
  println(size(rotate(rotate(shapeZ)))==size(shapeZ))
  
  
  
  // 7. shiftSE
  println("shiftSE")
  println(shiftSE(List(List(Blue)), 1, 2) ==
    List(List(Transparent, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Blue)))
  show(shiftSE(shapeI, 1, 2))

  //test
  println(shiftSE(List(
    List(Red,Green,Pink,Pink),
    List(Red,Green,Pink,Pink),
    List(Yellow,Blue,Red,Red),
    List(Red,Blue,Green,Green)
  ),0,3)==List(
    List(Transparent,Transparent,Transparent,Transparent),
    List(Transparent,Transparent,Transparent,Transparent),
    List(Transparent,Transparent,Transparent,Transparent),
    List(Red,Green,Pink,Pink),
    List(Red,Green,Pink,Pink),
    List(Yellow,Blue,Red,Red),
    List(Red,Blue,Green,Green)))

  // 8. shiftNW
  println("shiftNW")
  println(shiftNW(List(List(Blue)), 1, 2) ==
    List(List(Blue, Transparent),
         List(Transparent, Transparent),
         List(Transparent, Transparent)))
  show(shiftNW(shapeI, 1, 2))
  //test
  
  println(shiftNW (List(
    List(Red,Green,Pink,Pink),
    List(Red,Green,Pink,Pink),
    List(Yellow,Blue,Red,Red),
    List(Red,Blue,Green,Green)
  ),0,3)==List(
    List(Red,Green,Pink,Pink),
    List(Red,Green,Pink,Pink),
    List(Yellow,Blue,Red,Red),
    List(Red,Blue,Green,Green),
    List(Transparent,Transparent,Transparent,Transparent),
    List(Transparent,Transparent,Transparent,Transparent),
    List(Transparent,Transparent,Transparent,Transparent)
    ))
    

  // 9. padTo
  println("padTo")
  println(padTo(List(List(Blue)), 2, 3) ==
    List(List(Blue, Transparent, Transparent),
         List(Transparent, Transparent, Transparent)))
  show(padTo(shapeI, 6, 2))
  //test
  println(padTo(List(List(Blue,Green),List(Red,Red)),2,4)==
  List(List(Blue,Green,Transparent,Transparent),List(Red,Red,Transparent,Transparent))
  )


  // 10. overlap
  println("overlap")
  println(overlap(shapeI, shapeZ) == true)
  println(overlap(shapeI, shiftSE(shapeZ, 1, 1)) == false)
  //test
  println(overlap(List(List(Red,Transparent,Red),List(Red,Transparent,Transparent)),List(List(Transparent,Red,Transparent),List(Transparent,Red,Transparent)))==false)

/*
  // 11. combine
  println("combine")
  println(combine(List(List(Red), List(Transparent)),
                  List(List(Transparent), List(Blue))) ==
    List(List(Red), List(Blue)))
  show(combine(shiftSE(shapeI, 0, 1), shapeZ))
  //test

  */
}