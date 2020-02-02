import argon.State
import forge.tags.api
import spatial.dsl._

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._

object StreamFlowServer {
  val stateHolder: ListBuffer[argon.State] = new ListBuffer[State]()
  val d: scala.Int = 32
  val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()
  def getTypeName(sign: scala.Boolean): TypeName = {
    sign match {
      case true => TypeName("TRUE")
      case _ => TypeName("FALSE")
    }
  }

  def getTypeName(n: scala.Int): TypeName = {
    TypeName(s"_$n")
  }

  def getTermName(s: java.lang.String): TermName = {
    TermName(s)
  }

  def getTypeName(ns: java.lang.String): TypeName = {
    TypeName(ns)
  }

  def constructTypeQuasiQuote[A](instType: java.lang.String,
                                 dType: java.lang.String,
                                 sign: scala.Boolean,
                                 nIntBits: scala.Int,
                                 nFracBits: scala.Int,
                                 dims: List[scala.Int]): A = {
    val s = getTypeName(sign)
    val i = getTypeName(nIntBits)
    val f = getTypeName(nFracBits)
    val dT = getTypeName(dType)
    val m = getTermName(instType)
    val qq = tq"spatial.lang.$dT[$s, $i, $f]"
    val nDims = dims.length
    val tr: Tree = nDims match {
      case 0 =>
        scala.Console.println("on Reg Type")
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]"
      case 1 =>
        scala.Console.println("on DRAM Type")
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]($dims.head)"
      case 2 =>
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]($dims.head, $dims(1))"
      case 3 =>
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]($dims.head, $dims(1), $dims(2))"
      case 4 =>
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]($dims.head, $dims(1), $dims(2), $dims(3))"
      case 5 =>
        q"import spatial.dsl._; implicit val state = StreamFlowServer.stateHolder.head; $m[$qq]($dims.head, $dims(1), $dims(2), $dims(3), $dims(4))"
      case _ => q""

    }

    StreamFlowServer.toolbox.eval(tr).asInstanceOf[A]
  }
}


trait TestTrait {
  val _s = StreamFlowServer.stateHolder

  def getArgOut: Reg[Int] = {
    implicit val state = _s.head
    ArgOut[Int]
  }

}

object PythonSpaceVar {
  var tmp: Any = _
  var tmpSRAM: Any = _
}

case class InstanceHolder(v: Any)

@spatial object QuasiQuoteTest extends SpatialApp with TestTrait {
  def main(args: Array[String]): Unit = {
    val a = 1.to[Int]
    val b = 2.to[Int]
    val argInA = ArgIn[Int]
    val argInB = ArgIn[Int]
    val s = TypeName("TRUE")
    val i = TypeName("_32")
    val f = TypeName("_0")
    val dType = TypeName("FixPt")
    val qq = tq"spatial.lang.$dType[$s, $i, $f]"

    type p = FixPt[TRUE, _0, _1]

    setArg(argInA, a)
    setArg(argInB, b)

    def getState()(implicit s: argon.State): Unit = {
      StreamFlowServer.stateHolder.append(s)
    }

    getState()

    val argOutC = ArgOut[Int]

    val dram0 = DRAM[spatial.lang.FixPt[TRUE, _32, _0]](32.to[I32])
    val dram1 = DRAM[spatial.lang.FixPt[TRUE, _32, _0]](32.to[I32])
    scala.Console.println("testing...")
    val m = StreamFlowServer.constructTypeQuasiQuote[DRAM1[_]]("DRAM", "FixPt", true, 16, 16, List(32))

    val cmds = new ListBuffer[() => Any]()
    val varQ = new ListBuffer[Any]()

    val start = 0
    val stop = 32
    val stepSize = 32
    val p = 1


    val foreachCmds1: ListBuffer[I32 => Any] = new ListBuffer[I32 => Any]()
    val foreachCmds2: ListBuffer[I32 => Any] = new ListBuffer[I32 => Any]()
    foreachCmds1.append(
      (i: I32) => { PythonSpaceVar.tmpSRAM.asInstanceOf[SRAM1[_]].load(dram0) }
    )

    foreachCmds2.append(
      (i: I32) => { dram1 store PythonSpaceVar.tmpSRAM.asInstanceOf[SRAM1[_]]}
    )

    def runLoop(i: I32) = {
      foreachCmds1.foreach(m => m(i))
    }

    def runLoopNew(i: I32) = {
      foreachCmds2.foreach(m => m(i))
    }

    cmds.append(
      () => {val m = Reg[Int]; PythonSpaceVar.tmp = m},
      () => {PythonSpaceVar.tmp.asInstanceOf[Reg[Int]] := argInB.value + 1.to[Int]},
      () => {argOutC := argInA.value + argInB.value + PythonSpaceVar.tmp.asInstanceOf[Reg[Int]].value},
      () => {PythonSpaceVar.tmpSRAM = SRAM[Int](32.to[I32])},
      () => {
        Foreach (start.to[I32] until stop.to[I32] by stepSize.to[I32] par p.to[I32]) { i =>
        runLoop(i)
      }},

      () => {
        Foreach (start.to[I32] until stop.to[I32] by stepSize.to[I32] par p.to[I32]) { i =>
          runLoopNew(i)
        }
      }
    )


    Accel {
      cmds.foreach(m => m())
    }

    getMem(dram0)

    val c = getArg(argOutC)
    println(c)
  }
}
