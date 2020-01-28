import spatial.dsl._
import scala.reflect.runtime.universe._
import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

@spatial object QuasiQuoteTest extends SpatialApp {
  def main(args: Array[String]): Unit = {
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val toolBox = cm.mkToolBox()
    val a = 1.to[Int]
    val b = 2.to[Int]
    val argInA = ArgIn[Int]
    val argInB = ArgIn[Int]

    def getArgByPtrStr(str: java.lang.String, object_name: java.lang.String)(implicit s: argon.State) = {
      scala.Console.println(s)
      str match {
        case "Int" => toolBox.eval(
          q"implicit val state = $s; spatial.dsl.ArgOut[spatial.dsl.Int]"
        ).asInstanceOf[Reg[_]]
        case "I32" => q"dsl.ArgOut[I32]: dsl.Reg[Int]"
        case "_" => q"ArgOut[Boolean]: Reg[Boolean]"
      }
    }

    setArg(argInA, a)
    setArg(argInB, b)
    val argOutC  = getArgByPtrStr("Int", "ArgOut")

    Accel {
      val regC = Reg[Int]
      regC := argInB.value + 1.to[Int]
      q"argOutC := argInA.value + argInB.value + regC.value"
    }

    q"val c = getArg(argOutC)"
    q"println(c)"
    println("haha")
  }
}
