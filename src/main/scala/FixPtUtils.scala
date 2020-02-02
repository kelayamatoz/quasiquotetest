import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._
import argon.lang.FixPt

object FixPtUtils {
  def createFixPt(sign: Boolean, integer: Int, fractional: Int, value: Double): FixPt[_, _, _] = {
    val toolbox = currentMirror.mkToolBox()
    val signType = TypeName(if (sign) "TRUE" else "FALSE")
    val intName = TypeName(s"_$integer")
    val fracName = TypeName(s"_$fractional")
    val qq = tq"spatial.lang.FixPt[$signType, $intName, $fracName]"

    val constant = Literal(Constant(value))

    toolbox.eval(
      q"argon.lang.uconst[$qq](spatial.emul.FixedPoint.fromDouble($constant))"
    ).asInstanceOf[FixPt[_, _, _]]
  }
}


//import scala.reflect.runtime.universe._
//import scala.reflect.runtime.currentMirror
//import scala.tools.reflect.ToolBox



//    def getArgByPtrStr(str: java.lang.String, object_name: java.lang.String)(implicit s: argon.State) = {
//      scala.Console.println(s)
//      str match {
//        case "Int" => toolBox.eval(
//          q"implicit val state = $s; spatial.dsl.ArgOut[spatial.dsl.Int]"
//        ).asInstanceOf[Reg[_]]
//        case "I32" => q"dsl.ArgOut[I32]: dsl.Reg[Int]"
//        case "_" => q"ArgOut[Boolean]: Reg[Boolean]"
//      }
//    }




//val toolBox = currentMirror.mkToolBox()
//val signType = TypeName("TRUE")
//val intName = TypeName("_32")
//val fracName = TypeName("_0")
//val qq = tq"spatial.lang.FixPt[$signType, $intName, $fracName]"
