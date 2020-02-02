import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe._
import argon.lang.FixPt
import spatial.lang.ArgOut


object ArgOutUtils {
  def createArgOut()(implicit state: argon.State) = {
    val toolbox = currentMirror.mkToolBox()
    val signType = TypeName("argon.lang.TRUE")
    val intName = TypeName("argon.lang._32")
    val fracName = TypeName("argon.lang._0")
    val qq = tq"spatial.lang.FixPt[$signType, $intName, $fracName]"
    toolbox.eval(
      q"spatial.lang.ArgOut[$qq]"
    ).asInstanceOf[spatial.lang.Reg[_]]
  }
}
