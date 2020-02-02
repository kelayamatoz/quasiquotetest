import spatial.dsl._

@spatial object DRAMLoadStoreTest extends SpatialApp {
  def main(args: Array[String]): Unit = {
    val len = 32.to[I32];
    val memLen = 16.to[I32];
    val inData = Array.tabulate[Int](len){ i => i.to[Int] }
    val inDRAM: DRAM1[Int] = DRAM[Int](len)
    val outDRAM = DRAM[Int](len)
    setMem(inDRAM, inData)

    Accel {
      val mem = SRAM[Int](memLen)
      Sequential.Foreach(len by memLen) { i =>
        mem load inDRAM(i :: i + memLen)
        Foreach(memLen by 1.to[I32]) { j =>
          mem(j) = mem(j) + 3.to[I32]
        }

        outDRAM(i :: i + memLen) store mem
      }
    }

    val outData = getMem(outDRAM)
    printArray(outData)
  }
}
