package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractGeneralizedReverser(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt(log2Ceil(bitWidth).W))
    val result = Output(UInt(bitWidth.W))
  })
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {

  val patternBits = log2Ceil(bitWidth)
  var currentResult = io.input
  
  // Apply reversal levels based on pattern bits
  for (level <- 0 until patternBits) {
    val swapDistance = 1 << level  // 2^level
    val nextResult = Wire(UInt(bitWidth.W))
    val resultBits = Wire(Vec(bitWidth, Bool()))
    val currentBits = VecInit(currentResult.asBools)
    
    // For each bit position, decide whether to swap or keep
    for (i <- 0 until bitWidth) {
      val swapPartner = i ^ swapDistance
      val shouldSwap = io.pattern(level)
      
      when(shouldSwap && (swapPartner.U < bitWidth.U)) {
        resultBits(i) := currentBits(swapPartner)
      }.otherwise {
        resultBits(i) := currentBits(i)
      }
    }
    
    nextResult := resultBits.asUInt
    currentResult = nextResult
  }
  
  io.result := currentResult

}
