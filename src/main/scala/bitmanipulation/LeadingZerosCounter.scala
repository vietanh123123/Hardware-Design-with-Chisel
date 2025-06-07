package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractLeadingZerosCounter(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(log2Ceil(bitWidth + 1).W))
  })
}

// You may expect bitWidth to be a power of two.
class LeadingZerosCounter(bitWidth: Int)
    extends AbstractLeadingZerosCounter(bitWidth) {

  // Recursive function to count leading zeros with logarithmic depth
  def countLeadingZeros(input: UInt, width: Int): UInt = {
    if (width == 1) {
      // Base case: single bit
      // If bit is 0, then 1 leading zero, else 0 leading zeros
      Mux(input === 0.U, 1.U, 0.U)
      //Mux(condition, trueValue, falseValue)
    } else {
      // Recursive case: divide and conquer
      val halfWidth = width / 2
      val leftHalf = input(width - 1, halfWidth)
      val rightHalf = input(halfWidth - 1, 0)
      
      // Recursively count leading zeros in each half
      val leftCount = countLeadingZeros(leftHalf, halfWidth)
      val rightCount = countLeadingZeros(rightHalf, halfWidth)
      
      // Check if left half is all zeros
      val leftAllZeros = leftHalf === 0.U
      
      // Combine results:
      // If left half has any 1-bits -> result is just left count
      // If left half is all zeros -> result is left_width + right count
      val resultWidth = log2Ceil(width + 1)
      Mux(leftAllZeros, (halfWidth.U(resultWidth.W) + rightCount), leftCount)
    }
  }
  
  // Connect the recursive function to the output
  io.result := countLeadingZeros(io.input, bitWidth)

}