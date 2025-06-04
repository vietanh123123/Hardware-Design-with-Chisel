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
      
    var accumulatedCount = 0.U(log2Ceil(bitWidth + 1).W)
    var currentFragment = io.input // The part of input we are currently examining

    var currentProcessingWidth = bitWidth // The width of the current fragment being processed

    // The loop run log2(bitWidth) times
    // For bitWidth = 1, log2Ceil(1) = 0 , loop does not run
    // For bitWidth = 2, log2Ceil(2) = 1 , loop runs once
    // For bitWidth = 32 , log2Ceil(32) = 5, loop runs 5 times
    for (_ <- 0 until log2Ceil(bitWidth)) {
    // stepChunkSize is the size of the upper/lower half of the currentFragment
    // (and also the amount to add to the count if the upper half is all zeros)
    val stepChunkSize = currentProcessingWidth / 2

    // Extract the upper and lower halves of the currentFragment
    // Note: currentFragment itself shrinks in width through Mux selection below
    val upperHalf = currentFragment(currentProcessingWidth - 1, stepChunkSize)
    val lowerHalf = currentFragment(stepChunkSize - 1, 0)

    val upperHalfIsZero = (upperHalf === 0.U)

    // Temporary variables to hold the next state for this stage
    val nextAccumulatedCount = Wire(UInt(log2Ceil(bitWidth + 1).W))
    val nextFragment = Wire(UInt(stepChunkSize.W)) // The next fragment will be half the width

    when(upperHalfIsZero) {
      nextAccumulatedCount := accumulatedCount + stepChunkSize.U
      nextFragment := lowerHalf
    }.otherwise {
      nextAccumulatedCount := accumulatedCount
      nextFragment := upperHalf
    }

    // Update the Scala vars for the next iteration (i.e., for generating the next stage of hardware)
    accumulatedCount = nextAccumulatedCount
    currentFragment = nextFragment // currentFragment is now effectively narrower
    currentProcessingWidth = stepChunkSize
  }

  // After the loop:
  // - If bitWidth = 1, loop didn't run. accCount = 0, currentFragment = io.input (1 bit).
  // - If bitWidth > 1, loop ran. currentFragment is now 1-bit wide.
  //   accumulatedCount has the count of leading zeros from all preceding (wider) zero-blocks.
  //
  // The final 1-bit currentFragment determines the last bit of the count.
  // If this last processed bit (currentFragment(0)) is 0, it means it's also a leading zero.
  // Example: input 0b0000 (width 4)
  // Iteration 1 (width=4, step=2): upper="00", lower="00". acc=2. fragment="00" (2-bit).
  // Iteration 2 (width=2, step=1): upper="0", lower="0". acc=2+1=3. fragment="0" (1-bit).
  // Loop ends. acc=3. fragment="0". We need to add 1 because fragment(0) is 0. Result = 4.
  //
  // Example: input 0b0001 (width 4)
  // Iteration 1 (width=4, step=2): upper="00", lower="01". acc=2. fragment="01" (2-bit).
  // Iteration 2 (width=2, step=1): upper="0", lower="1". acc=2+1=3. fragment="1" (1-bit).
  // Loop ends. acc=3. fragment="1". We add 0 because fragment(0) is 1. Result = 3.

  io.result := accumulatedCount + (currentFragment(0) === 0.U).asUInt
}
