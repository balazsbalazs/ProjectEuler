package com.paranoid.euler

object Problem001 extends App {
  println("Result: " + getMultipliersSum(1000, 3, 5))
  println("Result: " + bruteForceCheck(1000, 3, 5))

  def getMultipliersSum(limit: Int, mult1: Int, mult2: Int): Int = {
	val result:Int =
		sumMultipliesOf(limit-1, mult1) +
		sumMultipliesOf(limit-1, mult2) -
		sumMultipliesOf(limit-1, mult1 * mult2)
	return result
  }
  
  def sumMultipliesOf(limit: Int, mult: Int): Int = {
    val n = limit/mult
    return mult * n * (n+1)/2
  }
  
  def bruteForceCheck(limit: Int, mult1: Int, mult2: Int): Int = {
    var sum:Int = 0
    var mult1counter = mult1
    var mult2counter = mult2

    while (mult1counter < limit || mult2counter < limit) {
      if (mult1counter == mult2counter) {
        sum += mult1counter
        mult1counter += mult1
        mult2counter += mult2
      } else if (mult1counter < mult2counter) {
        sum += mult1counter
        mult1counter += mult1
      } else {
        sum += mult2counter
        mult2counter += mult2
      }
    }
    return sum;
  }
}