package com.paranoid.euler
/**
 * Project Euler, problem 003
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 * test squash 
**/

object Problem003 extends App {
  println(maxPrimeFactor(600851475143L));

  def maxPrimeFactor(number: Long): Long = {
    return primeFactor(number).last;
  }

  def primeFactor(number: Long): List[Long] = {
    var primes = List(2L);
    var result = List[Long]();
    var i:Long = 2;
    var num = number
    while (i <= num) {
      if (num % i == 0) {
        result = result ::: List(i);
        num = num / i;
      } else {
        primes = nextPrime(primes);
        i = primes.last;
      }
    }
    return result;

  }

  def nextPrime(prevPrimes: List[Long]): List[Long] = {
    var i = prevPrimes.last + 1;
    var primesIterator = prevPrimes.iterator;
    while (!isPrime(i, prevPrimes))
      i += 1
    return prevPrimes ::: List(i);
  }

  def isPrime(number: Long, primes: List[Long]): Boolean = {
    var calc = number;
    var primesIterator = primes.iterator;
    var i = primesIterator.next();

    while (i <= calc && primesIterator.hasNext) {
      if (number % i == 0)
        return false;
      calc = Math.ceil(number / i).toInt;
      i = primesIterator.next();
    }
    return true;
  }

}