package com.teamgehem.xoroshiro128plus

case class Xoroshiro128Plus(var state0: Long, var state1: Long) {

  def nextLong() = {
    val s0 = state0
    var s1 = state1
    s1 ^= s0

    state0 = (s0 << 55 | s0 >>> 9) ^ s1 ^ (s1 << 14)
    state1 = (s1 << 36 | s1 >>> 28)

    state0 + state1
  }

  def nextInt(): Int = {
    nextLong.toInt
  }

  def nextInt(bound:Int = Int.MaxValue) = {
    ((bound * (nextLong >>> 33)) >> 31).toInt
  }

  def nextInt(inner:Int, outer:Int):Int = {
    inner + nextInt(outer - inner)
  }
}
