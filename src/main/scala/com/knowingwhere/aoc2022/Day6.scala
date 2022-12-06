package com.knowingwhere.aoc2022

import scala.io.Source

object Day6 extends App {
  val signalChars = Source.fromResource("day6-input.txt").getLines().toList.head
  println(findUnique(signalChars))
  println(findUniquePart2(signalChars))


  def findUnique(signalChars: String) = {
    val setOfFours = signalChars.sliding(4).toList
    val unique = setOfFours.find(_.toSet.size == 4)
    signalChars.indexOf(unique.get) + 4
  }

  def findUniquePart2(signalChars: String) = {
    val setOfFourteens = signalChars.sliding(14).toList
    val unique = setOfFourteens.find(_.toSet.size == 14)
    signalChars.indexOf(unique.get) + 14
  }
}
