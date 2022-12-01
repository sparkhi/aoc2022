package com.knowingwhere.aoc2022

import scala.io.Source

object Day1 extends App {
  val resource = Source.fromResource("day1-input.txt")
  val lineGroups = resource.getLines().mkString("\n").split("\n\n").toList.map(entry => entry.split("\n").map(s => s.toInt).toList)
  val sums = lineGroups.map(oneList => oneList.sum)
  println(sums.max)

  val topThreeSum = sums.sorted(Ordering[Int].reverse).take(3).sum
  println(topThreeSum)
}
