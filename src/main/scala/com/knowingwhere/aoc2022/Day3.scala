package com.knowingwhere.aoc2022

import scala.io.Source

object Day3 extends App {
  val rucksackContents = Source.fromResource("day3-input.txt").getLines().toList
  val compartmentContents: List[(String, String)] = rucksackContents.map(eachRucksack => splitIntoCompartments(eachRucksack))
  val commonContents = compartmentContents.map(pair => pair._1 intersect pair._2)
  val priorityValues = commonContents.map(commonContent => getPriorityValue(commonContent))

  println(priorityValues.sum)

  val groups = rucksackContents.grouped(3).toList
  val badgeValues = groups.map(eachGroup => getGroupIntersection(eachGroup))
  val badgePriorities = badgeValues.map(badge => getPriorityValue(badge))
  println(badgePriorities.sum)

  def getGroupIntersection(eachGroup: List[String]): String = {
    eachGroup.head intersect eachGroup.tail.head intersect eachGroup.tail.tail.head
  }

  def splitIntoCompartments(eachRucksack: String): (String, String) = {
    eachRucksack.splitAt(eachRucksack.length / 2)
  }

  /**
   * To help prioritize item rearrangement, every item type can be converted to a priority:
   * Lowercase item types a through z have priorities 1 through 26.
   * Uppercase item types A through Z have priorities 27 through 52.
   * @param commonContent priority code
   */
  def getPriorityValue(commonContent: String): Int = {
    val charToCheck = commonContent.toCharArray.head
    if (charToCheck.isUpper) {
      27 + charToCheck.toInt - 'A'.toInt
    } else {
      1 + charToCheck.toInt - 'a'.toInt
    }
  }

}
