package com.knowingwhere.aoc2022

import scala.io.Source

object Day4 extends App {

  val cleaningSchedule = Source.fromResource("day4-input.txt").getLines().toList
  val whollyContainedSchedules = cleaningSchedule.filter(eachSchedule => isFullyContained(eachSchedule))
  println(whollyContainedSchedules.size)

  val overlappingSchedules = cleaningSchedule.filter(eachSchedule => isAnyOverlap(eachSchedule))
  println(overlappingSchedules.size)

  /**
   *
   * @param schedulePair schedule of cleaning in the form of 71-73,1-72
   *                     this indicates that first cleanup is 71 through 73 inclusive and
   *                     second cleanup is 1 through 72 inclusive
   * @return true if one of the cleanups is wholly contained within another
   */
  def isFullyContained(schedulePair: String) = {
    val (schedule1: Schedule, schedule2: Schedule) = getSchedules(schedulePair)
    schedule1.isWhollyContained(schedule2)
  }

  def isAnyOverlap(schedulePair: String) = {
    val (schedule1: Schedule, schedule2: Schedule) = getSchedules(schedulePair)
    schedule1.isAnyOverlap(schedule2)
  }

  private def getSchedules(schedulePair: String) = {
    val schedules = schedulePair.split(",").toList
    val schedule1Values = schedules.head.split("-")
    val schedule1 = Schedule(schedule1Values.head.toInt, schedule1Values.tail.head.toInt)
    val schedule2Values = schedules.tail.head.split("-")
    val schedule2 = Schedule(schedule2Values.head.toInt, schedule2Values.tail.head.toInt)
    (schedule1, schedule2)
  }

  case class Schedule(start: Int, end: Int) {
    def isWhollyContained(another: Schedule): Boolean = {
      if (end - start > another.end - another.start) {
        if ((another.start >= start) && (another.end <= end)){
          true
        } else {
          false
        }
      } else {
        if ((start >= another.start) && (end <= another.end)){
          true
        } else {
          false
        }
      }
    }

    def isAnyOverlap(another: Schedule): Boolean = {
      val fullRange = Math.max(end, another.end) - Math.min(start, another.start)
      val individualRangeSum = (end - start) + (another.end - another.start)
      if (fullRange > individualRangeSum) {
        false
      } else {
        true
      }
    }
  }
}

