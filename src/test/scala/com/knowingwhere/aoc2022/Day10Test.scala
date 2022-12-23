package com.knowingwhere.aoc2022

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day10Test extends WordSpec with BeforeAndAfterEach {
  "Day 10 processing " should {
    "process the noop instruction and return modified X " in {
      val processDetails = ProcessDetails(4, 3)
      val instructionsAccumulator = List.empty[(Int,Int)]
      val result: (ProcessDetails, List[(Int, Int)]) = Day10.processNoop(processDetails, instructionsAccumulator)
      result._1.completedCycles shouldBe 5
      result._1.xValue shouldBe 3
      result._2.size shouldBe 1
      result._2.head shouldBe (5 -> 3)
    }

    "process the noop instruction and return modified X with previous list" in {
      val processDetails = ProcessDetails(2, 3)
      val instructionsAccumulator = List(1 -> 3, 2 -> 3)
      val result: (ProcessDetails, List[(Int, Int)]) = Day10.processNoop(processDetails, instructionsAccumulator)
      result._1.completedCycles shouldBe 3
      result._1.xValue shouldBe 3
      result._2.size shouldBe 3
      result._2.reverse.head shouldBe (3 -> 3)
    }

    "process the positive addition and return modified X " in {
      val processDetails = ProcessDetails(4, 3)
      val instructionsAccumulator = List.empty[(Int,Int)]
      val result: (ProcessDetails, List[(Int, Int)]) = Day10.processAddx(0, 10, processDetails, instructionsAccumulator)
      result._1.completedCycles shouldBe 6
      result._1.xValue shouldBe 13
      result._2.size shouldBe 2
      result._2.head shouldBe (5 -> 3)
      result._2.tail.head shouldBe (6 -> 3)
    }

  }
}