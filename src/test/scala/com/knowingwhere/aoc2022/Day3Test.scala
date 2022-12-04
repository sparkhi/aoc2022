package com.knowingwhere.aoc2022

import org.scalatest.Matchers._
import org.scalatest._

class Day3Test extends WordSpec with BeforeAndAfterEach {
  "Day 3 " should {
    "give correct priority value for code" in {
      Day3.getPriorityValue("a") shouldBe 1
      Day3.getPriorityValue("z") shouldBe 26
      Day3.getPriorityValue("A") shouldBe 27
      Day3.getPriorityValue("Z") shouldBe 52
      Day3.getPriorityValue("s") shouldBe 19
      Day3.getPriorityValue("P") shouldBe 42
    }
  }
}
