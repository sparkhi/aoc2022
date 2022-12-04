package com.knowingwhere.aoc2022

import org.scalatest.Matchers._
import org.scalatest._

class Day4Test extends WordSpec with BeforeAndAfterEach {
  "the cleanup" should {
    "correctly determine the simple wholly contained" in {
      Day4.isFullyContained("1-6,2-4") shouldBe true
    }

    "correctly determine the non wholly contained" in {
      Day4.isFullyContained("1-3,2-4") shouldBe false
    }

    "correctly determine when one of the end points of range is same" in {
      Day4.isFullyContained("1-33,22-33") shouldBe true
      Day4.isFullyContained("22-33,1-33") shouldBe true
    }

    "correctly determine when there is any overlap" in {
      Day4.isAnyOverlap("1-3,2-4") shouldBe true
    }

    "correctly determine when there is no overlap" in {
      Day4.isAnyOverlap("1-13,21-24") shouldBe false
    }

    "correctly determine when there is overlap at one end" in {
      Day4.isAnyOverlap("1-10,10-21") shouldBe true
      Day4.isAnyOverlap("5-10,1-5") shouldBe true
    }
  }
}