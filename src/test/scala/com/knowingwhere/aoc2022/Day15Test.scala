package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.Position
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day15Test extends WordSpec with BeforeAndAfterEach {
  "beacon finder " should {

    "calculate manhattan distance between the two positions " in {
      Day15.calculateManhattanDistance(Position(2,3), Position(2, 5)) shouldBe 2 //simple vertical distance
      Day15.calculateManhattanDistance(Position(17,3), Position(51, 3)) shouldBe 34 //simple horizontal distance
      Day15.calculateManhattanDistance(Position(139200, 174100), Position(139201, 173100)) shouldBe 1001 //simple horizontal distance
    }

    "coverage should be zero when the requested Y lies outside sensor - beacon manhattan distance " in {
      val sensorPosition = Position(8, 7)
      val beaconPosition = Position(2, 10)

      val coverage = Day15.findCoverageAtSpecificY(sensorPosition, beaconPosition, 18)
      coverage shouldBe List.empty
    }

    "coverage should be just three elements when the beacon and sensor are in vertical line " in {
      val sensorPosition = Position(8, 7)
      val beaconPosition = Position(8, 10)

      val coverage = Day15.findCoverageAtSpecificY(sensorPosition, beaconPosition, 9)
      coverage.size shouldBe 3
      coverage shouldBe List(7,8,9)
    }

    "coverage should be double when the beacon and sensor are in a horizontal line " in {
      val sensorPosition = Position(5, 20)
      val beaconPosition = Position(6, 20)

      val coverage = Day15.findCoverageAtSpecificY(sensorPosition, beaconPosition, 20)
      coverage.size shouldBe 3
      coverage shouldBe List(4,5,6)
    }

    "coverage should be correctly calculated for diagonal arrangement of sensor and beacon " in {
      val sensorPosition = Position(8, 7)
      val beaconPosition = Position(2, 10)

      val coverage = Day15.findCoverageAtSpecificY(sensorPosition, beaconPosition, 6)
      coverage shouldBe (0 to 16).toList

      Day15.findCoverageAtSpecificY(sensorPosition, beaconPosition, 0) shouldBe (6 to 10).toList
    }
  }
}
