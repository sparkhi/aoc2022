package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.Position

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day15 extends App {
  val instructions = Source.fromResource("day15-input.txt").getLines().toList
  val sensorBeaconMap = instructions.map(parseStatement).toMap

  val allCoverage = sensorBeaconMap.map(entry => findCoverageAtSpecificY(entry._1, entry._2, 2000000).toSet).toSet.flatten

  //there is one beacon on the line, so subtract that.
  (allCoverage.size - 1).pipe(println)

  def parseStatement(statement: String) = {
    //statement format: Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    val sensorPos = statement.split(":")(0).substring("Sensor at ".length).split(",")
    val beaconPos = statement.split(":")(1).substring(" closest beacon is at ".length).split(",")

    val sensor = Position(sensorPos(0).trim.substring("x=".length).toInt, sensorPos(1).trim.substring("y=".length).toInt)
    val beacon = Position(beaconPos(0).trim.substring("x=".length).toInt, beaconPos(1).trim.substring("y=".length).toInt)

    sensor -> beacon
  }

  def calculateManhattanDistance(sensor: Position, beacon: Position): Int = {
    (sensor.x - beacon.x).abs + (sensor.y - beacon.y).abs
  }

  def findCoverageAtSpecificY(sensor: Position, beacon: Position, yValue: Int) : List[Int] = {
    val distance = calculateManhattanDistance(sensor, beacon)
    val distanceToTargetRow = (sensor.y - yValue).abs
    val spreadAlongXAtSpecificY = distance - distanceToTargetRow

    spreadAlongXAtSpecificY match {
      case x if x < 0  => List.empty[Int]
      case _ =>  (sensor.x - spreadAlongXAtSpecificY to sensor.x + spreadAlongXAtSpecificY).toList
    }
  }
}
