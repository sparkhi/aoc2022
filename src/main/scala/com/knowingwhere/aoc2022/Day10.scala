package com.knowingwhere.aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day10 extends App {

  val instructions = Source.fromResource("day10-input.txt").getLines().toList
  val processedValues = processInstructions(instructions, ProcessDetails(0, 1), List.empty[(Int, Int)])
  val filterList = List(20, 60, 100, 140, 180, 220, 260, 300, 340)
  val filtered = processedValues._2.filter(thingy => filterList.contains(thingy._1))
  filtered.map(thingy => thingy._1 * thingy._2).sum.pipe(println)

  val drawing = createDrawing(processedValues._2, "")
  drawing.grouped(40).toList.map(_.pipe(println))

  def getCharToDraw(cycleNumber: Int, spriteLocation: Int) = {
    val currentDrawPosition = (cycleNumber % 40) - 1
    val spriteRange = List(spriteLocation - 1, spriteLocation, spriteLocation + 1)
    if (spriteRange.contains(currentDrawPosition)) {
      "#"
    } else {
      "."
    }
  }

  @tailrec
  def createDrawing(cyclePositionDetails: List[(Int, Int)], drawingAccumulator: String): String = {
    if (cyclePositionDetails.isEmpty) {
      drawingAccumulator
    } else {
      val instruction = cyclePositionDetails.head
      val charToDraw = getCharToDraw(instruction._1, instruction._2)
      createDrawing(cyclePositionDetails.tail, drawingAccumulator + charToDraw)
    }
  }


  def processNoop(processDetails: ProcessDetails, instructionAccumulator: List[(Int, Int)]): (ProcessDetails, List[(Int, Int)]) = {
    val addInstruction = processDetails.completedCycles + 1 -> processDetails.xValue
    val newAccumulator = instructionAccumulator :+ addInstruction
    ProcessDetails(processDetails.completedCycles + 1, processDetails.xValue) -> newAccumulator
  }

  @tailrec
  def processAddx(iter: Int, addValue: Int, processDetails: ProcessDetails, instructionsAccumulator: List[(Int, Int)]): (ProcessDetails, List[(Int, Int)]) = {
    if (iter < 1) {
      val newProcessDetails = ProcessDetails(processDetails.completedCycles + 1, processDetails.xValue)
      val newInstructionAccumulator = instructionsAccumulator :+ processDetails.completedCycles + 1 -> processDetails.xValue
      processAddx(iter + 1, addValue, newProcessDetails, newInstructionAccumulator)
    } else {
      val newX = processDetails.xValue + addValue
      val newInstructionAccumulator = instructionsAccumulator :+ processDetails.completedCycles + 1 -> processDetails.xValue
      val newProcessDetails = ProcessDetails(processDetails.completedCycles + 1, newX)
      newProcessDetails -> newInstructionAccumulator
    }
  }

  @tailrec
  def processInstructions(instructions: List[String], processDetails: ProcessDetails, instructionAccumulator: List[(Int, Int)]):(ProcessDetails, List[(Int, Int)]) = {
    if (instructions.isEmpty) {
      processDetails -> instructionAccumulator
    } else {
      val instruction = instructions.head
      instruction match {
        case "noop" =>
          val result = processNoop(processDetails, instructionAccumulator)
          processInstructions(instructions.tail, result._1, result._2)
        case _ =>
          val valueToAdd = instruction.split(" ")(1).toInt
          val result = processAddx(0, valueToAdd, processDetails, instructionAccumulator)
          processInstructions(instructions.tail, result._1, result._2)
      }
    }
  }
}

case class ProcessDetails(completedCycles: Int, xValue: Int)

