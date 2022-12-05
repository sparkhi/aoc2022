package com.knowingwhere.aoc2022

import scala.collection.mutable
import scala.io.Source

object Day5 extends App {
  val allLines = Source.fromResource("day5-input.txt").getLines().toList
  val stacksAndInstructions = allLines.filter(_.nonEmpty).splitAt(9)

  def rPad(eachString: String) = {
    (eachString + "                                        ").substring(0, 35)
  }

  val stackStrings = stacksAndInstructions._1.map(eachString => rPad(eachString))
  val instructions = stacksAndInstructions._2

  var stacks = createStacks(stackStrings)
  instructions.foreach(instruction => parseAndApplyInstruction(instruction))
  val heads = stacks.map(_.head).mkString.replaceAll("\\[", "").replaceAll("]", "").replaceAll(" ", "")
  println(heads)

  def parseAndApplyInstruction(instruction: String): Unit = {
    val params = instruction.split(" ").toList
    val howMany = params(1).toInt
    val fromStack = params(3).toInt
    val toStack = params(5).toInt
    applyInstructionPart2(howMany, fromStack - 1, toStack - 1)
  }

  def applyInstruction(howMany: Int, fromStack: Int, toStack: Int): Unit = {
    for (idx <- 0 until howMany) {
      val popped = stacks(fromStack).pop()
      stacks(toStack).push(popped)
    }
  }

  def applyInstructionPart2(howMany: Int, fromStack: Int, toStack: Int): Unit = {
    val tempStack = mutable.Stack[String]()
    for (idx <- 0 until howMany) {
      val popped = stacks(fromStack).pop()
      tempStack.push(popped)
      //stacks(toStack).push(popped)
    }
    for (idx <- 0 until howMany) {
      val popped = tempStack.pop()
      stacks(toStack).push(popped)
    }
  }


  def createStacks(stackStrings: List[String]) = {
    val stackArrangement = stackStrings.reverse.tail
    val grouped = stackArrangement.map(each => each.grouped(4).toList)
    val transposed = grouped.transpose
    val stacks: List[mutable.Stack[String]] = transposed.map(eachgroup => populateStack(eachgroup))
    stacks
  }

  def populateStack(eachgroup: List[String]) = {
    val stack = mutable.Stack[String]()
    stack.pushAll(eachgroup.filter(_.trim.nonEmpty))
    stack
  }

}
