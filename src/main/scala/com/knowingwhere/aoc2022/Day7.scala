package com.knowingwhere.aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day7 extends App {
  val instructions = Source.fromResource("day7-input.txt").getLines().toList
  var pathMap: Map[String, Int] = Map("//" -> 0)
  loadPathToSizeMap(instructions, "") //bad coding side effect :(

  val directories = pathMap.keySet.filter(_.endsWith("/")).toList

  val thisOne = pathMap.keySet.filter(_.length < 3)

  val directoryToSizeMap = directories.map(eachDir => getSizeOfDir(pathMap, eachDir)).toMap

  directoryToSizeMap.filter(_._2 <= 100000).values.sum.pipe(println)

  //part 2
  val spaceToFreeUp = 30000000 - (70000000 - directoryToSizeMap.values.max)
  directoryToSizeMap.filter(_._2 > spaceToFreeUp).values.min.pipe(println)

  def getSizeOfDir(pathMap: Map[String, Int], eachDir: String): (String, Int) = {
    val filteredMap = pathMap.filter(_._1.startsWith(eachDir))
    eachDir -> filteredMap.values.sum
  }

  //the first instruction of "cd /" does not add the root directory in the map, so it is hacked by
  // initialising the map with the root directory and iterate over rest of the instructions
  @tailrec
  def loadPathToSizeMap(instructions: List[String], currentDir: String): Unit = {
    if (instructions.nonEmpty) {
      val instruction = instructions.head

      if (instruction.startsWith("$ cd")) { //change the current directory and load rest of the tree
        val cmdDetails = instruction.split(" ")
        if (cmdDetails(2).equals("..")) {
          val parent: String = currentDir.split("/").reverse.tail.reverse.mkString("/")
          loadPathToSizeMap(instructions.tail, parent + "/")
        } else {
          loadPathToSizeMap(instructions.tail, currentDir + instruction.substring(5) + "/")
        }
      } else if (instruction.startsWith("$ ls")) { //simply move on to load rest of the tree
        loadPathToSizeMap(instructions.tail, currentDir)
      } else if (instruction.startsWith("dir")) { //load directory of size 0
        pathMap = pathMap + (currentDir + instruction.substring(4) + "/" -> 0)
        loadPathToSizeMap(instructions.tail, currentDir)
      } else {
        val fileDetails = instruction.split(" ") //load file and its size
        pathMap = pathMap + (currentDir + fileDetails(1) -> fileDetails(0).toInt)
        loadPathToSizeMap(instructions.tail, currentDir)
      }
    }
  }
}

