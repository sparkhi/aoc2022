package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.{Addition, Day11Operation, Multiplication, Square}

import scala.collection.mutable
import scala.collection.mutable.Queue
import scala.io.Source

object Day11 extends App {

  val monkeys = parseInput(Source.fromResource("day11-input.txt").getLines().toList)
  val inspectionCountMap: scala.collection.mutable.Map[Int, BigInt] = scala.collection.mutable.HashMap(0 -> 0L, 1 -> 0L, 2 -> 0L, 3 -> 0L, 4 -> 0L, 5 -> 0L, 6 -> 0L, 7 -> 0L)

  for (_ <- 1 to 10000) {
    for (monkeyId <- 0 to 7) {
      runRound(monkeyId)
    }
  }

  val sorted = inspectionCountMap.values.toList.sorted(Ordering.BigInt.reverse)
  println(sorted.head * sorted.tail.head)


  private def runRound(monkeyId: Int) = {
    val monkeyAtWork = monkeys(monkeyId)
    val itemsToWorkWith = monkeyAtWork.items
    while (itemsToWorkWith.nonEmpty) {
      val item = itemsToWorkWith.dequeue()
      inspectionCountMap(monkeyId) = inspectionCountMap(monkeyId) + 1

      //9699690 is the multiple of all numbers that are used in the divisibility test. As we do not really care about
      // individual worry level (the problem only asks to count number of inspections), we MOD the worry level by
      /// 9699690, this keeps the number small for calculation but still allows to produce correct result in the end
      val newItem = monkeyAtWork.operation.operate(item) % 9699690

      if (newItem % monkeyAtWork.testDivisibleBy == 0) {
        monkeys(monkeyAtWork.ifTrueThrowTo).items.enqueue(newItem)
      } else {
        monkeys(monkeyAtWork.ifFalseThrowTo).items.enqueue(newItem)
      }
    }
  }

  //e.g. Operation: new = old * 11
  def parseOperation(str: String) = {
    val expr = str.trim().substring("Operation: new = ".length)
    val tokens = expr.split(" ")
    tokens(1) match {
      case "+" => new Addition(tokens(2).toLong)
      case "*" => {
        if (tokens(2).equals(tokens(0))) {
          new Square
        } else {
          new Multiplication(tokens(2).toLong)
        }
      }
    }
  }

  def createMonkey(details: List[String]): Monkey = {
    val monkeyId = details(0).substring(0, details(0).length - 1).substring("Monkey ".length).toInt //Monkey 0:
    val listOfItems = details(1).substring("  Starting items: ".length).split(", ").map(_.toLong).toList
    val items = mutable.Queue(listOfItems: _*)//  Starting items: 71, 56, 50, 73
    val operation: Day11Operation = parseOperation(details(2))
    val divisibleBy = details(3).substring("  Test: divisible by ".length).toInt
    val ifTrue = details(4).substring("    If true: throw to monkey ".length).toInt
    val ifFalse = details(5).substring("    If false: throw to monkey ".length).toInt

    Monkey(monkeyId, items, operation, divisibleBy, ifTrue, ifFalse)
  }

  def parseInput(monkeyDetails: List[String]) = {
    val groups = monkeyDetails.grouped(7).toList
    val monkeys = groups.map(createMonkey)
    monkeys
  }

}

case class Monkey(id: Int, items: mutable.Queue[Long], operation: Day11Operation, testDivisibleBy: Int, ifTrueThrowTo: Int, ifFalseThrowTo:Int)
