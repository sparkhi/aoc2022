package com.knowingwhere.aoc2022.util

trait Day11Operation {
  def operate (param1: Long): Long

}

class Multiplication (someConstant: Long) extends Day11Operation {
  override def operate(param1: Long): Long = param1 * someConstant
  override def toString: String = "old * " + someConstant
}

class Addition (someConstant: Long) extends Day11Operation {
  override def operate(param1: Long): Long = param1 + someConstant
  override def toString: String = "old + " + someConstant
}

class Square extends Day11Operation {
  override def operate(param1: Long): Long = param1 * param1
  override def toString: String = "old * old"
}
