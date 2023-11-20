package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.{Day8TreeVisibilityFinder, Position}

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day8 extends App {
  val treeHeightGrid = Source.fromResource("day8-input.txt").getLines().map(_.toCharArray.map(_.toInt).toList).toList
  val treeHeightGridReverse = treeHeightGrid.map(_.reverse)
  val treeHeightGridTopDown = treeHeightGrid.transpose
  val treeHeightGridTopDownReverse = treeHeightGridTopDown.map(_.reverse)

  val indexOfLastCol = treeHeightGrid.head.length - 1
  val indexOfLastRow = treeHeightGrid.length - 1


  //visible from left
  val leftVisibleTrees: List[List[Int]] = treeHeightGrid.map(Day8TreeVisibilityFinder.getVisibleTrees)
  val leftVisibleTreesWithIndex = leftVisibleTrees.zipWithIndex
  val visibleFromLeft = leftVisibleTreesWithIndex.map(createPositions).flatten(_.toList)

  //visible from right
  val rightVisibleTrees: List[List[Int]] = treeHeightGridReverse.map(Day8TreeVisibilityFinder.getVisibleTrees)
  val adjustedVisibilityToLeftIndexed = rightVisibleTrees.map(_.map(indexOfLastCol - _))
  val rightVisibleTreesWithIndex = adjustedVisibilityToLeftIndexed.zipWithIndex
  val visibleFromRight = rightVisibleTreesWithIndex.map(createPositions).flatten(_.toList)

  //visible from top
  val topVisibleTrees: List[List[Int]] = treeHeightGridTopDown.map(Day8TreeVisibilityFinder.getVisibleTrees)
  val topVisibleTreesWithIndex = topVisibleTrees.zipWithIndex
  val visibleFromTop = topVisibleTreesWithIndex.map(createPositionsVertical).flatten(_.toList)

  //visible from bottom
  val bottomVisibleTrees: List[List[Int]] = treeHeightGridTopDownReverse.map(Day8TreeVisibilityFinder.getVisibleTrees)
  val adjustedVisibilityToTopIndexed = bottomVisibleTrees.map(_.map(indexOfLastRow - _))
  val bottomVisibleTreesWithIndex = adjustedVisibilityToTopIndexed.zipWithIndex
  val visibleFromBottom = bottomVisibleTreesWithIndex.map(createPositionsVertical).flatten(_.toList)

  (visibleFromLeft ++ visibleFromRight ++ visibleFromTop ++ visibleFromBottom).toSet.size.pipe(println)


  def createPositions(trees: (List[Int], Int)): List[Position] = {
    trees._1.map(Position(trees._2, _))
  }

  def createPositionsVertical(trees: (List[Int], Int)): List[Position] = {
    trees._1.map(Position(_, trees._2))
  }

  //viewing distances from left to right
  val leftVisibleDistances = treeHeightGrid.map(Day8TreeVisibilityFinder.getViewingDistanceAheadOfEachTree)
//  leftVisibleDistances.pipe(println)
  val leftVisibleDistancesWithIndex = leftVisibleDistances.zipWithIndex
  leftVisibleDistancesWithIndex.pipe(println)

}


