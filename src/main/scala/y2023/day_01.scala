package com.kaaveland.aoc
package y2023

object day_01 {

  def numericValue(s: Array[Char]): Option[Int] = s.headOption.filter(_.isDigit).map(_.asDigit)
  private val numberWords =
    Vector("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").map(_.toCharArray)

  private def numberWordValue(s: Array[Char]): Option[Int] = {
    numberWords.zipWithIndex
      .find { case (word, _) =>
        s.startsWith(word)
      }
      .map { case (_, idx) => idx + 1 }
  }

  private def firstNum(s: List[Array[Char]]): Int = s.iterator.map(numericValue).find(_.isDefined).flatten.get
  private def firstNumIncludingWords(s: List[Array[Char]]): Int =
    s.iterator.map { tail => numericValue(tail).orElse(numberWordValue(tail)) }.find(_.isDefined).flatten.get

  private def tailsOf(s: String) = s.linesIterator.filter(_.nonEmpty).map(_.toCharArray.tails.toList)

  def part2(s: String): Int =
    tailsOf(s).map { tails =>
      10 * firstNumIncludingWords(tails) + firstNumIncludingWords(tails.reverse)
    }.sum

  def part1(s: String): Int =
    tailsOf(s).map { tails =>
      10 * firstNum(tails) + firstNum(tails.reverse)
    }.sum

}
