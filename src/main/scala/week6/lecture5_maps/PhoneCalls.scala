package week6.lecture5_maps

import scala.io.Source

object PhoneCalls extends App {
  val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  val words = in.getLines.filter(_.forall(_.isLetter)).toList

  val mnem = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ",
  )

  /**
    * Invert the mnem map to give a map from chars 'A' ... 'Z' to '2' ... '9'
    * Char -> Int
    */
  val charCode: Map[Char, Char] = mnem.flatMap{case (k,v) => v.map(_ -> k)}

  /**
    * Maps a word to the digit string it can represent, e.g. "Java" -> "5282"
    */
  def wordCode(word: String): String = word.toUpperCase.map(charCode)

  /**
    * A map from digit strings to the words that represent them,
    * e.g. "5282" -> List("Java", "Kata", "Lava", ...)
    * Note: A missing number should map to the empty set, e.g. "1111" -> List()
    */
  val wordsForNum: Map[String, List[String]] = words groupBy wordCode withDefaultValue List()

  /**
    * Return all ways to encode a number as a list of words
    * eg. encode"7225247386"
    */
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")
  println(translate("7225247386"))
}
