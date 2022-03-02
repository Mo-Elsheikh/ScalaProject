/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Homework 03 - LexicalAnalyzer (an iterable lexical analyzer)
 * Student(s) Name(s): Mohamed Elsheikh
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS, WORD_TO_TOKEN}

import scala.io.Source

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme]{

  var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + NEW_LINE
  input = input.trim

  // checks if reached eof
  private def eof: Boolean = {

    input.length == 0

  }

  // returns the current char (requires checking for eof before call)
  private def getChar(): Char = {
    input(0)
  }

  // advances the input one character (requires checking for eof before call)
  private def nextChar() = {
    input = input.substring(1)
  }

  // checks if input has a blank character ahead
  private def hasBlank(): Boolean = {
    BLANKS.contains(getChar)
  }

  // reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    while (!eof && hasBlank)
        nextChar
  }

  // checks if input has a letter ahead
  private def hasLetter(): Boolean = {
    LETTERS.contains(getChar)
  }

  // checks if input has a digit ahead
  private def hasDigit(): Boolean = {
    DIGITS.contains(getChar)
  }

  // checks if input has a special character ahead
  private def hasSpecial(): Boolean = {
    SPECIALS.contains(getChar)
  }

  // checks if input has a punctuation character ahead
  private def hasPunctuation(): Boolean = {
    PUNCTUATIONS.contains(getChar)
  }

  // returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {

    new Iterator[Lexeme] {

      // returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      }

      // returns the next lexeme (or end of line if there isn't any lexeme left to be read)
      // TODO: finish this part of the code
      override def next(): Lexeme = {
        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        var c = getChar
        // Check if the tokens have a letter, add getChar, and match with map.
        if (hasLetter) {
          var str = c + ""
          nextChar
          while (!eof && hasDigit() || hasLetter() ) {
            str += getChar
            nextChar
          }
          str match {
            case "abstract" => return new Lexeme(str, Token.ABSTRACT)
            case "class"    => return new Lexeme(str, Token.CLASS)
            case "public"   => return new Lexeme(str, Token.PUBLIC)
            case  "final"   => return new Lexeme(str, Token.FINAL)
            case "extends"  => return new Lexeme(str, Token.EXTENDS)
            case "implements" => return new Lexeme(str, Token.IMPLEMENTS)
            case _ => return new Lexeme(str, Token.IDENTIFIER)
          }
        }
          // Check if the tokens have a comma
          else if (c == ',') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.COMMA)
          }
          // Check if the tokens have a block closing.
          else if (c == '}') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.BLOCK_CLOSE)
          }
          // Check if the tokens have a block opening.
          else if (c == '{') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.BLOCK_OPEN)
          }
          // Check if the tokens have a block special and add getCha.
          else if (hasSpecial) {
          var str = c + ""
          nextChar
          while (!eof && hasDigit() || hasLetter() || hasSpecial()) {
            str += getChar
            nextChar
          }
            return new Lexeme(str, Token.IDENTIFIER)
          }
          // Check if the tokens have a digit and add getChar
          else if (hasDigit) {
            var str = ""
            while (!eof && hasDigit()) {
              str += getChar
              nextChar
           }
          }
        //throw an exception if an unrecognizable symbol is found
        throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
      }
    }
  }
}

object LexicalAnalyzer {
  val BLANKS       = " \n\t"
  val NEW_LINE     = '\n'
  val LETTERS      = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS       = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS     = "<_@#$%^&()-+='/\\[]{}|"
  val WORD_TO_TOKEN = Map(
    "class"      -> Token.CLASS,
    "public"     -> Token.PUBLIC,
    "abstract"   -> Token.ABSTRACT,
    "final"      -> Token.FINAL,
    "extends"    -> Token.EXTENDS,
    "implements" -> Token.IMPLEMENTS,
  )

  def main(args: Array[String]): Unit = {

    // checks the command-line for source file
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    // iterates over the lexical analyzer, printing the lexemes found
    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext)
      println(it.next())

  } // end main method
}
