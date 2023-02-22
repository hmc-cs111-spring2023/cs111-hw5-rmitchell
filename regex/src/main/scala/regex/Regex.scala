package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage

case object Epsilon extends RegularLanguage

case class Character(input:Char) extends RegularLanguage

case class Union(input1: RegularLanguage, input2: RegularLanguage) extends RegularLanguage

case class Concat(input1: RegularLanguage, input2: RegularLanguage) extends RegularLanguage

case class Star(input: RegularLanguage) extends RegularLanguage

// Add your definitions here

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = 
  lang match {
    case Concat(Epsilon, input) => simplify(input)
    case Concat(input, Epsilon) => simplify(input)
    case Concat(Empty, input) => Empty
    case Concat(input, Empty) => Empty
    case Concat(input1, input2) => Concat(simplify(input1), simplify(input2))
    case Union(Empty, input) => simplify(input)
    case Union(input, Empty) => simplify(input)
    case Union(input1, input2) => Union(simplify(input1), simplify(input2))
    case Star(Epsilon) => Epsilon
    case Star(Empty) => Empty
    case Star(input) => Star(simplify(input))
    case _ => lang
  } 

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = ???

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = ???

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
