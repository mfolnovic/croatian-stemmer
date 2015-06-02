package net.croz.croatian.stemmer

import java.util.regex.Pattern

import io.Source

object CroatianRuleBasedStemmer {
  private case class Transformation(from: String, to: String)

  private val RulesPath = "/rules.txt"
  private val TransformationsPath = "/transformations.txt"

  private val transformations = parseTransformations(readAllLines(TransformationsPath))
  private val rules = parseRules(readAllLines(RulesPath))

  private def readAllLines(filePath: String) = Source.fromURL(getClass.getResource(filePath))(io.Codec("UTF-8")).getLines().toList

  private def parseRules(lines: List[String]) = {
    lines.map(line => {
      val Array(osnova, nastavak) = line.split(" ")
      Pattern.compile(s"^($osnova)($nastavak)$$")
    })
  }

  private def parseTransformations(lines: List[String]) = {
    lines.map(line => {
      val Array(from, to) = line.split("\\s+")
      Transformation(from, to)
    })
  }

  private def highlightSyllableFormingR(word: String) = word.replaceAll("(^|[^aeiou])r($|[^aeiou])", "$1R$2")
  private def hasVowel(word: String) = {
    val higlightedWord = highlightSyllableFormingR(word)
    "aeiouR".exists(higlightedWord contains _)
  }

  private def transform(word: String) = {
    val transformationOption = transformations.find(transformation => word.endsWith(transformation.from))
    transformationOption match {
      case Some(transformation) => {
        word.take(word.length - transformation.from.length) + transformation.to
      }
      case None => word
    }
  }

  private def root(word: String) = {
    val ruleOption = rules.find(rule => {
      val matcher = rule.matcher(word)
      matcher.find && hasVowel(matcher.group(1)) && matcher.group(1).length() > 1
    })

    ruleOption match {
      case Some(rule) => {
        val matcher = rule.matcher(word)
        matcher.find()
        matcher.group(1)
      }
      case None => word
    }
  }

  def stem(word: String): String = {
    val stepword = root(transform(word))
    if (stepword == word) stepword
    else stem(stepword)
  }
}
