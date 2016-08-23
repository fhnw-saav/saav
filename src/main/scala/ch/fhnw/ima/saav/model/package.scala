package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.model.domain.{AnalysisBuilder, Entity, Review}

import scala.util.Random

package object model {

  def mockAnalysis = {
    val builder = AnalysisBuilder()

    populateIndicator(builder, "Methodologie", "Klare Fragestellung und Zielsetzung", "Indikator 1")
    populateIndicator(builder, "Methodologie", "Angemessenheit der gewählten Methode(n) zur Beantwortung der Forschungsfrage", "Indikator 1")
    populateIndicator(builder, "Methodologie", "Ergebnisoffene Anlage des Projektes", "Indikator 1")
    populateIndicator(builder, "Methodologie", "Reflexion über die vielfachen Voraussetzungen", "Indikator 1")
    populateIndicator(builder, "Methodologie", "Gebrauch klarer, verständlicher Wissenschaftssprache", "Indikator 1")
    populateIndicator(builder, "Methodologie", "Bewusstsein für ethische und rechtliche Anforderungen in der Forschung", "Indikator 1")

    populateIndicator(builder, "Integration", "Situierung des Projektes zum Forschungsstand", "Indikator 1")
    populateIndicator(builder, "Integration", "Berücksichtigung der relevanten wissenschaftlichen Kenntnisse und Debatten", "Indikator 1")
    populateIndicator(builder, "Integration", "Situierung des Projektes zu vorherrschenden Deutungen, Schulen, Paradigmen", "Indikator 1")
    populateIndicator(builder, "Integration", "Erweiterung des Kenntnisstandes bestehender Forschungsfelder", "Indikator 1")
    populateIndicator(builder, "Integration", "Eröffnung neuer Forschungsfelder", "Indikator 1")

    populateIndicator(builder, "Machbarkeit", "x1", "Indikator 1")
    populateIndicator(builder, "Machbarkeit", "x2", "Indikator 1")
    populateIndicator(builder, "Machbarkeit", "x3", "Indikator 1")

    populateIndicator(builder, "Leistung", "x1", "Indikator 1")
    populateIndicator(builder, "Leistung", "x2", "Indikator 1")
    populateIndicator(builder, "Leistung", "x3", "Indikator 1")
    populateIndicator(builder, "Leistung", "x4", "Indikator 1")

    populateIndicator(builder, "Ver...", "x1", "Indikator 1")

    populateIndicator(builder, "Inf...", "x1", "Indikator 1")

    populateIndicator(builder, "Aus...", "x1", "Indikator 1")


    builder.build

  }

  def alphabetSoupAnalysis = {
    val review = Review("Review")

    val helloWorld = Entity("Hello World")
    val foo = Entity("Foo")
    val bar = Entity("Bar")
    val baz = Entity("Baz")
    val obama = Entity("Obama")
    val clinton = Entity("Clinton")
    val trump = Entity("Trump")
    val chuchichaeschtli = Entity("Chuchichäschtli")

    val entities = Seq(helloWorld, foo, bar, baz, obama, clinton, trump, chuchichaeschtli)

    val builder = AnalysisBuilder()
    val category1 = builder.category("Category A")

    val subCategory11 = category1.subCategory("Length")
    val lengthIndicatorScope = subCategory11.indicator("Length")

    val subCategory12 = category1.subCategory("Consonant vs. Vowel")
    val consonantIndicatorScope = subCategory12.indicator("Consonant Count")
    val vowelIndicatorScope = subCategory12.indicator("Vowel Count")

    val category2 = builder.category("Category B")

    val subCategory21 = category2.subCategory("Whitespace")
    val wordCountIndicator = subCategory21.indicator("Word Count")

    def isVowel(c: Char) = "aeiouäöü".contains(c)

    for (e <- entities) {
      val length = e.name.length
      val consonantCount = e.name.toLowerCase.filterNot(isVowel).length
      val vowelCount = e.name.toLowerCase.filter(isVowel).length
      val wordCount = e.name.filter(_ == ' ').length + 1

      println(s"${e.name}: length $length, $consonantCount consonant(s), $vowelCount vowel(s), $wordCount word(s)")

      lengthIndicatorScope.addValue(e, review, length)
      consonantIndicatorScope.addValue(e, review, consonantCount)
      vowelIndicatorScope.addValue(e, review, vowelCount)
      wordCountIndicator.addValue(e, review, wordCount)
    }

    builder.build

  }

  private def populateIndicator(builder: AnalysisBuilder, categoryName: String, subCategoryName: String, indicatorName: String) = {
    val indicatorScope = builder.category(categoryName).subCategory(subCategoryName).indicator(indicatorName)

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    val r = Random

    for (i <- 1 to 10) {
      val project = Entity(s"Project $i")
      indicatorScope.addValue(project, reviewOne, r.nextInt(100))
      indicatorScope.addValue(project, reviewTwo, r.nextInt(100))
    }
  }

}
