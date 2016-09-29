package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.model.domain._

import scala.util.Random

package object model {

  def mockAnalysis: Analysis = {
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

    populateIndicator(builder, "A very looooong criteria name", "A very loooooong sub-criteria name", "Indikator 1")


    builder.build

  }

  def alphabetSoupAnalysis: Analysis = {
    val review = ReviewId("Review")

    val helloWorld = Entity(EntityId("Hello World"))
    val foo = Entity(EntityId("Foo"))
    val bar = Entity(EntityId("Bar"))
    val baz = Entity(EntityId("Baz"))
    val obama = Entity(EntityId("Obama"))
    val clinton = Entity(EntityId("Clinton"))
    val trump = Entity(EntityId("Trump"))
    val chuchichaeschtli = Entity(EntityId("Chuchichäschtli"))

    val entities = Seq(helloWorld, foo, bar, baz, obama, clinton, trump, chuchichaeschtli)

    val builder = AnalysisBuilder()
    val criteria1 = builder.criteria("Criteria A")

    val subCriteria11 = criteria1.subCriteria("Length")
    val lengthIndicatorScope = subCriteria11.indicator("Length")

    val subCriteria12 = criteria1.subCriteria("Consonant vs. Vowel")
    val consonantIndicatorScope = subCriteria12.indicator("Consonant Count")
    val vowelIndicatorScope = subCriteria12.indicator("Vowel Count")

    val criteria2 = builder.criteria("Criteria B")

    val subCriteria21 = criteria2.subCriteria("Word Count")
    val wordCountIndicator = subCriteria21.indicator("Word Count")

    def isVowel(c: Char) = "aeiouäöü".contains(c)
    def isSpace(c: Char) = c == ' '

    for (e <- entities) {
      val length = e.displayName.length
      val consonantCount = e.displayName.toLowerCase.filterNot(isVowel).filterNot(isSpace).length
      val vowelCount = e.displayName.toLowerCase.filter(isVowel).length
      val wordCount = e.displayName.filter(isSpace).length + 1

      lengthIndicatorScope.addValue(e, review, length)
      consonantIndicatorScope.addValue(e, review, consonantCount)
      vowelIndicatorScope.addValue(e, review, vowelCount)
      wordCountIndicator.addValue(e, review, wordCount)
    }

    builder.build

  }

  private def populateIndicator(builder: AnalysisBuilder, criteriaName: String, subCriteriaName: String, indicatorName: String) = {
    val indicatorScope = builder.criteria(criteriaName).subCriteria(subCriteriaName).indicator(indicatorName)

    val reviewOne = ReviewId("Review 1")
    val reviewTwo = ReviewId("Review 2")

    val r = Random

    for (i <- 1 to 10) {
      val project = Entity(EntityId(s"Project $i"))
      indicatorScope.addValue(project, reviewOne, r.nextInt(100))
      indicatorScope.addValue(project, reviewTwo, r.nextInt(100))
    }
  }

}
