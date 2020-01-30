import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning

object AppDependencies {

  lazy val appDependencies: Seq[ModuleID] = compile ++ test ++ it

  private val domainVersion = "5.6.0-play-26"
  private val playWhitelistFilterVersion = "3.1.0-play-26"

  private val scalaTestVersion = "3.0.8"
  private val scalaTestPlusVersion = "3.1.3"
  private val pegdownVersion = "1.6.0"
  private val jsoupVersion =  "1.8.1"
  private val mockitoVersion = "2.7.6"
  private val wiremockVersion = "2.25.1"

  val compile = Seq(
    "uk.gov.hmrc" %% "govuk-template" % "5.48.0-play-26",
    "uk.gov.hmrc" %% "play-ui" % "8.8.0-play-26",
    ws,
    "uk.gov.hmrc" %% "bootstrap-play-26" % "1.3.0",
    "uk.gov.hmrc" %% "domain" % domainVersion,
    "uk.gov.hmrc" %% "play-whitelist-filter" % playWhitelistFilterVersion,
    "uk.gov.hmrc" %% "play-language" % "4.2.0-play-26"
  )

  def test = {
    val scope = "test,it"
    Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % scope,
      "org.scalatestplus.play" %% "scalatestplus-play" % scalaTestPlusVersion % scope,
      "org.pegdown" % "pegdown" % pegdownVersion % scope,
      "org.jsoup" % "jsoup" % jsoupVersion % scope,
      "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
      "org.mockito" % "mockito-core" % mockitoVersion % scope
    )
  }

  def it = {
    val scope = "it"

    Seq(
      "com.github.tomakehurst" % "wiremock-jre8" % wiremockVersion % scope
    )
  }

}
