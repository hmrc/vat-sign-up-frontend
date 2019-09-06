/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.vatsignupfrontend.helpers

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.http.HeaderNames
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.{Application, Environment, Mode}
import uk.gov.hmrc.play.test.UnitSpec
import SessionCookieBaker._
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitch, FeatureSwitching}

trait ComponentSpecBase extends UnitSpec with GuiceOneServerPerSuite with WiremockHelper with BeforeAndAfterAll
 with FeatureSwitching with BeforeAndAfterEach {
  lazy val ws = app.injector.instanceOf[WSClient]

  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .in(Environment.simple(mode = Mode.Dev))
    .configure(config)
    .build
  val mockHost = WiremockHelper.wiremockHost
  val mockPort = WiremockHelper.wiremockPort.toString
  val mockUrl = s"http://$mockHost:$mockPort"
  val administrativeDivisionVRN = "543278967"

  def config: Map[String, String] = Map(
    "auditing.enabled" -> "false",
    "play.filters.csrf.header.bypassHeaders.Csrf-Token" -> "nocheck",
    "microservice.services.auth.host" -> mockHost,
    "microservice.services.auth.port" -> mockPort,
    "microservice.services.vat-sign-up.host" -> mockHost,
    "microservice.services.vat-sign-up.port" -> mockPort,
    "microservice.services.identity-verification-proxy.host" -> mockHost,
    "microservice.services.identity-verification-proxy.port" -> mockPort,
    "microservice.services.citizen-details.host" -> mockHost,
    "microservice.services.citizen-details.port" -> mockPort,
    "microservice.services.incorporation-information.url" -> mockUrl,
    "administrative-divisions" -> administrativeDivisionVRN
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    startWiremock()
  }

  override def afterAll(): Unit = {
    stopWiremock()
    super.afterAll()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    FeatureSwitch.switches foreach disable
    resetWiremock()
  }

  def get(uri: String, cookies: Map[String, String] = Map.empty): WSResponse =
    await(
      buildClient(uri)
        .withHeaders(HeaderNames.COOKIE -> cookieValue(cookies))
        .get()
    )

  def post(uri: String, cookies: Map[String, String] = Map.empty)(form: (String, String)*): WSResponse = {
    val formBody = (form map { case (k, v) => (k, Seq(v)) }).toMap
    await(
      buildClient(uri)
        .withHeaders(HeaderNames.COOKIE -> cookieValue(cookies), "Csrf-Token" -> "nocheck")
        .post(formBody)
    )
  }

  val baseUrl: String = "/vat-through-software/sign-up"

  def buildClient(path: String) = ws.url(s"http://localhost:$port$baseUrl$path").withFollowRedirects(false)

  protected lazy val appConfig: AppConfig = app.injector.instanceOf[AppConfig]

}
