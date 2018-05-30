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

package uk.gov.hmrc.vatsignupfrontend.config

import javax.inject.{Inject, Singleton}

import play.api.Mode.Mode
import play.api.mvc.Call
import play.api.{Configuration, Environment}
import uk.gov.hmrc.domain.{SaUtr, TaxIds}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitching, StubIncorporationInformation}

@Singleton
class AppConfig @Inject()(val runModeConfiguration: Configuration, environment: Environment) extends ServicesConfig with FeatureSwitching {
  //$COVERAGE-OFF$Disabling scoverage for this section because they are generated by the build job
  override protected def mode: Mode = environment.mode

  private def loadConfig(key: String) = runModeConfiguration.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  lazy val baseUrl: String = loadConfig("base.url")
  val contextRoute = "/vat-through-software/sign-up"
  lazy val ggUrl = loadConfig(s"government-gateway.url")
  lazy val ggSignInContinueUrl = s"$baseUrl$contextRoute"

  def ggSignOutUrl(redirectionUrl: String = ggSignInContinueUrl) = s"$ggUrl/gg/sign-out?continue=$redirectionUrl"

  private val contactHost = runModeConfiguration.getString(s"contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = "MTDVAT"

  lazy val assetsPrefix = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  lazy val analyticsToken = loadConfig(s"google-analytics.token")
  lazy val analyticsHost = loadConfig(s"google-analytics.host")
  lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  // $COVERAGE-ON$

  lazy val shutterPage: String = loadConfig("shutter-page.url")

  lazy val govUK: String = loadConfig("gov-uk.url")

  private def whitelistConfig(key: String): Seq[String] =
    runModeConfiguration.getString(key).getOrElse("").split(",").toSeq

  lazy val whitelistIps: Seq[String] = whitelistConfig("ip-whitelist.urls")
  lazy val ipExclusionList: Seq[Call] = whitelistConfig("ip-whitelist.excludeCalls").map(ip => Call("GET", ip))

  lazy val protectedMicroServiceUrl = baseUrl("vat-sign-up") + "/vat-sign-up"
  lazy val storeVatNumberUrl = s"$protectedMicroServiceUrl/subscription-request/vat-number"

  def vatNumberEligibilityUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/mtdfb-eligibility"

  def storeCompanyNumberUrl(vatNumber: String) = s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/company-number"

  def storeEmailAddressUrl(vatNumber: String) = s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/email"

  def storeIdentityVerificationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/identity-verification"

  def incorporationInformationUrl: String =
    if (isEnabled(StubIncorporationInformation)) loadConfig("microservice.services.incorporation-information.stub-url")
    else loadConfig("microservice.services.incorporation-information.url")

  def getCompanyName(companyNumber: String) = s"$incorporationInformationUrl/company-number/$companyNumber/company-name"

  def getCitizenDetailsUrl(sautr: String) = s"$citizenDetailsUrl/citizen-details/sautr/$sautr/"

  lazy val btaUrl = loadConfig("bta.url")

  lazy val agentServicesUrl = loadConfig("agent-services.url")

  // TODO confirm url
  lazy val authoriseAgentUrl = loadConfig("agent-services.url")
  lazy val matchingStubUrl = baseUrl("matching-stub")

  lazy val stubCitizenDetailsUserUrl = s"$matchingStubUrl/dynamic-cid"

  lazy val citizenDetailsUrl = baseUrl("citizen-details")

  lazy val identityVerificationProxyUrl = baseUrl("identity-verification-proxy")

  lazy val identityVerificationStartUrl = identityVerificationProxyUrl + "/identity-verification-proxy/journey/start"

  lazy val identityVerificationFrontendUrl = loadConfig("identity-verification-frontend.url")

  def identityVerificationFrontendRedirectionUrl(link: String) = s"$identityVerificationFrontendUrl$link"

  lazy val contactFrontendUrl = loadConfig("contact-frontend.host")

  lazy val betaFeedbackUrl = s"$contactFrontendUrl/contact/beta-feedback?service=$contactFormServiceIdentifier"

  lazy val betaFeedbackUnauthenticatedUrl = s"$contactFrontendUrl/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"

  lazy val taxEnrolmentsUrl = baseUrl("tax-enrolments")

  /*
  *  This checks to see if the testOnlyDoNotUseInAppConf route is set in configuration instead of the default prod.Routes
  *  This flag can be used by the application to check if the test only routes are enabled. i.e. this flag can be used to
  *  determine the service is not running in the prod environment
  *
  *  One usage of this is in StoreNinoService where we determine if a "True-Client-IP" should be added for the purpose of
  *  matching.
  */
  lazy val hasEnabledTestOnlyRoutes: Boolean =
    runModeConfiguration.getString("application.router").get == "testOnlyDoNotUseInAppConf.Routes"

  lazy val backendFeatureSwitchUrl: String = s"$protectedMicroServiceUrl/test-only/feature-switch"
}
