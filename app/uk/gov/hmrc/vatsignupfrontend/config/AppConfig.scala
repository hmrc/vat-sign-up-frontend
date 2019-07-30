/*
 * Copyright 2019 HM Revenue & Customs
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

import java.net.URLEncoder

import javax.inject.{Inject, Singleton}
import play.api.Mode.Mode
import play.api.i18n.Lang
import play.api.mvc.Call
import play.api.{Configuration, Environment}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{FeatureSwitching, StubIncorporationInformation, WelshTranslation}

@Singleton
class AppConfig @Inject()(val runModeConfiguration: Configuration, environment: Environment) extends ServicesConfig with FeatureSwitching {
  //$COVERAGE-OFF$Disabling scoverage for this section because they are generated by the build job
  override protected def mode: Mode = environment.mode

  private def loadConfig(key: String) = runModeConfiguration.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  def languageMap: Map[String, Lang] = Map(
    "english" -> Lang("en"),
    "cymraeg" -> Lang("cy")
  )

  def routeToSwitchLanguage: String => Call = (lang: String) => uk.gov.hmrc.vatsignupfrontend.controllers.routes.LanguageSwitchController.switchToLanguage(lang)

  def languageTranslationEnabled = isEnabled(WelshTranslation)

  lazy val baseUrl: String = loadConfig("base.url")
  val contextRoute = "/vat-through-software/sign-up"
  lazy val ggUrl: String = loadConfig(s"government-gateway.url")
  lazy val ggSignInContinueUrl = s"$baseUrl$contextRoute"

  lazy val feedbackUrl: String = loadConfig("feedback.url")
  lazy val exitSurveyAgentOrigin = "MTDfB-VAT-agent-led-sign-up"
  lazy val exitSurveyPrincipalOrigin = "MTDfB-VAT-sign-up"
  lazy val agentFeedbackUrl = s"$feedbackUrl/feedback/$exitSurveyAgentOrigin"
  lazy val principalFeedbackUrl = s"$feedbackUrl/feedback/$exitSurveyPrincipalOrigin"
  lazy val createAccountUrl: String = ggUrl + "/login/create-account"

  private def encodeUrl(url: String): String = URLEncoder.encode(url, "UTF-8")

  def ggSignOutUrl(redirectionUrl: String = ggSignInContinueUrl): String = s"$ggUrl/gg/sign-out?continue=${encodeUrl(redirectionUrl)}"

  def ggSignInUrl(redirectionUrl: String = ggSignInContinueUrl): String = s"$ggUrl/gg/sign-in?continue=${encodeUrl(redirectionUrl + "/resolve-vat-number")}"

  private val contactHost = runModeConfiguration.getString(s"contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = "MTDVAT"

  lazy val assetsPrefix: String = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  lazy val analyticsToken: String = loadConfig(s"google-analytics.token")
  lazy val analyticsHost: String = loadConfig(s"google-analytics.host")
  lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  // $COVERAGE-ON$

  lazy val shutterPage: String = loadConfig("shutter-page.url")

  lazy val govUK: String = loadConfig("gov-uk.url")

  lazy val guidancePageUrl: String = s"$govUK/guidance/use-software-to-submit-your-vat-returns"

  lazy val signUpGuidancePageUrl: String = s"$govUK/guidance/sign-up-for-making-tax-digital-for-vat"

  lazy val findLostUtrNumberUrl: String = s"$govUK/find-lost-utr-number"

  lazy val checkNeedForMtdUrl: String = s"$govUK/guidance/check-when-a-business-must-follow-the-rules-for-making-tax-digital-for-vat"

  private def whitelistConfig(key: String): Seq[String] =
    runModeConfiguration.getString(key).getOrElse("").split(",").toSeq

  lazy val whitelistIps: Seq[String] = whitelistConfig("ip-whitelist.urls")
  lazy val ipExclusionList: Seq[Call] = whitelistConfig("ip-whitelist.excludeCalls").map(ip => Call("GET", ip))

  lazy val protectedMicroServiceUrl: String = baseUrl("vat-sign-up") + "/vat-sign-up"
  lazy val storeVatNumberUrl = s"$protectedMicroServiceUrl/subscription-request/vat-number"

  def vatNumberEligibilityUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/mtdfb-eligibility"

  def storeCompanyNumberUrl(vatNumber: String) = s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/company-number"

  def storeEmailAddressUrl(vatNumber: String) = s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/email"

  def storeTransactionEmailAddressUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/transaction-email"

  def storeIdentityVerificationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/identity-verification"

  def storePartnershipInformationUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/partnership-information"

  def storeVatGroupInformationUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/vat-group"

  def storeRegisteredSocietyUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/registered-society"

  def storeTrustInformationUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/trust"

  def storeAdministrativeDivisionUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/administrative-division"

  def storeUnincorporatedAssociationInformationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/unincorporated-association"

  def storeCharityInformationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/charity"

  def storeOverseasInformationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/overseas"

  def storeGovOrgInformationUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/government-organisation"

  def storeContactPreferenceUrl(vatNumber: String): String =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/contact-preference"

  def claimSubscriptionUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/claim-subscription/vat-number/$vatNumber"

  def storeJointVentureInformationUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber/joint-venture"

  def subscriptionRequestSummaryUrl(vatNumber: String) =
    s"$protectedMicroServiceUrl/subscription-request/vat-number/$vatNumber"

  def incorporationInformationUrl: String =
    if (isEnabled(StubIncorporationInformation)) loadConfig("microservice.services.incorporation-information.stub-url")
    else loadConfig("microservice.services.incorporation-information.url")

  def getCompanyName(companyNumber: String): String = s"$incorporationInformationUrl/incorporation-information/$companyNumber/incorporated-company-profile"

  def getCitizenDetailsUrlBySautr(sautr: String): String = s"$citizenDetailsUrl/citizen-details/sautr/$sautr/"
  
  def getCitizenDetailsUrlByNino(nino: String): String = s"$citizenDetailsUrl/citizen-details/nino/$nino/"

  lazy val ctReferenceLookupUrl: String = s"$protectedMicroServiceUrl/subscription-request/ct-reference-check"

  lazy val btaUrl: String = loadConfig("bta.url")

  lazy val btaRedirectUrl: String = "/business-account"

  lazy val btaAddVatUrl: String = s"$btaUrl/add-tax/vat/have-vrn"

  lazy val agentServicesUrl: String = loadConfig("agent-services.url")

  // TODO confirm url
  lazy val authoriseAgentUrl: String = loadConfig("agent-services.url")
  lazy val matchingStubUrl: String = baseUrl("matching-stub")

  lazy val stubCitizenDetailsUserUrl: String = s"$matchingStubUrl/dynamic-cid"

  lazy val citizenDetailsUrl: String = baseUrl("citizen-details")

  lazy val identityVerificationProxyUrl: String = baseUrl("identity-verification-proxy")

  lazy val identityVerificationStartUrl: String = identityVerificationProxyUrl + "/identity-verification-proxy/journey/start"

  lazy val identityVerificationFrontendUrl: String = loadConfig("identity-verification-frontend.url")

  def identityVerificationFrontendRedirectionUrl(link: String): String = s"$identityVerificationFrontendUrl$link"

  lazy val contactFrontendUrl: String = loadConfig("contact-frontend.host")

  lazy val betaFeedbackUrl: String = s"$contactFrontendUrl/contact/beta-feedback?service=$contactFormServiceIdentifier"

  lazy val betaFeedbackUnauthenticatedUrl: String = s"$contactFrontendUrl/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"

  lazy val taxEnrolmentsUrl: String = baseUrl("tax-enrolments")

  lazy val softwareOptionsUrl: String = govUK + "/guidance/find-software-thats-compatible-with-making-tax-digital-for-vat"

  lazy val makingTaxDigitalSoftwareUrl: String = loadConfig("software-choices.url")

  lazy val makingTaxDigitalForVatCollection: String = govUK + "/government/collections/making-tax-digital-for-vat"

  lazy val recoverAccountUrl: String = govUK + "/log-in-register-hmrc-online-services/problems-signing-in"

  lazy val directDebitTermsAndConditionsUrl: String = "https://www.tax.service.gov.uk/direct-debit/vat/terms-and-conditions"

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

  lazy val backendDDConfigUrl: String = s"$protectedMicroServiceUrl/test-only/dd-config"

  lazy val companiesHouse: String = loadConfig("companies-house.url")

  lazy val countdownLength: String = loadConfig("timeout.countdown")

  lazy val timeoutLength: String = loadConfig("timeout.length")

}
