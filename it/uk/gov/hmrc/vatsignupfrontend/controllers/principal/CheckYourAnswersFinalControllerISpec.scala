
package uk.gov.hmrc.vatsignupfrontend.controllers.principal

import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.vatsignupfrontend.SessionKeys
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.{DirectDebitTermsJourney, FeatureSwitching, FinalCheckYourAnswer}
import uk.gov.hmrc.vatsignupfrontend.helpers.IntegrationTestConstants._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.SubmissionStub.{stubSubmissionFailure, stubSubmissionSuccess}
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.SubscriptionRequestSummaryStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}
import uk.gov.hmrc.vatsignupfrontend.models._

class CheckYourAnswersFinalControllerISpec extends ComponentSpecBase with CustomMatchers with FeatureSwitching {

  "GET /check-your-answers-final" when {
    "feature switch is disabled" should {
      "return NOT_FOUND" in {
        disable(FinalCheckYourAnswer)

        val res = get("/check-your-answers-final",
          Map(SessionKeys.vatNumberKey -> testVatNumber)
        )

        res should have(
          httpStatus(NOT_FOUND)
        )
      }
    }

    "the subscription request summary returned INTERNAL SERVER ERROR" should {
      "return INTERNAL_SERVER_ERROR" in {
        enable(FinalCheckYourAnswer)
        stubAuth(OK, successfulAuthResponse())
        stubGetSubscriptionRequestException(testVatNumber)(INTERNAL_SERVER_ERROR)

        val res = get("/check-your-answers-final",
          Map(SessionKeys.vatNumberKey -> testVatNumber)
        )

        res should have(
          httpStatus(INTERNAL_SERVER_ERROR)
        )
      }
    }

    "the subscription request summary returned other error" should {
      "return SEE_OTHER and restart journey" in {
        enable(FinalCheckYourAnswer)
        stubAuth(OK, successfulAuthResponse())
        stubGetSubscriptionRequestInvalidJson(testVatNumber)(SEE_OTHER)

        val res = get("/check-your-answers-final")

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureVatNumberController.show().url)
        )
      }
    }

    "the subscription request summary returned OK and all data is present in the session" should {
      "return OK" in {
        val model = SubscriptionRequestSummary(
          vatNumber = testVatNumber,
          businessEntity = SoleTrader,
          optSignUpEmail = None,
          transactionEmail = testEmail,
          contactPreference = Digital
        )

        enable(FinalCheckYourAnswer)
        stubAuth(OK, successfulAuthResponse())
        stubGetSubscriptionRequest(testVatNumber)(OK, Some(model))

        val res = get("/check-your-answers-final",
          Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.businessEntityKey -> BusinessEntity.SoleTraderKey,
            SessionKeys.ninoKey -> testNino,
            SessionKeys.emailKey -> testEmail,
            SessionKeys.contactPreferenceKey -> ContactPreference.DigitalKey
          ))

        res should have(
          httpStatus(OK)
        )
      }
    }
  }
  "POST /check-your-answers-final" when {
    "the user has the direct debit attribute on the control list" should {
      "Submit successfully and redirect to information received" in {
        enable(FinalCheckYourAnswer)
        stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
        stubSubmissionSuccess()

        val res = post("/check-your-answers-final", cookies = Map(
          SessionKeys.vatNumberKey -> testVatNumber,
          SessionKeys.hasDirectDebitKey -> "true",
          SessionKeys.acceptedDirectDebitTermsKey -> "true"
        ))()

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.InformationReceivedController.show().url)
        )
      }

      "Submission is unsuccessful" should {
        "return INTERNAL_SERVER_ERROR" in {
          enable(FinalCheckYourAnswer)
          stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
          stubSubmissionFailure()

          val res = post("/check-your-answers-final", cookies = Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.hasDirectDebitKey -> "true",
            SessionKeys.acceptedDirectDebitTermsKey -> "true"
          ))()

          res should have(
            httpStatus(INTERNAL_SERVER_ERROR)
          )
        }
      }
    }
    "the user does not have the direct debit attribute on the control list" should {
      "redirect to resolve-vat-number" when {
        "vat number and acceptedDirectDebit keys missing from session" in {
          enable(FinalCheckYourAnswer)
          stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
          stubSubmissionFailure()

          val res = post("/check-your-answers-final")()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.ResolveVatNumberController.resolve().url)
          )
        }
      }
      "redirect to direct-debit-terms-and-conditions" when {
        "acceptedDirectDebit key is missing from session" in {
          enable(FinalCheckYourAnswer)
          enable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
          stubSubmissionFailure()

          val res = post("/check-your-answers-final", cookies = Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.hasDirectDebitKey -> "true"
          ))()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.DirectDebitTermsAndConditionsController.show().url)
          )
        }
      }
      "redirect to direct-debit-terms-and-conditions" when {
        "acceptedDirectDebit key in session is false" in {
          enable(FinalCheckYourAnswer)
          enable(DirectDebitTermsJourney)
          stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
          stubSubmissionFailure()

          val res = post("/check-your-answers-final", cookies = Map(
            SessionKeys.vatNumberKey -> testVatNumber,
            SessionKeys.hasDirectDebitKey -> "true",
            SessionKeys.acceptedDirectDebitTermsKey -> "false"
          ))()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.DirectDebitTermsAndConditionsController.show().url)
          )
        }
      }
      "redirect to resolve-vat-number" when {
        "vat number key is missing from session" in {
          enable(FinalCheckYourAnswer)
          stubAuth(OK, successfulAuthResponse(vatDecEnrolment))
          stubSubmissionFailure()

          val res = post("/check-your-answers-final", cookies = Map(
            SessionKeys.acceptedDirectDebitTermsKey -> "true"
          ))()

          res should have(
            httpStatus(SEE_OTHER),
            redirectUri(routes.ResolveVatNumberController.resolve().url)
          )
        }
      }
    }
  }
}
