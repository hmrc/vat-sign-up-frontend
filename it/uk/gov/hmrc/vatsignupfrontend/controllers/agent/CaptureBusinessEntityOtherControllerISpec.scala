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

package uk.gov.hmrc.vatsignupfrontend.controllers.agent

import play.api.http.Status._
import uk.gov.hmrc.vatsignupfrontend.config.featureswitch._
import uk.gov.hmrc.vatsignupfrontend.forms.OtherBusinessEntityForm._
import uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks.AuthStub._
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class CaptureBusinessEntityOtherControllerISpec extends ComponentSpecBase with CustomMatchers {

  override def afterAll(): Unit = {
    super.afterAll()
    disable(DivisionJourney)
    disable(OptionalSautrJourney)
  }

  "GET /business-type" should {
    "return an OK" in {
      stubAuth(OK, successfulAuthResponse(agentEnrolment))

      val res = get("/client/business-type-other")

      res should have(
        httpStatus(OK)
      )
    }
  }

  "POST /business-type" should {

    "the business type is vat group" should {
      "return a SEE_OTHER status and go to vat group resolver" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        val res = post("/client/business-type-other")(businessEntity -> vatGroup)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.VatGroupResolverController.resolve().url)
        )
      }
    }

    "the business type is division" should {
      "return a SEE_OTHER status and go to division resolver" in {
        enable(DivisionJourney)
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        val res = post("/client/business-type-other")(businessEntity -> division)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.DivisionResolverController.resolve().url)
        )
      }
    }

    "the business type is Unincorporated Association" should {
      "return a SEE_OTHER status and go to the unincorporated association resolver" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        enable(UnincorporatedAssociationJourney)

        val res = post("/client/business-type-other")(businessEntity -> unincorporatedAssociation)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.UnincorporatedAssociationResolverController.resolve().url)
        )
      }
    }

    "the business type is trust" should {
      "redirect to trust resolver controller" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        enable(TrustJourney)

        val res = post("/client/business-type-other")(businessEntity -> trust)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.TrustResolverController.resolve().url)
        )
      }
    }

    "the business type is registered society" should {
      "redirect to capture registered society company number" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        enable(RegisteredSocietyJourney)

        val res = post("/client/business-type-other")(businessEntity -> registeredSociety)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CaptureRegisteredSocietyCompanyNumberController.show().url)
        )
      }
    }

    "the business type is charity" should {
      "redirect to charity resolver controller" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))

        val res = post("/client/business-type-other")(businessEntity -> charity)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.CharityResolverController.resolve().url)
        )
      }
    }

    "the business type is government organisation" should {
      "redirect to government organisation resolver controller" in {
        stubAuth(OK, successfulAuthResponse(agentEnrolment))
        enable(GovernmentOrganisationJourney)

        val res = post("/client/business-type-other")(businessEntity -> governmentOrganisation)

        res should have(
          httpStatus(SEE_OTHER),
          redirectUri(routes.GovernmentOrganisationResolverController.resolve().url)
        )
      }
    }
  }
}
