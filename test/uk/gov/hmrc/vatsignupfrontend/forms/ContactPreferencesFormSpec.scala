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

package uk.gov.hmrc.vatsignupfrontend.forms

import play.api.data.FormError
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.forms.ContactPreferencesForm._
import uk.gov.hmrc.vatsignupfrontend.models.{Digital, Paper}

class ContactPreferencesFormSpec extends UnitSpec {
  "contactPreferencesForm" should {

    val agentContactPreferencesErrorKey = "error.agent.contact-preference"
    val principalContactPreferencesErrorKey = "error.principal.receive_email_notifications"
    val validateContactPreferencesForm = contactPreferencesForm(isAgent = false)

    "successfully parse digital" in {
      val res = validateContactPreferencesForm.bind(Map(contactPreference -> digital))
      res.value should contain(Digital)
    }

    "successfully parse paper" in {
      val res = validateContactPreferencesForm.bind(Map(contactPreference -> paper))
      res.value should contain(Paper)
    }

    "fail when it is not an expected value in the agent view" in {
      val validateContactPreferencesForm = contactPreferencesForm(isAgent = true)
      val res = validateContactPreferencesForm.bind(Map(contactPreference -> "invalid"))
      res.errors should contain(FormError(contactPreference, agentContactPreferencesErrorKey))
    }

    "fail when it is not an expected value in the principal view" in {
      val res = validateContactPreferencesForm.bind(Map(contactPreference -> "invalid"))
      res.errors should contain(FormError(contactPreference, principalContactPreferencesErrorKey))
    }
  }
}
