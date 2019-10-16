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

package uk.gov.hmrc.vatsignupfrontend.config.featureswitch

import uk.gov.hmrc.vatsignupfrontend.config.featureswitch.FeatureSwitch.prefix

sealed trait FeatureSwitch {
  val name: String
  val displayText: String
}

object FeatureSwitch {
  val prefix = "feature-switch"

  val switches: Set[FeatureSwitch] = Set(
    StubIncorporationInformation,
    BTAClaimSubscription,
    AdditionalKnownFacts,
    DirectDebitTermsJourney,
    SendYourApplication,
    OptionalSautrJourney,
    SkipCidCheck,
    WelshTranslation,
    SkipCtUtrOnCotaxNotFound,
    DirectToCTUTROnMismatchedCTUTR,
    FinalCheckYourAnswer,
    GeneralPartnershipNoSAUTR,
    ReSignUpJourney,
    DivisionLookupJourney
  )

  def apply(str: String): FeatureSwitch =
    switches find (_.name == str) match {
      case Some(switch) => switch
      case None => throw new IllegalArgumentException("Invalid feature switch: " + str)
    }

  def get(str: String): Option[FeatureSwitch] = switches find (_.name == str)

}

case object StubIncorporationInformation extends FeatureSwitch {
  override val name: String = s"$prefix.stub-incorporation-information"
  override val displayText: String = "Use Stub for Incorporation Information Connection"
}

case object BTAClaimSubscription extends FeatureSwitch {
  override val name: String = s"$prefix.bta-claim-subscription"
  override val displayText: String = "Enable users from BTA to claim their subscription through VAT sign up"
}

case object WelshTranslation extends FeatureSwitch {
  override val name: String = s"$prefix.welsh-translation"
  override val displayText: String = "Enable welsh language"
}

case object AdditionalKnownFacts extends FeatureSwitch {
  override val name: String = s"$prefix.additional-known-facts"
  override val displayText: String = "Enable the two additional Known Facts"
}

case object DirectDebitTermsJourney extends FeatureSwitch {
  override val name: String = s"$prefix.direct-debit-terms-journey"
  override val displayText: String = "Enable the Direct Debit T&Cs Journey"
}

case object OptionalSautrJourney extends FeatureSwitch {
  override val name: String = s"$prefix.optional-sautr-journey"
  override val displayText: String = "Enable the 'Do you have an SAUTR' question for general partnerships"
}

case object SendYourApplication extends FeatureSwitch {
  override val name: String = s"$prefix.send-your-application"
  override val displayText: String = "Enable the Send Your Application view (replaces Terms of Participation)"
}

case object SkipCidCheck extends FeatureSwitch {
  override val name: String = s"$prefix.disable-cid-check"
  override val displayText: String = "Disable checking the user's NINO via CID"
}

case object SkipCtUtrOnCotaxNotFound extends FeatureSwitch {
  override val name: String = s"$prefix.skip-ctutr-on-cotax-not-found"
  override val displayText: String = "Enable users to skip CT UTR if there is no match on COTAX"
}

case object DirectToCTUTROnMismatchedCTUTR extends FeatureSwitch {
  override val name: String = s"$prefix.direct-to-ctutr-on-mismatched-ctutr"
  override val displayText: String = "Direct users to capture CT UTR page when present credential causes a mismatch"
}

case object FinalCheckYourAnswer extends FeatureSwitch {
  override val name: String = s"$prefix.final-check-your-answer"
  override val displayText: String = "Enable users to view check your answers and declaration page for individual or agent "
}

case object GeneralPartnershipNoSAUTR extends FeatureSwitch {
  override val name: String = s"$prefix.General-partnership-no-sautr"
  override val displayText: String = "Enable users to view General Partnership No SAUTR page for individual or agent "
}

case object DivisionLookupJourney extends FeatureSwitch {
  override val name: String = s"$prefix.division-lookup-journey"
  override val displayText: String = "Check VRN against list of known Administrative Divisions"
}

case object ReSignUpJourney extends FeatureSwitch {
  override val name: String = s"$prefix.re-sign-up-journey"
  override val displayText: String = "Enable the Resignup journey for users already migrated to ETMP"
}
