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

package uk.gov.hmrc.vatsignupfrontend.models

import play.api.libs.json._
import uk.gov.hmrc.vatsignupfrontend.utils.SessionUtils.SessionFormatter

sealed trait BusinessEntity {

  import BusinessEntity._

  override def toString: String = this match {
    case LimitedCompany => LimitedCompanyKey
    case SoleTrader => SoleTraderKey
    case GeneralPartnership => GeneralPartnershipKey
    case LimitedPartnership => LimitedPartnershipKey
    case LimitedLiabilityPartnership => LimitedLiabilityPartnershipKey
    case ScottishLimitedPartnership => ScottishLimitedPartnershipKey
    case VatGroup => VatGroupKey
    case Division => DivisionKey
    case UnincorporatedAssociation => UnincorporatedAssociationKey
    case Trust => TrustKey
    case RegisteredSociety => RegisteredSocietyKey
    case Charity => CharityKey
    case Overseas => OverseasKey
    case GovernmentOrganisation => GovernmentOrganisationKey
    case Other => OtherKey
  }
}

object GeneralPartnership extends BusinessEntity

trait LimitedPartnershipBase extends BusinessEntity

object LimitedPartnership extends LimitedPartnershipBase

object LimitedLiabilityPartnership extends LimitedPartnershipBase

object ScottishLimitedPartnership extends LimitedPartnershipBase

object LimitedCompany extends BusinessEntity

object SoleTrader extends BusinessEntity

object VatGroup extends BusinessEntity

object Division extends BusinessEntity

object UnincorporatedAssociation extends BusinessEntity

object Trust extends BusinessEntity

object RegisteredSociety extends BusinessEntity

object Charity extends BusinessEntity

object Overseas extends BusinessEntity

object GovernmentOrganisation extends BusinessEntity

object Other extends BusinessEntity

object BusinessEntity {
  val LimitedCompanyKey = "limited-company"
  val SoleTraderKey = "sole-trader"
  val GeneralPartnershipKey = "general-partnership"
  val LimitedPartnershipKey = "limited-partnership"
  val LimitedLiabilityPartnershipKey = "llp"
  val ScottishLimitedPartnershipKey = "scottish-partnership"
  val VatGroupKey = "vat-group"
  val DivisionKey = "division"
  val UnincorporatedAssociationKey = "unincorporated-association"
  val TrustKey = "trust"
  val RegisteredSocietyKey = "registered-society"
  val CharityKey = "charity"
  val OverseasKey = "overseas"
  val GovernmentOrganisationKey = "government-organisation"
  val OtherKey = "other"

  private def stringToOptBusinessEntity(businessEntity:String): Option[BusinessEntity] = businessEntity match {
    case LimitedCompanyKey => Some(LimitedCompany)
    case SoleTraderKey => Some(SoleTrader)
    case GeneralPartnershipKey => Some(GeneralPartnership)
    case LimitedPartnershipKey => Some(LimitedPartnership)
    case LimitedLiabilityPartnershipKey => Some(LimitedLiabilityPartnership)
    case ScottishLimitedPartnershipKey => Some(ScottishLimitedPartnership)
    case VatGroupKey => Some(VatGroup)
    case DivisionKey => Some(Division)
    case UnincorporatedAssociationKey => Some(UnincorporatedAssociation)
    case TrustKey => Some(Trust)
    case RegisteredSocietyKey => Some(RegisteredSociety)
    case CharityKey => Some(Charity)
    case OverseasKey => Some(Overseas)
    case GovernmentOrganisationKey => Some(GovernmentOrganisation)
    case OtherKey => Some(Other)
    case _ => None
  }

  private def backendToFrontendBusinessEntity(businessEntity: String): Option[BusinessEntity] = businessEntity match {
    case "limitedCompany" => Some(LimitedCompany)
    case "soleTrader" => Some(SoleTrader)
    case "generalPartnership" => Some(GeneralPartnership)
    case "limitedPartnership" => Some(LimitedPartnership)
    case "limitedLiabilityPartnershipKey" => Some(LimitedLiabilityPartnership)
    case "scottishLimitedPartnershipKey" => Some(ScottishLimitedPartnership)
    case "vatGroup" => Some(VatGroup)
    case "administrativeDivision" => Some(Division)
    case "unincorporatedAssociation" => Some(UnincorporatedAssociation)
    case "trust" => Some(Trust)
    case "registeredSociety" => Some(RegisteredSociety)
    case "charity" => Some(Charity)
    case "nonUKCompanyWithUKEstablishment" => Some(Overseas)
    case "nonUKCompanyNoUKEstablishment" => Some(Overseas)
    case "governmentOrganisation" => Some(GovernmentOrganisation)
    case _ => None
  }

  def frontendToBackendBusinessEntity(businessEntity: BusinessEntity): String = businessEntity match {
    case LimitedCompany => "limitedCompany"
    case SoleTrader => "soleTrader"
    case GeneralPartnership => "generalPartnership"
    case LimitedPartnership => "limitedPartnership"
    case LimitedLiabilityPartnership => "limitedLiabilityPartnershipKey"
    case ScottishLimitedPartnership => "scottishLimitedPartnershipKey"
    case VatGroup => "vatGroup"
    case Division => "administrativeDivision"
    case UnincorporatedAssociation => "unincorporatedAssociation"
    case Trust => "trust"
    case RegisteredSociety => "registeredSociety"
    case Charity => "charity"
    case Overseas => "nonUKCompanyWithUKEstablishment"
    case GovernmentOrganisation => "governmentOrganisation"
  }


  val jsonReadsFromBackend: Reads[BusinessEntity] = new Reads[BusinessEntity] {
    override def reads(json: JsValue): JsResult[BusinessEntity] =
      json.validate[String] map backendToFrontendBusinessEntity match {
        case JsSuccess(Some(businessEntity), _) => JsSuccess(businessEntity)
        case JsSuccess(None, _) => JsError("Is not a valid BusinessEntity")
        case JsError(errors) => JsError(errors)
      }
  }

  val jsonReads: Reads[BusinessEntity] = new Reads[BusinessEntity] {
    override def reads(json: JsValue): JsResult[BusinessEntity] =
      json.validate[String] map stringToOptBusinessEntity match {
        case JsSuccess(Some(businessEntity), _) => JsSuccess(businessEntity)
        case JsSuccess(None, _) => JsError("Is not a valid BusinessEntity")
        case JsError(errors) => JsError(errors)
      }
  }

  implicit object BusinessEntitySessionFormatter extends SessionFormatter[BusinessEntity] {
    override def fromString(string: String): Option[BusinessEntity] = stringToOptBusinessEntity(string)

    override def toString(entity: BusinessEntity): String = entity match {
      case LimitedCompany => LimitedCompanyKey
      case SoleTrader => SoleTraderKey
      case GeneralPartnership => GeneralPartnershipKey
      case LimitedPartnership => LimitedPartnershipKey
      case LimitedLiabilityPartnership => LimitedLiabilityPartnershipKey
      case ScottishLimitedPartnership => ScottishLimitedPartnershipKey
      case VatGroup => VatGroupKey
      case Division => DivisionKey
      case UnincorporatedAssociation => UnincorporatedAssociationKey
      case Trust => TrustKey
      case RegisteredSociety => RegisteredSocietyKey
      case Charity => CharityKey
      case Overseas => OverseasKey
      case GovernmentOrganisation => GovernmentOrganisationKey
      case Other => OtherKey
    }
  }

}
