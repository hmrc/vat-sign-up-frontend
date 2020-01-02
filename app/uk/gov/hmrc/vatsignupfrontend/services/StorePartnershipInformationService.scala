/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.vatsignupfrontend.services

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.connectors.StorePartnershipInformationConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StorePartnershipInformationHttpParser.StorePartnershipInformationResponse
import uk.gov.hmrc.vatsignupfrontend.models.PartnershipEntityType.GeneralPartnership
import uk.gov.hmrc.vatsignupfrontend.models.{PartnershipEntityType, PostCode}

import scala.concurrent.Future

@Singleton
class StorePartnershipInformationService @Inject()(storePartnershipInformationConnector: StorePartnershipInformationConnector) {

  def storePartnershipInformation(vatNumber: String,
                                  sautr: Option[String],
                                  postCode: Option[PostCode]
                                 )(implicit hc: HeaderCarrier): Future[StorePartnershipInformationResponse] =
    storePartnershipInformationConnector.storePartnershipInformation(
      vatNumber = vatNumber,
      sautr = sautr,
      partnershipType = GeneralPartnership,
      companyNumber = None,
      postCode = postCode
    )

  def storePartnershipInformation(vatNumber: String,
                                  sautr: Option[String],
                                  companyNumber: String,
                                  partnershipEntity: PartnershipEntityType,
                                  postCode: Option[PostCode])(implicit hc: HeaderCarrier): Future[StorePartnershipInformationResponse] =
    storePartnershipInformationConnector.storePartnershipInformation(
      vatNumber = vatNumber,
      sautr = sautr,
      partnershipType = partnershipEntity,
      companyNumber = Some(companyNumber),
      postCode = postCode
    )
}
