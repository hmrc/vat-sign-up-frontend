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

package uk.gov.hmrc.vatsignupfrontend.models

import play.api.libs.json.{JsResult, JsValue, Reads}

case class SubscriptionRequestSummary(
                                       vatNumber: String,
                                       businessEntity: BusinessEntity,
                                       optNino: Option[String],
                                       optCompanyNumber: Option[String],
                                       optSautr: Option[String],
                                       optSignUpEmail: Option[String],
                                       transactionEmail: String,
                                       contactPreference: ContactPreference
                                     )

object SubscriptionRequestSummary {
  implicit val jsonReads: Reads[SubscriptionRequestSummary] = new Reads[SubscriptionRequestSummary] {
    override def reads(json: JsValue): JsResult[SubscriptionRequestSummary] = {
      for {
        vatNumber         <- (json \ "vatNumber").validate[String]
        businessEntity    <- (json \ "businessEntity" \ "entityType").validate[BusinessEntity] (BusinessEntity.jsonReadsFromBackend)
        optNino           <- (json \ "businessEntity" \ "nino").validateOpt[String]
        optCompanyNumber  <- (json \ "businessEntity" \ "companyNumber").validateOpt[String]
        optSautr          <- (json \ "businessEntity" \ "sautr").validateOpt[String]
        optSignUpEmail    <- (json \ "optSignUpEmail").validateOpt[String]
        transactionEmail  <- (json \ "transactionEmail").validate[String]
        contactPreference <- (json \ "contactPreference").validate[ContactPreference] (ContactPreference.jsonReads)
      } yield {
        SubscriptionRequestSummary(vatNumber, businessEntity, optNino, optCompanyNumber, optSautr, optSignUpEmail, transactionEmail, contactPreference)
      }
    }
  }
}
