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

package uk.gov.hmrc.vatsignupfrontend.connectors

import javax.inject.{Inject, Singleton}

import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.vatsignupfrontend.httpparsers.StoreCompanyNumberHttpParser._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StoreCompanyNumberConnector @Inject()(val http: HttpClient,
                                            val applicationConfig: AppConfig) {

  val companyNumberKey = "companyNumber"
  val companyUtrKey = "ctReference"

  def storeCompanyNumber(vatNumber: String, companyNumber: String, optCompanyUtr: Option[String])
                        (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[StoreCompanyNumberResponse] =
    http.PUT[JsObject, StoreCompanyNumberResponse](applicationConfig.storeCompanyNumberUrl(vatNumber),
      optCompanyUtr match {
        case Some(companyUtr) =>
          Json.obj(
            companyNumberKey -> companyNumber,
            companyUtrKey -> companyUtr
          )
        case None =>
          Json.obj(
            companyNumberKey -> companyNumber
          )
      }
    )
}
