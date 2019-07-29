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

package uk.gov.hmrc.vatsignupfrontend.services

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vatsignupfrontend.connectors.SubscriptionRequestSummaryConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.GetSubscriptionRequestSummaryResponse

import scala.concurrent.Future

@Singleton
class SubscriptionRequestSummaryService @Inject()(val subscriptionRequestSummaryConnector: SubscriptionRequestSummaryConnector) {

  def getSubscriptionSummaryRequest(vatNumber: String)(implicit hc: HeaderCarrier): Future[GetSubscriptionRequestSummaryResponse] = {
    subscriptionRequestSummaryConnector.getSubscriptionRequest(vatNumber)
  }
}