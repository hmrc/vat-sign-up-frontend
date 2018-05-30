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

package uk.gov.hmrc.vatsignupfrontend.connectors

import javax.inject.Inject

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CitizenDetailsHttpParser.CitizenDetailsResponse
import uk.gov.hmrc.vatsignupfrontend.httpparsers.CitizenDetailsHttpParser.CitizenDetailsHttpReads

import scala.concurrent.Future


class CitizenDetailsConnector @Inject()(val http: HttpClient,
                                        val applicationConfig: AppConfig) {

  def getCitizenDetails(sautr: String)(implicit hc: HeaderCarrier): Future[CitizenDetailsResponse] =
    http.GET[CitizenDetailsResponse](applicationConfig.getCitizenDetailsUrl(sautr))

}
