/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.vatsignupfrontend.connectors.GetCompanyNameConnector
import uk.gov.hmrc.vatsignupfrontend.httpparsers.GetCompanyNameHttpParser.GetCompanyNameResponse
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GetCompanyNameService @Inject()(val getCompanyNameConnector: GetCompanyNameConnector) {

  private val crnMaxLength = 8
  private val CrnRegex = "([a-zA-Z]*)([0-9a-zA-Z]*)".r

  def getCompanyName(companyNumber: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[GetCompanyNameResponse] =
    getCompanyNameConnector.getCompanyName(padCrn(companyNumber))


  private def padCrn(companyNumber: String): String =
    companyNumber match {
      case CrnRegex(prefix, remainder) =>
        prefix + Seq.fill(crnMaxLength - companyNumber.length)('0').mkString + remainder
      case _ =>
        throw new IllegalArgumentException(s"[GetCompanyNameService] Company number '$companyNumber' was invalid")
    }

}
