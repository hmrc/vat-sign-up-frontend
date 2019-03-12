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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

object StoreContactPreferenceHttpParser {

  type StoreContactPreferenceResponse = Either[StoreContactPreferenceFailure, StoreContactPreferenceSuccess.type]

  implicit object StoreContactPreferenceHttpReads extends HttpReads[StoreContactPreferenceResponse] {
    override def read(method: String, url: String, response: HttpResponse): StoreContactPreferenceResponse = {
      response.status match {
        case NO_CONTENT => Right(StoreContactPreferenceSuccess)
        case status => Left(StoreContactPreferenceFailure(status))
      }
    }
  }

  case object StoreContactPreferenceSuccess

  case class StoreContactPreferenceFailure(status: Int)

}

