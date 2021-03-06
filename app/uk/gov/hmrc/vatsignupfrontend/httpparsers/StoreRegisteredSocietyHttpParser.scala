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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

object StoreRegisteredSocietyHttpParser {

  val CodeKey = "CODE"
  val ctReferenceMismatch = "CtReferenceMismatch"

  type StoreRegisteredSocietyResponse = Either[StoreRegisteredSocietyFailure, StoreRegisteredSocietySuccess.type]

  implicit object StoreRegisteredSocietyHttpReads extends HttpReads[StoreRegisteredSocietyResponse] {
    override def read(method: String, url: String, response: HttpResponse): StoreRegisteredSocietyResponse = {

      def responseCode: Option[String] = (response.json \ CodeKey).asOpt[String]

      response.status match {
        case NO_CONTENT => Right(StoreRegisteredSocietySuccess)
        case BAD_REQUEST if responseCode contains ctReferenceMismatch => Left(CtReferenceMismatch)
        case status => Left(StoreRegisteredSocietyFailureResponse(status))
      }
    }
  }

  sealed trait StoreRegisteredSocietyFailure {
    def status: Int
  }

  case object StoreRegisteredSocietySuccess

  case object CtReferenceMismatch extends StoreRegisteredSocietyFailure {
    override def status = BAD_REQUEST
  }

  case class StoreRegisteredSocietyFailureResponse(status: Int) extends StoreRegisteredSocietyFailure

}
