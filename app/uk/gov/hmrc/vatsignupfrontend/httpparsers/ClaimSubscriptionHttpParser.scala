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

package uk.gov.hmrc.vatsignupfrontend.httpparsers

import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

object ClaimSubscriptionHttpParser {
  type ClaimSubscriptionResponse = Either[ClaimSubscriptionFailure, SubscriptionClaimed.type]

  implicit object ClaimSubscriptionHttpReads extends HttpReads[ClaimSubscriptionResponse] {
    override def read(method: String, url: String, response: HttpResponse): ClaimSubscriptionResponse = {
      response.status match {
        case NO_CONTENT => Right(SubscriptionClaimed)
        case FORBIDDEN => Left(KnownFactsMismatch)
        case BAD_REQUEST => Left(InvalidVatNumber(response.body))
        case CONFLICT => Left(AlreadyEnrolledOnDifferentCredential)
        case status => Left(ClaimSubscriptionFailureResponse(status, response.body))
      }
    }
  }

  case object SubscriptionClaimed

  sealed trait ClaimSubscriptionFailure

  case object KnownFactsMismatch extends ClaimSubscriptionFailure

  case class InvalidVatNumber(message: String) extends ClaimSubscriptionFailure

  case object AlreadyEnrolledOnDifferentCredential extends ClaimSubscriptionFailure

  case class ClaimSubscriptionFailureResponse(status: Int, message: String) extends ClaimSubscriptionFailure
}


