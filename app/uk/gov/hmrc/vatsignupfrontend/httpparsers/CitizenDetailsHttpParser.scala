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

package uk.gov.hmrc.vatsignupfrontend.httpparsers


import play.api.http.Status.{INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import play.api.libs.json.{JsError, JsResult, JsSuccess}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.vatsignupfrontend.models.UserDetailsModel
import uk.gov.hmrc.vatsignupfrontend.models.UserDetailsModel.citizenDetailsReads

object CitizenDetailsHttpParser {

  type CitizenDetailsResponse = Either[CitizenDetailsRetrievalFailure, CitizenDetailsRetrievalSuccess]

  implicit object CitizenDetailsHttpReads extends HttpReads[CitizenDetailsResponse] {
    override def read(method: String, url: String, response: HttpResponse): CitizenDetailsResponse = {

      def parseCitizenDetails: JsResult[UserDetailsModel] = response.json.validate[UserDetailsModel](citizenDetailsReads)

      response.status match {
        case OK => parseCitizenDetails match {
          case JsSuccess(userDetails,  _) => Right(CitizenDetailsRetrievalSuccess(userDetails))
          case JsError(_) => Left(CitizenDetailsRetrievalFailureResponse(INTERNAL_SERVER_ERROR))
        }
        case NOT_FOUND => Left(NoCitizenRecord)
        case INTERNAL_SERVER_ERROR => Left(MoreThanOneCitizenMatched)
        case status => Left(CitizenDetailsRetrievalFailureResponse(status))
      }
    }
  }

  sealed trait CitizenDetailsRetrievalFailure

  case class CitizenDetailsRetrievalSuccess(userDetails: UserDetailsModel)

  case object NoCitizenRecord extends CitizenDetailsRetrievalFailure

  case object MoreThanOneCitizenMatched extends CitizenDetailsRetrievalFailure

  case class CitizenDetailsRetrievalFailureResponse(status: Int) extends CitizenDetailsRetrievalFailure

}
