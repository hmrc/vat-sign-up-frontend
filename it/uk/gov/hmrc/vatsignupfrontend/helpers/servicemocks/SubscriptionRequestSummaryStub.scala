
package uk.gov.hmrc.vatsignupfrontend.helpers.servicemocks

import com.github.tomakehurst.wiremock.stubbing.StubMapping
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.vatsignupfrontend.httpparsers.SubscriptionRequestSummaryHttpParser.SubscriptionRequestUnexpectedError
import uk.gov.hmrc.vatsignupfrontend.models.{ContactPreference, SubscriptionRequestSummary}
import uk.gov.hmrc.vatsignupfrontend.models.BusinessEntity.frontendToBackendBusinessEntity

object SubscriptionRequestSummaryStub extends WireMockMethods {

  def subscriptionRequestSummaryToJson(subModel: SubscriptionRequestSummary): JsObject = {
    Json.obj(
      "vatNumber" -> subModel.vatNumber,
      "businessEntity" -> Json.obj(
        "entityType" -> frontendToBackendBusinessEntity(subModel.businessEntity)
      ),
      "transactionEmail" -> subModel.transactionEmail,
      "contactPreference" -> ContactPreference.contactPreferenceFormat.toString(subModel.contactPreference)
    ) ++ subModel.optSignUpEmail.fold(Json.obj())(email => Json.obj("optSignUpEmail" -> email))
  }
  def stubGetSubscriptionRequest(vatNumber: String)(status: Int, response: Option[SubscriptionRequestSummary]): StubMapping =
    when(method = GET, uri = s"/vat-sign-up/subscription-request/vat-number/$vatNumber")
      .thenReturn(status = status, body = response.map(subscriptionRequestSummaryToJson(_)).getOrElse(Json.obj()))

  def stubGetSubscriptionRequestInvalidJson(vatNumber: String)(status: Int): StubMapping =
    when(method = GET, uri = s"/vat-sign-up/subscription-request/vat-number/$vatNumber")
      .thenReturn(status = status, body = Json.obj("foo" -> "bar"))

  def stubGetSubscriptionRequestException(vatNumber: String)(responseStatus: Int): StubMapping =
    when(method = GET, uri = s"/vat-sign-up/subscription-request/vat-number/$vatNumber")
    .thenReturn(status = responseStatus)
}
