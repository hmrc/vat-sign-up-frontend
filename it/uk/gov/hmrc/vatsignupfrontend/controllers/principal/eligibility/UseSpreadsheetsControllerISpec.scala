
package uk.gov.hmrc.vatsignupfrontend.controllers.principal.eligibility

import play.mvc.Http.Status._
import uk.gov.hmrc.vatsignupfrontend.controllers.principal.eligibility
import uk.gov.hmrc.vatsignupfrontend.helpers.{ComponentSpecBase, CustomMatchers}

class UseSpreadsheetsControllerISpec extends ComponentSpecBase with CustomMatchers {

  s"GET ${eligibility.routes.UseSpreadsheetsController.show().url}" should {
    s"return $OK" in {
      val res = get("/interruption/use-spreadsheets")

      res should have(
        httpStatus(OK)
      )
    }
  }
}
