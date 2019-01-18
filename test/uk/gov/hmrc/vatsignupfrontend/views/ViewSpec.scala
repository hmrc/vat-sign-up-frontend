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

package uk.gov.hmrc.vatsignupfrontend.views

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.twirl.api.Html
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.vatsignupfrontend.assets.MessageLookup.{Base => common}
import uk.gov.hmrc.vatsignupfrontend.config.AppConfig

trait ViewSpec extends UnitSpec with GuiceOneAppPerSuite {

  val testBackUrl = "/test-back-url"
  val testCall = Call("POST", "/test-url")
  val viewTestRequest = FakeRequest("POST", "/test-url")

  trait ElementTest {
    val name: String

    val element: Element

    private def selectHead(name: String, cssQuery: String): ElementTest = {
      lazy val n = s"""${this.name}."$name""""
      ElementTest(n, () => {
        val selector = element.select(cssQuery)
        if (selector.isEmpty) fail(s"Unable to locate $cssQuery in\n$element")
        selector.get(0)
      })
    }

    def shouldHavePara(paragraph: String): Unit =
      s"$name should have the paragraph (P) '$paragraph'" in {
        element.getElementsByTag("p").text() should include(paragraph)
      }

    def shouldHaveParaSeq(paragraphs: String*): Unit = {
      if (paragraphs.isEmpty) fail("Should provide at least 1 paragraph for this test")
      val ps = paragraphs.mkString(" ")
      s"$name should have the paragraphs (P) [${paragraphs.mkString("], [")}]" in {
        element.getElementsByTag("p").text() should include(ps)
      }
    }

    def shouldHaveBulletSeq(bullets: String*): Unit = {
      if (bullets.isEmpty) fail("Should provide at least 1 bullet point for this test")
      val bs = bullets.mkString(" ")
      s"$name should have the bulletPoints (LI) [${bullets.mkString("], [")}]" in {
        element.getElementsByTag("li").text() should include(bs)
      }
    }

    def shouldHaveH2(text: String): Unit =
      s"$name have a Heading 2 (H2) for '$text'" in {
        element.getElementsByTag("h2").text() should include(text)
      }

    def shouldHaveH3(text: String): Unit =
      s"$name have a Heading 3 (H3) for '$text'" in {
        element.getElementsByTag("h3").text() should include(text)
      }

    def shouldHaveHint(hint: String): Unit =
      s"$name should have the hint text '$hint'" in {
        element.getElementsByTag("span").hasClass("form-hint") shouldBe true
        element.getElementsByTag("span").text() should include(hint)
      }

    def shouldHaveALink(text: String, href: => String): Unit =
      s"$name have a link with text '$text' pointed to '$href'" in {
        val link = element.select("a")
        if (link == null) fail(s"Unable to locate any links in $name\n$element\n")
        if (link.size() > 1) fail(s"Multiple links located in $name, please specify an id")
        link.attr("href") shouldBe href
        link.text() shouldBe text
      }

    def shouldHaveALink(id: String, text: String, href: => String): Unit =
      s"$name have a link with text '$text' pointed to '$href'" in {
        val link = element.getElementById(id)
        if (link == null) fail(s"Unable to locate $id")
        if (!link.tagName().equals("a")) fail(s"The element with id=$id is not a link")
        link.attr("href") shouldBe href
        link.text() shouldBe text
      }

    def shouldHaveSignOutLink(isAgent: Boolean, text: String = common.signOut): Unit = {
      val signOutId = "sign-out"
      val appConfig = app.injector.instanceOf[AppConfig]
      val expectedHref =
        if (isAgent) appConfig.ggSignOutUrl(appConfig.agentFeedbackSurveyUrl)
        else appConfig.ggSignOutUrl(appConfig.principalFeedbackSurveyUrl)
      shouldHaveALink(signOutId, text, expectedHref)
    }

    def shouldHaveAccordion(heading: String, text: String): Unit = {
      s"$name should have an accordion with heading $heading and $text" in {
        val accordions = element.getElementsByTag("details")
        if (accordions == null) fail(s"Unable to find any details tag")
        if (accordions.size() > 1) fail(s"found multiple accordions, need to specify an id")
        val targetAccordion = accordions.get(0)

        val targetHeading = targetAccordion.getElementsByClass("summary").text()
        withClue(s"accordion heading '$targetHeading' does not match '$heading'\n") {
          targetHeading shouldBe heading
        }

        val targetText = targetAccordion.select(".panel.panel-border-narrow").text()
        withClue(s"accordion text '$targetText' does not match '$text'\n") {
          targetText shouldBe text
        }
      }
    }

    def shouldHaveSignOutButton(isAgent: Boolean, text: String = common.signOut): Unit = {
      val id = "sign-out-button"
      val appConfig = app.injector.instanceOf[AppConfig]
      val expectedHref =
        if (isAgent) appConfig.ggSignOutUrl(appConfig.agentFeedbackSurveyUrl)
        else appConfig.ggSignOutUrl(appConfig.principalFeedbackSurveyUrl)
      shouldHaveALink(id, text, expectedHref)
    }

    def shouldHaveTextField(name: String,
                            label: String,
                            hideLabel: Boolean = true,
                            maxLength: Option[Int] = None,
                            pattern: Option[String] = None,
                            inputMode: Option[String] = None
                           ): Unit = {

      s"${this.name} should have an input field '$name'" which {

        s"is a text field" in {
          import collection.JavaConversions._
          val eles = element.select(s"""input[name=$name]""")
          if (eles.isEmpty) fail(s"$name does not have an input field with name=$name\ncurrent list of inputs:\n[${element.select("input")}]")
          if (eles.size() > 1) fail(s"$name have multiple input fields with name=$name")
          val ele = eles.head
          ele.attr("type") shouldBe "text"
          maxLength.map {
            l => ele.attr("maxLength") shouldBe l.toString
          }
          pattern.map {
            p => ele.attr("pattern") shouldBe p
          }
          inputMode.map {
            m => ele.attr("inputMode") shouldBe m
          }
        }

        lazy val labelField = element.select(s"label[for=$name]")

        s"with the expected label '$label'" in {
          labelField.text() shouldBe label
        }

        if (hideLabel) {
          s"and the label should be visuallyhidden" in
            withClue(s"$name does not have the class 'visuallyhidden'\n") {
              labelField.hasClass("visuallyhidden") shouldBe true
            }
        }
      }
    }

    def shouldHaveDateField(id: String, legend: String, exampleDate: String): Unit = {
      val selector = s"#$id"
      s"${this.name} have a fieldset with id '$id' with the legend '$legend'" in {
        val ele = element.getElementById(id)
        ele.select("span.form-label-bold").text() shouldBe legend
        ele.select("span.form-hint").text() shouldBe exampleDate
        ele.tag().toString shouldBe "fieldset"
      }
      val date = selectHead(id, selector)
      val numericPattern = "[0-9]*"
      val inputMode = "numeric"
      date.shouldHaveTextField(s"$id.dateDay", common.day, hideLabel = false, maxLength = Some(2), pattern = Some(numericPattern), inputMode = Some(inputMode))
      date.shouldHaveTextField(s"$id.dateMonth", common.month, hideLabel = false, maxLength = Some(2), pattern = Some(numericPattern), inputMode = Some(inputMode))
      date.shouldHaveTextField(s"$id.dateYear", common.year, hideLabel = false, maxLength = Some(4), pattern = Some(numericPattern), inputMode = Some(inputMode))
    }

    object ElementTest {

      // n.b. element must be null-ary function to prevent evaluation at instantiation
      def apply(name: String, element: () => Element): ElementTest = {
        val n = name
        val ele = element
        if (ele == null) {
          throw new IllegalArgumentException("creation of name failed: element is null")
        }
        new ElementTest {
          override lazy val name: String = n
          override lazy val element: Element = ele()
        }

      }

    }

    def shouldHaveSubmitButton(text: String): Unit =
      s"$name should have the a submit button (Button) '$text'" in {
        import collection.JavaConversions._
        val submitButtons = element.getElementsByTag("input").filter(_.attr("type") == "submit")
        submitButtons.size shouldBe 1
        submitButtons.head.attr("class") should include("button")
        submitButtons.head.attr("value") shouldBe text
      }

    def shouldHaveContinueButton(): Unit = shouldHaveSubmitButton(common.continue)

    def shouldHaveConfirmAndContinueButton(): Unit = shouldHaveSubmitButton(common.confirmAndContinue)

    def shouldHaveConfirmButton(): Unit = shouldHaveSubmitButton(common.confirm)

    def shouldHaveAcceptAndContinueButton(): Unit = shouldHaveSubmitButton(common.acceptAndContinue)

    def shouldHaveAgreeAndContinueButton(): Unit = shouldHaveSubmitButton(common.agreeAndContinue)

    def shouldHaveAgreeButton(): Unit = shouldHaveSubmitButton(common.agree)

    def shouldHaveContinueToSignUpButton(): Unit = shouldHaveSubmitButton(common.continueToSignUp)

    def shoudHaveSignUpClientButton(): Unit = shouldHaveSubmitButton(common.signUpClient)

    def shouldHaveSignUpAnotherClientButton(): Unit = shouldHaveSubmitButton(common.signUpAnotherClient)

    def shouldHaveTryAgainButton(): Unit = shouldHaveSubmitButton(common.tryAgain)

    def shouldHaveContinueButtonLink(href: String, text: String): Unit =
      shouldHaveALink("continue-button", href = href, text = text)

    def shouldHaveForm(formName: String, id: Option[String] = None)(actionCall: => Call): Unit = {
      val selector =
        id match {
          case Some(i) => s"#$i"
          case _ => "form"
        }

      lazy val method = actionCall.method
      lazy val url = actionCall.url
      // this test is put in here because it doesn't make sense for it to be called on anything
      // other than a form
      s"$formName in $name must have a $method action to '$url'" in {
        val formSelector = element.select(selector)
        formSelector.attr("method") shouldBe method.toUpperCase
        formSelector.attr("action") shouldBe url
      }
    }
  }


  class TestView(override val name: String,
                 title: String,
                 heading: String,
                 page: => Html,
                 haveSignOutInBanner: Boolean) extends ElementTest {

    lazy val document: Document = Jsoup.parse(page.body)
    override lazy val element: Element = document.getElementById("content")

    s"$name should have the title '$title'" in {
      document.title() shouldBe title
    }

    s"$name should have the heading (H1) '$heading'" in {
      val h1 = document.getElementsByTag("H1")
      h1.size() shouldBe 1
      h1.text() shouldBe heading
    }

    s"$name should ${if (!haveSignOutInBanner) "not "}have a sign out link in the banner" in {
      val signOut = document.getElementById("logOutNavHref")
      if (haveSignOutInBanner)
        signOut should not be null
      else
        signOut shouldBe null
    }

  }

  object TestView {
    def apply(name: String,
              title: String,
              heading: String,
              page: => Html,
              haveSignOutInBanner: Boolean = true): TestView = new TestView(name, title, heading, page, haveSignOutInBanner)
  }

}
