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

package uk.gov.hmrc.vatsignupfrontend.assets

object MessageLookup {

  object Base {
    val continue = "Continue"
    val continueToSignUp = "Continue to sign up"
    val confirmAndContinue = "Confirm and continue"
    val confirm = "Confirm"
    val acceptAndContinue = "Accept and continue"
    val agreeAndContinue = "Agree and continue"
    val agree = "Agree"
    val submit = "Submit"
    val update = "Update"
    val signOut = "Sign out"
    val signUp = "Sign up"
    val goBack = "Go back"
    val signUpAnotherClient = "Sign up another client"
    val day = "Day"
    val month = "Month"
    val year = "Year"
    val change = "Change"
    val tryAgain = "Try again"
    val startNow = "Start now"
    val yes = "Yes"
    val no = "No"
  }

  object ErrorMessage {
    val invalidVatNumber = "Please enter a valid vat registration number"
    val invalidCompanyNumber = "Please enter a valid company number"
  }

  object NotEnrolledToAS {
    val title = "You can't use this service yet"
    val heading: String = title
    val line1 = "To use this service, you need to set up an agent services account."
  }

  object NoAgentClientRelationship {
    val title = "You can't sign up this client yet"
    val heading: String = title
    val line1 = "To use this service, your client needs to authorise you as their agent."
  }

  object CannotUseServiceYet {
    val title = "Your client can't use this service yet"
    val heading: String = title
    val line1 = "This service is currently only available to certain sole traders and limited companies."
    val line2 = "Your client must come back and sign up in April 2019, when this service is available to them."
  }

  object CaptureVatNumber {
    val title = "What is your client's VAT number?"
    val heading: String = title
    val description = "This is the 9-digit number they received when they registered for VAT."
    val hint = "For example, 123456789"
  }

  object ConfirmVatNumber {
    val title = "Confirm your client's VAT number"
    val heading: String = title
    val link = "Change VAT number"
  }

  object CaptureCompanyNumber {
    val title = "What is your client's company number?"
    val heading: String = title
  }

  object PrincipalCompanyNameNotFound {
    val title = "We could not confirm your company"
    val heading: String = title
    val line1 = "The company number you entered is not on our system."
  }

  object PrincipalSignInDifferentDetails {
    val title = "You need to sign in with different details"
    val heading: String = title
    val line1 = "The details you have used to sign in are for a different sole trader. You need to sign out, then sign in with the correct Government Gateway details."
    val linkText = "change your answer"
    val line2 = s"You can $linkText."
  }

  object PrincipalIncorrectEnrolmentVatNumber {
    val title = "You need to sign in with different details"
    val heading: String = title
    val line1 = "The VAT number you entered is for a different business that has its own Government Gateway account. Once you've signed out, you need to sign in with the correct details."
    val linkText = "change your answer"
    val line2 = s"If you've entered the wrong VAT number, you can $linkText."
  }

  object CaptureBusinessEntity {
    val title = "What type of business is your client registered as?"
    val heading: String = title
    val radioSoleTrader: String = "Sole trader"
    val radioLimitedCompany: String = "Limited company"
    val radioOther: String = "Other"
  }

  object YesNo {
    val title = "Do you have more than one VAT registered business?"
    val heading: String = title
    val radioYes: String = "yes"
    val radioNo: String = "no"
  }

  object ClientDetails {
    val title = "Enter your client's details"
    val heading = "Enter your client's details"
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, 'QQ 12 34 56 C'."
    val formhint2 = "For example, 10 12 1990"
  }

  object ConfirmCompanyNumber {
    val title = "Confirm your client's company number"
    val heading: String = title
    val companyNumberHeading = "Company number"
    val link = "Change company number"
  }

  object AgreeCaptureEmail {
    val title = "Agree for your client to get secure messages from HMRC"
    val heading: String = title
    val insetText = "To join this trial, your client must agree to get secure messages instead of letters."
    val line1 = "When your client has a new message about VAT in their business tax account, we will send an email notification to let them know."
    val line2 = "Your client will need to sign in to their business tax account to read their secure messages."
  }

  object CaptureAgentEmail {
    val title = "What is your email address?"
    val heading: String = title
    val line1 = "We will only send you an email to let you know if your client can join the trial."
    val hint = "For example, me@me.com"
  }

  object CaptureClientEmail {
    val title = "What is your client's email address?"
    val heading: String = title
    val hint = "For example, me@me.com"
  }

  object ConfirmEmail {
    val title = "Check your client's email address"
    val heading: String = title
    val emailHeading = "Email address"
    val link = "Change email address"
  }

  object ConfirmAgentEmail {
    val title = "Confirm your email address"
    val heading: String = title
    val link = "Change email address"
  }

  object SentClientEmail {
    val title = "We have sent your client an email"
    val heading: String = title

    def line1(email: String) = s"We have sent an email to $email. Your client needs to click on the link in the email to verify their email address."

    val line2 = "They must verify their email address to get secure message notifications and other VAT emails from HMRC."
  }

  object VerifyAgentEmail {
    val title = "Verify your email address"
    val heading: String = title

    def line1(email: String) = s"We have sent an email to $email. Click on the link in the email to verify your email address."

    val linkText1 = "change your email address"
    val line2 = s"You can $linkText1 if it is not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your junk folder. If it is not there we can $linkText2. If we send your email again, any previous links will stop working."
  }

  object AgentEmailVerified {
    val title = "You have verified your email address"
    val heading: String = title
    val line1 = "You need to continue to sign up your client. We will send you a confirmation message after you sign up this client."
  }

  object UseDifferentEmailAddress {
    val title = "You need to enter a different email address"
    val heading: String = title
    val line1 = "The email address you entered for your client is the same as your own email address."
  }

  object AgentConfirmCompany {
    val title = "Confirm your client's company"
    val heading: String = title
    val link = "Change company"
  }

  object AgentCompanyNameNotFound {
    val title = "We could not confirm your client's company"
    val heading: String = title
    val line1 = "The company number you entered is not on our system."
  }

  object ClientVerifiedEmail {
    val title = "You've verified your email address"
    val heading: String = title
    val line1 = "HM Revenue and Customs will contact you by email instead of sending letters."
    val link = "Return to GOV.UK"
  }

  object ConfirmClient {
    val title = "Confirm your client's details"
    val heading = "Confirm your client's details"
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object FailedClientMatching {
    val title = "There's a problem"
    val heading: String = title
    val description = "The details you've entered are not on our system."
  }

  object ClientAlreadySignedUp {
    val title = "Your client has already signed up"
    val heading: String = title
    val line1 = "This client's details are already in use."
  }

  object Terms {
    val title = "Terms of participation"
    val heading: String = title
    val line1 = "By taking part in this trial, you agree that either you or your client will:"
    val bullet1 = "use relevant software to record your client's sales and purchases, then to submit their VAT Returns"
    val bullet2 = "submit each VAT Return within one calendar month and 7 days from the end of their accounting period"
    val bullet3 = "tell HMRC if your client stops trading and then submit their final VAT Return"
    val bullet4 = "tell HMRC if your client wants to leave this trial"
    val line2 = "These terms are not contractual and your client can stop taking part in this trial at any time."
  }

  object AgentInformationReceived {
    val title = "We have received your client's information"
    val heading: String = title

    object Section1 {
      val heading = "What happens next"
      val line1 = "As this service is currently a trial, it is only available to some limited companies, sole traders and partnerships."
      val line2 = "You must tell your client if they can take part in the trial."
      val bullet1 = "We will let you know whether your client can take part in this trial, usually within 24 hours."
      val bullet2 = "Choose relevant third party software. If your client will use software, it is important they choose a package that can interact with yours."
      val bullet3 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val bullet4 = "Use software to record your client's sales and purchases."
      val bullet5 = "Submit your client's VAT Returns before their deadlines."
      val line3 = "Your client can view their VAT Return deadlines in their accounting software or business tax account."
    }

  }

  object YourVatNumber {
    val title = "Confirm you want to use this VAT number to sign up"
    val heading: String = title
    val vatNumberHeading = "VAT number"
    val link = "I want to use a different VAT number"
  }

  object VatNumber {
    val title = "What is your VAT number?"
    val heading: String = title
    val line1 = "This is the 9-digit number on your VAT registration certificate."
  }

  object CouldNotConfirmVatNumber {
    val title = "We could not confirm your client's VAT number"
    val heading: String = title
    val line1 = "The information you provided does not match the details on our system."
    val tryAgain = "Try again"
  }

  object VatRegistrationDate {
    val title = "When did you register for VAT?"
    val heading = "When did you become VAT registered?"
    val line1 = "You can find this date on your VAT registration certificate."
    val formhint1 = "For example, 6 4 2017"
  }

  object PrincipalCouldNotConfirmVatNumber {
    val title = "We could not confirm your VAT number"
    val heading: String = title
    val line1 = "The VAT number you entered is not on our system."
  }

  object SignInWithDifferentDetails {
    val title = "You need to sign in with different details"
    val heading: String = title
    val line1 = "You need to sign in with the Government Gateway details for the business you want to sign up."
  }

  object PrincipalPlaceOfBusiness {
    val title = "Where is the principal place of business?"
    val heading = title
    val line1 = "This is where the company carries out most of its business activities."
    val line2 = "If the company does business in different places or contract work on client premises, it is where the company keeps its financial and business records."
    val label = "UK postcode"
  }

  object PrincipalGuidance {
    val title = "Use software to submit your VAT Returns"
    val heading: String = title
    val line1 = "HM Revenue and Customs (HMRC) are changing the way you submit your VAT Returns."
    val line2 = "From April 2019, VAT registered businesses with a turnover above £85,000 must use relevant third party software to submit their VAT Returns."
    val line3 = "If you're a sole trader or a director of a limited company you can try this new way of reporting now, instead of waiting until April 2019."
    val line4 = "You'll need to sign up for this new service, even if you already use software to submit your VAT Returns."

    object Section1 {
      val heading = "How it works"
      val line1 = "To sign up for this service, you need to use software to record your purchases and sales. If you don't already record in this way, submit your next VAT Return on GOV.UK or by post before you sign up. This will give you more time to complete your first return using software."
      val number1 = "You need to choose relevant third party software. If you already use software, check with your supplier to see if you can use it with this service."
      val number2 = "Sign up for this service and agree to get emails instead of letters."
      val number3 = "Allow your software to submit VAT Returns to HMRC. You might need to sign in with your Government Gateway details."
      val number4 = "Use your software to record your purchases and sales, then to submit your VAT Returns. We'll email you to let you know when to send a return."
      val line2 = "You can also choose to:"
      val bullet1 = "pay throughout the year, if it helps you manage your income"
      val bullet2 = "get your accountant to send your VAT Returns"
      val bullet3 = "view your VAT Return deadlines in your business tax account"
    }

    object Section2 {
      val heading = "Sign up"
      val line1 = "To sign up you'll need the Government Gateway details you got when you registered for VAT."
      val line2 = "If you haven't submitted a VAT Return online before, then you'll also need your VAT registration certificate."
    }

  }

  object PrincipalCaptureBusinessEntity {
    val title = "What type of business are you registered as?"
    val heading: String = title
    val radioSoleTrader: String = "Sole trader"
    val radioLimitedCompany: String = "Limited company"
    val radioGeneralPartnership: String = "General partnership"
    val radioOther: String = "Other"
  }

  object PrincipalCheckYourAnswers {
    val title = "Check your answers"
    val heading = "Check your answers"
    val subHeading = "You've told us"
    val yourVatNumber = "What is your VAT number?"
    val vatRegistrationDate = "What is your VAT registration date?"
    val businessPostCode = "What is your business postcode?"
    val businessEntity = "What type of business are you registered as?"
  }

  object PrincipalNoCtEnrolmentSummary {
    val title = "Check your answers"
    val heading = "Check your answers"
    val subHeading = "You've told us"
    val companyNumber = "What is your company number?"
    val companyUtr = "What is your company Unique Taxpayer Reference?"
    val businessEntity = "What is your business type?"
  }

  object PrincipalCannotUseServiceYet {
    val title = "You can't use this service yet"
    val heading: String = title
    val line1 = "This service is only available to some limited companies and sole traders."
    val line2 = "You'll be able to sign up for this service by April 2019."
  }

  object PrincipalCaptureEmail {
    val title = "What is your email address?"
    val heading: String = title
    val hint = "For example, me@me.com"
  }

  object PrincipalAgreeCaptureEmail {
    val title = "Agree to get emails instead of letters"
    val heading: String = title
    val line1 = "When you have a new message about VAT in your HMRC account, we'll send you an email to let you know."
    val line2 = "You'll need to sign in to your account to read the message."
  }

  object PrincipalConfirmEmail {
    val title = "Check your email address"
    val heading: String = title
    val emailHeading = "Email address"
    val link = "Change email address"
  }

  object PrincipalVerifyEmail {
    val title = "Verify your email address"
    val heading: String = title

    def line1(email: String) = s"We've sent an email to $email. Click on the link in the email to verify your email address."

    val linkText1 = "change your email address"
    val line2 = s"You can $linkText1 if it is not correct."
    val accordionHeading = "I did not get an email"
    val linkText2 = "send it again"
    val accordionText = s"Check your junk folder. If it's not there we can $linkText2. If we send your email again, any previous links will stop working."
  }

  object PrincipalEmailVerified {
    val title = "You've verified your email address"
    val heading: String = title
    val line1 = "You'll now receive messages and email notifications from HMRC."
  }

  object YourDetails {
    val title = "Enter your details"
    val heading: String = title
    val line1 = "We will attempt to match these details against information we currently hold."
    val field1 = "First name"
    val field2 = "Last name"
    val field3 = "National Insurance number"
    val field4 = "Date of birth"
    val formhint1_line1 = "For example, 'QQ 12 34 56 C'."
    val formhint2 = "For example, 10 12 1990"
  }

  object ConfirmDetails {
    val title = "Confirm your details"
    val heading = "Confirm your details"
    val subHeading = "You've told us"
    val firstName = "First name"
    val lastName = "Last name"
    val nino = "National Insurance number"
    val dob = "Date of birth"
  }

  object PrincipalConfirmYourDetails {
    val title = "Confirm your details"
    val heading: String = title
    val firstName = "First name"
    val lastName = "Last name"
    val dob = "Date of birth"
    val nino = "National Insurance number"
    val link = "Change details"
  }

  object FailedIdentityVerification {
    val title = "We couldn't confirm your details"
    val heading: String = title
    val line1 = "The information you provided doesn't match the details on our system."
    val tryAgain = "Try again"
  }

  object CouldNotConfirmBusiness {
    val title = "We could not confirm your business"
    val heading: String = title
    val line1 = "The information you provided does not match the details we have about your business."
    val tryAgain = "Try again"
  }

  object BTACannotConfirmBusiness {
    val title = "We cannot confirm the business"
    val heading: String = title
    val line1 = "The information you provided does not match the details we have about the business."
    val tryAgain = "Try again"
  }

  object IdentityVerificationSuccess {
    val title = "We've confirmed your identity"
    val heading: String = title
    val line1 = "You can now sign up for this service."
  }

  object FailedMatching {
    val title = "We couldn't confirm your details"
    val heading: String = title
    val line1 = "The information you provided doesn't match the details on our system."
    val tryAgain = "Try again"
  }

  object PrincipalCaptureCompanyNumber {
    val title = "What is your company number?"
    val heading: String = title
    val line1 = "You received this from Companies House when you set up your company. It's 8 digits and sometimes starts with 2 letters."
  }

  object PrincipalConfirmCompanyNumber {
    val title = "Confirm your company number"
    val heading: String = title
    val companyNumberHeading = "Company number"
    val link = "Change company number"
  }

  object PrincipalConfirmCompany {
    val title = "Confirm your company"
    val heading: String = title
    val link = "Change company"
  }

  object PrincipalCaptureCompanyUtr {
    val title = "What is your company's Unique Taxpayer Reference number?"
    val heading: String = title
    val line1 = "You can find it on letters about Corporation Tax from HMRC. It is 10-digits and is sometimes called a UTR."
  }

  object PrincipalTerms {
    val title = "Terms of participation"
    val heading: String = title
    val line1 = "By taking part in this trial, you agree to:"
    val bullet1 = "use relevant software to record your sales and purchases, then to submit your VAT Returns"
    val bullet2 = "submit each VAT Return within one calendar month and 7 days from the end of your accounting period"
    val bullet3 = "authorise any third party you use (such as your accountant) and have responsibility for any information they give to HMRC on your behalf"
    val bullet4 = "tell HMRC if you stop trading and then submit your final VAT Return"
    val bullet5 = "tell HMRC if you want to leave this trial"
    val line2 = "These terms are not contractual and you can stop taking part in the trial at any time."
  }

  object PrincipalInformationReceived {
    val title = "We've received your information"
    val heading: String = title

    object Section1 {
      val heading = "What happens next"
      val line1 = "We'll let you know whether you can use software to submit your VAT Returns, usually within 24 hours."
    }

    object Section2 {
      val heading = "After your application is approved"
      val bullet1 = "Choose accounting software that supports this service if you haven't already."
      val bullet2 = "Sign in to the software with your Government Gateway details and authorise it to interact with HMRC."
      val bullet3 = "Add any sales and purchases that you've already received or paid out."
      val bullet4 = "Record your future sales and purchases using the software."
      val bullet5 = "Submit your VAT Returns before your deadlines."
      val line1 = "You can view your VAT Return deadlines in your accounting software or business tax account."
    }

  }

  object PrincipalAlreadySignedUp {
    val title = "You've already signed up"
    val heading: String = title
    val line1 = "Your sign in details are already in use."
  }

  object AgentGuidance {
    val title = "Use software to submit your client's VAT Returns"
    val heading = "Use software to submit your client's VAT Returns"
    val line1 = "From April 2019, sole trader businesses and limited companies with a turnover above £85,000 must submit their VAT Returns using software that supports Making Tax Digital."
    val line2 = "If your client is VAT registered they can choose to sign up for this new reporting method now. To help us test this new way of working out your tax, they'll need to:"
    val bullet1 = "use software that supports Making Tax Digital to record their purchases and sales"
    val bullet2 = "submit their VAT Returns using their software"
    val bullet3 = "agree to get emails instead of letters"

    val subHeadingBeforeYouStart = "Before you start"
    val beforeYouStartLine1 = "To sign your clients up to this service you'll need to:"
    val beforeYouStartBullet1 = "set up an agent services account"
    val beforeYouStartBullet2 = "add clients to your account"

    val subHeadingSignUpYourClients = "Sign up your clients"
    val signUpYourClientsLine1 = "Once you've set up your agent services account, you can sign up your clients for this new way of reporting their tax. This will add clients to your account."
    val signUpYourClientsLine2 = "You can sign up your clients if they're either a sole trader or limited company. Your clients can submit their VAT Returns using software, or you can submit them on their behalf."

    val subHeadingSoftware = "Software"
    val softwareLine1 = "You need to choose software that supports Making Tax Digital. You can still use your existing software if it supports Making Tax Digital."
    val softwareLine2 = "If you and your client will use software, it's important they choose a software package that can interact with yours."

    val subHeadingGetHelp = "Get help"
    val getHelpLine1 = "You need to contact your software supplier if you need help using your software, for example to upload sales records or sending updates to HMRC."
    val getHelpLine2 = "If you have other questions about sending updates, contact HMRC."

  }

  object AssistantCredentialError {
    val title = "You cannot use this service"
    val heading = title
    val line1 = "You can only sign up this business if you are an administrator."
  }

  object SignUpCompleteClient {
    val title = "You have added VAT to your business tax account"
    val heading = "You have added VAT to your business tax account"
    val line1 = "You can now use your business tax account to:"
    val bullet1 = "see what you owe"
    val bullet2 = "check your deadlines"
    val bullet3 = "pay your VAT"
    val bullet4 = "tell us about any changes to your business"
  }

  object ConfirmGeneralPartnershipUtr {
    val title = "Is this the right Unique Taxpayer Reference for the partnership?"
    val heading = "Is this the right Unique Taxpayer Reference (UTR) for the partnership?"
    val line1 = "We hold the following information:"
    val line2 = "Unique Taxpayer Reference (UTR)"
  }

  object PrincipalCapturePartnershipCompanyNumber {
    val title = "What is your company number?"
    val heading: String = title
    val linkText = "search for your company number (opens in a window or tab)"
    val line1 = s"You can $linkText on Companies House."
  }

  object SignUpAfterThisDate {
    val title = "You must come back later to sign up"
    val heading = "You must come back later to sign up"
    val line1 = "There is not enough time for us to set you up with Making Tax Digital before your next direct debit payment is due."
    def line2(date: String) = s"Please come back and sign up after $date."
  }

  object SignUpBetweenTheseDates {
    val title = "You must come back later to sign up"
    val heading = "You must come back later to sign up"
    val line1 = "There is not enough time for us to set you up with Making Tax Digital before your next direct debit payment is due."
    def line2(startDate: String, endDate: String) = s"Please come back and sign up between $startDate and $endDate."
  }

}
