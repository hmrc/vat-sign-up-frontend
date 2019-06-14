[![Apache-2.0 license](http://img.shields.io/badge/license-Apache-brightgreen.svg)](http://www.apache.org/licenses/LICENSE-2.0.html)
[![Build Status](https://travis-ci.org/hmrc/vat-sign-up-frontend.svg)](https://travis-ci.org/hmrc/vat-sign-up-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/vat-sign-up-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/vat-sign-up-frontend/_latestVersion)

#### Use software to submit your VAT Returns (MTD VAT)
# VAT Sign Up Frontend

This is a Scala/Play frontend web UI that provides screens for VAT Individual users and VAT Agents acting on behalf of VAT Users to sign up to submit their VAT Returns using software. This is based upon:

  - VAT registered businesses with a turnover above £85,000 must use relevant third party software to submit their VAT Returns.

### Running the sign up vat.business.services locally

You will need [sbt](http://www.scala-sbt.org/)

1) **[Install Service-Manager](https://github.com/hmrc/service-manager/wiki/Install#install-service-manager)**


2) **Start the MTD VAT sign up services:**

   `sm --start VAT_SIGN_UP_ALL -f`


3) **Clone the frontend service:**

  - SSH

    `git clone git@github.com:hmrc/vat-sign-up-frontend.git`

  - HTTPS

    `git clone https://github.com/hmrc/vat-sign-up-frontend.git`


4) **Start the frontend service:**
   
   `sm --stop VAT_SIGN_UP_FRONTEND`

   `sbt "run 9566 -Dapplication.router=testOnlyDoNotUseInAppConf.Routes"`


5) **Go to the homepage:**

   http://localhost:9566/vat-through-software/sign-up/

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")

