
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/19hourz/fireData/master/LICENSE.txt)
[![Build Status](https://travis-ci.org/19hourz/fireData.svg?branch=master)](https://travis-ci.org/19hourz/fireData)
[![codecov](https://codecov.io/gh/19hourz/fireData/branch/master/graph/badge.svg)](https://codecov.io/gh/19hourz/fireData)

# Firedata - Enabling easy cloud stats through Firestore

======

This project is part of Firedata that adds support for Google Cloud Firestore. Methods are implemented through Google Firestore REST API. Before using, please set up Firebase accordingly. Certain functions requires using OAuth 2.0 access token. To gain the access token, credentials of the project will be needed and can be found from [here](https://console.developers.google.com/apis/credentials). Firestore brought additional features upon realtime database such as querying.

======

## Setup

**Firestore Setup:**
- visit at https://firebase.google.com/
- login and enter the "console"
- add new project
- find the API keys and projectID at the "overview" page clicking on add firebase to web-app
- certain functions requires OAuth 2.0 access which needed client_id and client_secret, these can be found from https://console.developers.google.com/apis/credentials

**R Package Setup:**

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("19hourz/fireData")

library(fireData)
```

======

## Examples
