Sys.setenv("R_TESTS" = "")
library(testthat)
library(PortalDataSummaries)

test_check("PortalDataSummaries")
