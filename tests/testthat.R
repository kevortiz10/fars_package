library(testthat)
library(FarsKevin)

test_check("FarsKevin")

testthat::test_that("Make filename test",
        expect_that(make_filename("A"),
                    gives_warning("NAs introducidos por coerción"))
)
