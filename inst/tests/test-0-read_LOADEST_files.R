context("metatests-read_LOADEST_files")
# Make sure that read_LOADEST_files.R does what we want.

test_that("read_LOADEST_inputs can read each example without error", {
  
  LOADEST_example_inputs <- read_LOADEST_input_examples("../loadest_examples/app")
  expect_that(length(LOADEST_example_inputs) == 7, is_true())
  
  lapply(LOADEST_example_inputs, function(inputs) {
    expect_that(names(inputs), equals(c("header.inp", "calib.inp", "est.inp")))
    expect_that(is.list(inputs$header.inp), is_true())
    expect_that(length(inputs$header.inp) > 5, is_true())
    expect_that(is.data.frame(inputs$calib.inp), is_true())
    expect_that(nrow(inputs$calib.inp) > 15, is_true())
    expect_that(is.data.frame(inputs$est.inp), is_true())
    expect_that(nrow(inputs$est.inp) > 30, is_true())
  })
  
})

test_that("read_LOADEST_outputs can read each example without error", {
  
  LOADEST_example_outputs <- read_LOADEST_output_examples("../loadest_examples/app")
  expect_that(length(LOADEST_example_outputs) == 7, is_true())
  
  lapply(LOADEST_example_outputs, function(outputs) {
    expect_that(names(outputs), equals(c("echo.out", "out", "ind", "res")))
    
    expect_that(is.character(outputs$echo.out), is_true())
    expect_that(length(outputs$echo.out) == 1, is_true())
    
    expect_that(is.list(outputs$out), is_true())
    for(const in names(outputs$out)) {
      expect_that(is.list(outputs$out[[const]]), is_true())
      expect_that(length(outputs$out[[const]]) == 3, is_true())
    }
    expect_that(is.list(outputs$ind), is_true())
    for(const in names(outputs$ind)) {
      expect_that(is.data.frame(outputs$ind[[const]]), is_true())
      expect_that(nrow(outputs$ind[[const]]) > 30, is_true())
    }
    expect_that(is.list(outputs$res), is_true())
    for(const in names(outputs$res)) {
      expect_that(is.data.frame(outputs$res[[const]]), is_true())
      expect_that(nrow(outputs$res[[const]]) > 30, is_true())
    }
  })
  
})