context("metatests-run_LOADEST")

test.inoutputs.path="../loadest_examples/app"

test_that("run_LOADEST can run once", {
  expect_that(
    run_LOADEST(
      inputs.path=paste0(test.inoutputs.path,"1"), 
      loadest.exe.path="C:/Users/aa20/Dropbox/Software/loadest/loadest.exe"),
    takes_less_than(1))
})

# Takes a long time; test infrequently.
# test_that("run_LOADEST can run every example", {
#   expect_that(
#     run_LOADEST_examples(
#       inputs.path=test.inoutputs.path, 
#       loadest.exe.path="C:/Users/aa20/Dropbox/Software/loadest/loadest.exe"),
#     takes_less_than(1500))
# })

test_that("run_rloadest_from_LOADEST can run once", {
  expect_that(
    run_rloadest_from_LOADEST(example.num=1, use.Date=FALSE, inoutputs.path=test.inoutputs.path),
    takes_less_than(1))
})

test_that("run_rloadest_from_LOADEST can run every example", {
  expect_that(
    {
      rloadest_example_results <- run_rloadest_examples(test.inoutputs.path)
      expect_that(length(rloadest_example_results) == 7, is_true())
      for(i in 1:length(rloadest_example_results)) {
        expect_that(is.list(rloadest_example_results[[i]]$outs$loadRegs), is_true())
        for(j in 1:length(rloadest_example_results[[i]]$outs$loadRegs)) {
          expect_that(class(rloadest_example_results[[i]]$outs$loadRegs[[j]]) == "loadReg", is_true())
          expect_that(!is.na(rloadest_example_results[[i]]$outs$loadRegs[[j]]$load.units), is_true())
          expect_that(!is.na(rloadest_example_results[[i]]$outs$loadRegs[[j]]$conc.units), is_true())
          #expect_that(!is.na(rloadest_example_results[[i]]$outs$loadRegs[[j]]$flow.units), is_true())
        }
      }
    },
    takes_less_than(20))
})