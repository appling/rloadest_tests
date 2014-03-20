
test.inoutputs.path="../loadest_examples/app" # for calls from test()
#test.inoutputs.path="inst/loadest_examples/app" # for running these lines manually

for(withfix in c(FALSE, TRUE)) {
  
  if(withfix) {
    # To see if the patch worked, go into dev_mode and reload rloadest as the version in pangia/rloadest/pred-newdata-checks
    dev_mode(on=TRUE)
    unload(inst("rloadest"))
    install_github(repo="pangia/rloadest", ref="flow-units")
  }
  
  test_that(paste(if(withfix) "with" else "without","fix, loadReg objects include a flow.units element"), {

    rloadest_example_results <- run_rloadest_examples(test.inoutputs.path)
    for(i in 1:length(rloadest_example_results)) {
      for(j in 1:length(rloadest_example_results[[i]]$outs$loadRegs)) {
        expect_that(!is.na(rloadest_example_results[[i]]$outs$loadRegs[[j]]$flow.units), is_true())
      }
    }
    
  })
  
  test_that(paste(if(withfix) "with" else "without","fix, loadReg objects work as loadReg objects should"), {
    
    rloadest_example_results <- run_rloadest_examples(test.inoutputs.path)
    for(i in 1:length(rloadest_example_results)) {
      for(j in 1:length(rloadest_example_results[[i]]$outs$loadRegs)) {
        lr <- rloadest_example_results[[i]]$outs$loadRegs[[j]]
        expect_that(lr, is_a("loadReg"))
        # Test the S3 methods for loadRegs
        expect_that(names(coef(lr))[1], equals("(Intercept)"))
        expect_that(fitted(lr), is_a("numeric"))
        expect_that(logLik(lr), is_a("logLik"))
        #plot(lr) also works; visual inspection
        expect_that(print(lr), prints_text("Selected Load Model"))
        expect_that(residuals(lr), is_a("numeric"))
        expect_that(rmse(lr), is_a("numeric"))
        expect_that(vif(lr), is_a("numeric"))
      }
    }
    
  })
  
  # Leave dev_mode and reload rloadest from the usual directory
  unload(inst("rloadest"))
  dev_mode(on=FALSE)
  library("rloadest")
}
