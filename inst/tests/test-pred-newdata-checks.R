
test.inoutputs.path="../loadest_examples/app" # for calls from test()
#test.inoutputs.path="inst/loadest_examples/app" # for running these lines manually

for(withfix in c(FALSE, TRUE)) {

  if(withfix) {
    # To see if the patch worked, go into dev_mode and reload rloadest as the version in pangia/rloadest/pred-newdata-checks
    dev_mode(on=TRUE)
    unload(inst("rloadest"))
    install_github(repo="pangia/rloadest", ref="pred-newdata-checks")
  }
  
  test_that(paste(if(withfix) "with" else "without","fix, rloadest returns informative errors when newdata is missing columns in predLoad"), {
    
    ### Setup ###
    
    library("rloadest")
    
    LOADEST.inputs <- read_LOADEST_inputs(inputs.path=paste0(test.inoutputs.path,"1"))
    
    # Get & munge the calibration data
    calib.inp <- LOADEST.inputs$calib.inp
    names(calib.inp)[which(names(calib.inp)=="CCONC")] <- "Phosphorus"
    calib.inp$DATES <- as.Date(strptime(as.character(calib.inp$CDATE), format="%Y%m%d"))
    calib.inp$TIMES <- as.character(calib.inp$CTIME)
    calib.inp$FLOW <- calib.inp$CFLOW
    # We've now duplicated the rloadest data from the LOADEST data:
    expect_that(calib.inp[c("DATES","TIMES","FLOW","Phosphorus")], equals(app1.calib))
    
    # Fit a loadReg model
    app1_lr <- loadReg(
      formula=Phosphorus ~ model(1),
      data=calib.inp[c("DATES","FLOW","Phosphorus")],  
      flow="FLOW", 
      dates="DATES", 
      conc.units="mg/L",
      station="Illinois River at Marseilles, Ill.")
    expect_that(class(app1_lr) == "loadReg", is_true())
    
    # Get & munge the estimation data
    est.inp <- LOADEST.inputs$est.inp
    est.inp$DATES <- as.Date(strptime(as.character(est.inp$EDATE), format="%Y%m%d"))
    est.inp$TIMES <- as.character(est.inp$ETIME)
    est.inp$FLOW <- est.inp$EFLOW
    est.inp$GROUPING <- rep(1:4, each=24)
    # Again, we've duplicated the rloadest data from the LOADEST data:
    expect_that(est.inp[c("DATES","TIMES","FLOW")], equals(app1.calib[-which(names(app1.calib)=="Phosphorus")]))
    
    ### call predLoad - ERRORS ###
    
    # This call to predLoad is successful
    expect_that(predLoad(newdata=est.inp[c("DATES","FLOW")], fit=app1_lr, load.units="tons", by="unit"), is_a("data.frame"))
    expect_that(nrow(predLoad(newdata=est.inp[c("DATES","FLOW")], fit=app1_lr, load.units="tons", by="unit")), equals(nrow(est.inp)))
    
    # This call to predLoad fails because app1_lr implies that est.inp has a FLOW column, but est.inp actually has EFLOW
    # Failure is fine. However, the error is unhelpful and differs depending on the value of the 'by' argument.
    # What we get:
    if(!withfix) {
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="unit"), throws_error("arguments imply differing number of rows"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="day"), throws_error("arguments imply differing number of rows"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="month"), throws_error("arguments must have same length"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="water year"), throws_error("arguments must have same length"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="calendar year"), throws_error("arguments must have same length"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW","GROUPING")], load.units="tons", by="GROUPING"), throws_error("arguments must have same length"))
      expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="total"), throws_error("subscript out of bounds"))
    }
    
    ### More elegant failures ###
    
    # What would be more helpful would be an error that tells you how to fix the problem before it creates others
    new_error <- "newdata has missing, misnamed, or malformed predictor columns"
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="unit"), throws_error(new_error))
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="day"), throws_error(new_error))
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="month"), throws_error(new_error))
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="water year"), throws_error(new_error))
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="calendar year"), throws_error(new_error))
    expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW","GROUPING")], load.units="tons", by="GROUPING"), throws_error(new_error))
    expect_that(predLoad(newdata=est.inp[c("DATES","EFLOW")], fit=app1_lr, load.units="tons", by="total"), throws_error(new_error))
    
    # One solution to generate errors like this would be to add error handling at 
    # line 104, just after model.inp has been set by either model.matrix or 
    # setXLDat. For example, I've tried adding the following at predLoad:line104:
    
    # if (nrow(model.inp) == 0) stop("one or more expected columns in newdata were not found")
    
    # This is effective at stopping the above calls with that error message for 
    # any value of 'by'. A few more lines of code at that location could generate 
    # an even more explicit error message that explains which column (or other 
    # mismatch) created the problem.
    
    ### Proximate causes of error ###
    
    # I've traced these problems to the following errors.
    
    # by="unit" or by="day" throw errors at the point after estlday where they call
    # retval <- data.frame(Date = newdata[[dates]], Flow = Flow, 
    #  Flux = Flux, Std.Err = Std.Err, SEP = SEP, L95 = L95, 
    #  U95 = U95)
    # Because of the vectors passed to data.frame, all but newdata[[dates]] have length 0.
    # "estlday" has returned values of length 0 because model.inp has 0 rows. 
    # model.inp really should have nrow(est.inp) rows, but it doesn't because 
    # newdata$FLOW was not available, and this problem hasn't been caught by this 
    # point in the code.
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="unit"), throws_error("arguments imply differing number of rows"))
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="day"), throws_error("arguments imply differing number of rows"))
    
    # by="month","water year","calendar year", or an arbitrary column names gives 
    # problems at the call to tapply(seq(nrow(model.inp)), newdata[[by]], ...) on 
    # line 360. seq(nrow(model.inp)) has length 2, while newdata has length 96. 
    # They need to have the same length. This could be caught by comparing the
    # lengths of these two objects, but it seems that a better fix would be to
    # make sure that model.inp and newdata have the same nrow, and to run that
    # check much earlier in the code (where I suggested putting the stop() call)
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="month"), throws_error("arguments must have same length"))
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="water year"), throws_error("arguments must have same length"))
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="calendar year"), throws_error("arguments must have same length"))
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW","GROUPING")], load.units="tons", by="GROUPING"), throws_error("arguments must have same length"))
    
    # by='total' fails at line 334, xlpred=model.inp[KDays,], because model.inp
    # has 0 rows and cannot be indexed by KDays=c(1,0). The main problem is that
    # model.inp has 0 rows when it should have nrow(est.inp) rows. A secondary,
    # proximate problem is that KDays is c(1,0) because of the call KDays <-
    # seq(nrow(model.inp)) in line 305, because seq(0) gives c(1,0).
    #expect_that(predLoad(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], load.units="tons", by="total"), throws_error("subscript out of bounds"))  
    
    # Because all three of these errors stem from the same fundamental problem, I
    # think it makes the most sense to prevent these errors through the above
    # suggestion to add a check for nrow(model.inp)==0 right after building
    # model.inp.
    
  })
  
  test_that(paste(if(withfix) "with" else "without","fix, rloadest returns informative errors when newdata is missing columns in predConc"), {
    
    ### Setup ###
    
    library("rloadest")
    
    LOADEST.inputs <- read_LOADEST_inputs(inputs.path=paste0(test.inoutputs.path,"1"))
    
    # Get & munge the calibration data
    calib.inp <- LOADEST.inputs$calib.inp
    names(calib.inp)[which(names(calib.inp)=="CCONC")] <- "Phosphorus"
    calib.inp$DATES <- as.Date(strptime(as.character(calib.inp$CDATE), format="%Y%m%d"))
    calib.inp$TIMES <- as.character(calib.inp$CTIME)
    calib.inp$FLOW <- calib.inp$CFLOW
    # We've now duplicated the rloadest data from the LOADEST data:
    expect_that(calib.inp[c("DATES","TIMES","FLOW","Phosphorus")], equals(app1.calib))
    
    # Fit a loadReg model
    app1_lr <- loadReg(
      formula=Phosphorus ~ model(1),
      data=calib.inp[c("DATES","FLOW","Phosphorus")],  
      flow="FLOW", 
      dates="DATES", 
      conc.units="mg/L",
      station="Illinois River at Marseilles, Ill.")
    expect_that(class(app1_lr) == "loadReg", is_true())
    
    # Get & munge the estimation data
    est.inp <- LOADEST.inputs$est.inp
    est.inp$DATES <- as.Date(strptime(as.character(est.inp$EDATE), format="%Y%m%d"))
    est.inp$TIMES <- as.character(est.inp$ETIME)
    est.inp$FLOW <- est.inp$EFLOW
    est.inp$GROUPING <- rep(1:4, each=24)
    # Again, we've duplicated the rloadest data from the LOADEST data:
    expect_that(est.inp[c("DATES","TIMES","FLOW")], equals(app1.calib[-which(names(app1.calib)=="Phosphorus")]))
    
    ### call predConc - ERRORS ###
    
    # This call to predConc is successful
    expect_that(predConc(newdata=est.inp[c("DATES","FLOW")], fit=app1_lr, by="unit"), is_a("data.frame"))
    expect_that(nrow(predConc(newdata=est.inp[c("DATES","FLOW")], fit=app1_lr, by="unit")), equals(nrow(est.inp)))
    
    # This call to predConc fails because app1_lr implies that est.inp has a FLOW column, but est.inp actually has EFLOW
    # Failure is fine. However, the error is unhelpful and differs depending on the value of the 'by' argument.
    # What we get:
    if(!withfix) {
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="unit"), throws_error("arguments imply differing number of rows"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="day"), throws_error("arguments imply differing number of rows"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="month"), throws_error("arguments must have same length"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="water year"), throws_error("arguments must have same length"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="calendar year"), throws_error("arguments must have same length"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW","GROUPING")], by="GROUPING"), throws_error("arguments must have same length"))
      expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="total"), throws_error("subscript out of bounds"))
    }
    
    ### More elegant failures ###
    
    # What would be more helpful would be an error that tells you how to fix the problem before it creates others
    new_error <- "newdata has missing, misnamed, or malformed predictor columns"
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="unit"), throws_error(new_error))
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="day"), throws_error(new_error))
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="month"), throws_error(new_error))
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="water year"), throws_error(new_error))
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW")], by="calendar year"), throws_error(new_error))
    expect_that(predConc(fit=app1_lr, newdata=est.inp[c("DATES","EFLOW","GROUPING")], by="GROUPING"), throws_error(new_error))
    expect_that(predConc(newdata=est.inp[c("DATES","EFLOW")], fit=app1_lr, by="total"), throws_error(new_error))
  })
  
  if(withfix) {
    # Leave dev_mode and reload rloadest from the usual directory
    unload(inst("rloadest"))
    dev_mode(on=FALSE)
    library("rloadest")
  }
}