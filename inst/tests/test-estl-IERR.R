
# # To see if the patch worked, go into dev_mode and reload rloadest as the version in pangia/rloadest/estl-IERR
# dev_mode(on=TRUE)
# unload(inst("rloadest"))
# install_github(repo="pangia/rloadest", ref="estl-IERR")
# library("rloadest")

test.inoutputs.path="../loadest_examples/app"

test_that("rloadest returns informative errors when IERR>0 in predLoad",{
  
  ### Setup ###
  
  LOADEST.inputs <- read_LOADEST_inputs(inputs.path="inst/loadest_examples/app1")
  
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
  
  # but i've managed to get IERR errors before...

})

test_that("rloadest returns informative errors when IERR>0 in predConc",{
  
})


# # Leave dev_mode and reload rloadest from the usual directory
# unload(inst("rloadest"))
# dev_mode(on=FALSE)
# library("rloadest")