
#### LOADEST ####

run_LOADEST <- function(inputs.path="inst/loadest_examples/app1", loadest.exe.path="C:/Users/aa20/Dropbox/Software/loadest/loadest.exe") {
  saveddir <- getwd()
  setwd(inputs.path)
  system(loadest.exe.path)
  setwd(saveddir)
}

run_LOADEST_examples <- function(inputs.path="inst/loadest_examples/app", loadest.exe.path="C:/Users/aa20/Dropbox/Software/loadest/loadest.exe") {
  lapply(1:7, function(i) {
    print(paste("Running LOADEST example application",i))
    system.time(run_LOADEST(paste0(inputs.path,i), loadest.exe.path=loadest.exe.path))
  })
}
# LOADEST_timings <- run_LOADEST_examples()

#### rloadest ####

run_rloadest_from_LOADEST <- function(LOADEST.inputs=NA, example.num=7, use.Date=TRUE, inoutputs.path="inst/loadest_examples/app") {
  require(rloadest)
  if(is.na(LOADEST.inputs)) {
    if(is.na(example.num)) {
      stop("One of LOADEST.inputs and example.num should be specified.")
    } else {
      example.num <- as.integer(example.num)
      if(exists("LOADEST_example_inputs")) {
        LOADEST.inputs <- LOADEST_example_inputs[[example.num]]
      } else {
        LOADEST.inputs <- read_LOADEST_inputs(inputs.path=paste0(inoutputs.path,example.num))
      }
      if(exists("LOADEST_example_outputs")) {
        LOADEST.outputs <- LOADEST_example_outputs[[example.num]]
      } else {
        LOADEST.outputs <- read_LOADEST_outputs(outputs.path=paste0(inoutputs.path,example.num))
      }
    }
  } else {
    example.num == as.integer(0)
  }
  
  ### Fit the regressions ###
  
  if(use.Date==TRUE) {
    loadreg.data <- transform(LOADEST.inputs$calib.inp, DATE=as.Date(strptime(as.character(LOADEST.inputs$calib.inp$CDATE), format="%Y%m%d")))
  } else {
    loadreg.data <- transform(LOADEST.inputs$calib.inp, DATE=LOADEST.inputs$calib.inp$DATETIME)
  }
  
  loadreg.formula <- NULL
  if(example.num == 1) {
    loadreg.data <- loadreg.data
    loadreg.formula <- NULL # use default based on MODNO
  } else if(example.num == 2) {
    loadreg.data <- transform(loadreg.data, Period=seasons(DATE,breaks=c("Apr", "Jul")))
    loadreg.formula <- "Period*center(log(CFLOW))"
  } else if(example.num == 3) {
    # D Lorenz's example 3 differs from LOADEST's example 3 in the source dataset
    #   app3.qw <- importNWISqw("01646580", params="00660", begin.date="2001-10-01", end.date="2010-09-30")
    #   app3.flow <- renCol(readNWIS("01646502", begin.date="2001-10-01", end.date="2010-09-30"))
    #   app3.qw <- subset(app3.qw, !duplicated(sample_dt))
    #   names(app3.qw)[2] <- "datetime"
    #   app3.calib <- mergeQ(app3.qw, FLOW="Flow", DATES="datetime", Qdata=app3.flow, Plot=FALSE)
    #   head(app3.calib)
    # The important thing here is that app3.calib has a concentration column that has format qw (for censored data); see ?USGSwsQW::as.qw.
    # This can also be done using convert2qw, which I'll demonstrate for example.num==4 below.
    loadreg.data <- transform(
      loadreg.data, CCONC=as.qw(as.numeric(gsub("<","",loadreg.data$CCONC)), remark.codes=gsub("[[:digit:].]","",loadreg.data$CCONC), 
                                value.codes="", reporting.level=as.numeric(NA), reporting.method="", reporting.units="mg/l", 
                                analyte.method="ALGOR", analyte.name="Phosphate", unique.code="00660")) # see showMethods("as.qw") for the long signature required
    loadreg.formula <- NULL
  } else if(example.num == 4) {
    loadreg.data <- transform(
      loadreg.data, 
      Buty.rmk=gsub("[[:digit:].]","",loadreg.data$buty.,),
      Buty=as.numeric(gsub("<","",loadreg.data$buty.)),
      Atra=atra.,
      Alach.rmk=gsub("[[:digit:].]","",loadreg.data$alach.,),
      Alach=as.numeric(gsub("<","",loadreg.data$alach.)),
      SuspSed=replace(spsed., which(spsed.==-9999), NA))
    loadreg.data <- convert2qw(loadreg.data, scheme="partial")
    loadreg.data <- transform(
      loadreg.data,
      buty.=Buty,
      atra.=Atra,
      alach.=Alach,
      spsed.=SuspSed)
    loadreg.formula <- NULL # use default based on MODNO
  } else if(example.num == 5) {
    loadreg.formula <- "center(log(CFLOW)) + log(CADDL)"
  } else if(example.num == 6) {
    loadreg.formula <- "log(CADDL1) + log(CADDL2)"
  } else if(example.num == 7) {
    loadreg.data <- transform(
      loadreg.data, 
      CCONC=as.numeric(gsub("<","",CCONC)),
      CCONC.rmk=gsub("[[:digit:].]","",CCONC))
    loadreg.data <- convert2qw(loadreg.data[,c("CDATE","CTIME","CFLOW","CCONC.rmk","CCONC","DATETIME","DATE")], scheme="partial")
    loadreg.formula <- NULL # use default based on MODNO
  }
  
  constnames <- LOADEST.inputs$header.inp$CNAME_UCFLAG_ULFLAG$CNAME
  loadregs <- list()
  for(const in 1:LOADEST.inputs$header.inp$NCONST) {
    #const=1 # comment out soon
    loadregs[[constnames[const]]] <- do.call("loadReg",list(
      
      formula=as.formula(paste0(
        constcol=names(LOADEST.inputs$calib.inp)[3+const+(if(is.null(LOADEST.inputs$header.inp$NADDL)) 0 else LOADEST.inputs$header.inp$NADDL)],
        " ~ ",
        if(is.null(loadreg.formula) & LOADEST.inputs$header.inp$MODNO %in% 0:9) {
          paste0("model(",modelnum=LOADEST.outputs$out[[const]]$LOAD$MODNO,")")
        } else if(!is.null(loadreg.formula)) {
          loadreg.formula
        } else {
          stop("Need an explicit formula specification")
        })),
      
      data=loadreg.data, 
      
      # subset - don't give anything,
      # na.action - don't give anything,
            
      flow="CFLOW", 
      dates="DATE", 
      
      # see https://github.com/USGS-R/rloadest/blob/master/R/loadConvFactor.R for accepted units:
      flow.units="cfs", # loadReg will accept: c("cubic meter per second", "cms"); anything else is assumed to be cfs. LOADEST requires cfs for CFLOW
      conc.units=c("mg/L","ug/L")[LOADEST.inputs$header.inp$CNAME_UCFLAG_ULFLAG[const,"UCFLAG"]], # loadReg will accept: mg/l, mg/L, ug/l, ug/L, ng/l, ng/L, milligrams per liter, micrograms per liter, nanograms per liter, col/100mL, col/dL, colonies per 100mL
      # I had thought that the load outputs would correspond to the ULFLAG in header.inp, but now I think they're just always kg...
      #load.units=c("kg","g","pounds","tons")[LOADEST.inputs$header.inp$CNAME_UCFLAG_ULFLAG[const,"ULFLAG"]], # loadReg will accept: pounds, tons, mg, milligrams, grams, g, kilograms, kg, `metric tons`, Mg, `million colonies`
      load.units="kg", 
      
      time.step=if(use.Date==TRUE) {"day"} else {"instantaneous"},
      
      station=LOADEST.inputs$header.inp$TITLE))
  }
  
  
  ### Run the predictions ###
  
  if(use.Date==TRUE) {
    loadpred.data <- transform(LOADEST.inputs$est.inp, DATE=as.Date(strptime(as.character(LOADEST.inputs$est.inp$CDATE), format="%Y%m%d")))
  } else {
    loadpred.data <- transform(LOADEST.inputs$est.inp, DATE=LOADEST.inputs$est.inp$DATETIME)
  }
  loadpred.data$CFLOW <- loadpred.data$EFLOW
  if(example.num == 1) {
    # nothing needed
  } else if(example.num == 2) {
    loadpred.data <- transform(loadpred.data, Period=seasons(DATE,breaks=c("Apr", "Jul")))
  } else if(example.num == 3) {
    # nothing needed
  } else if(example.num == 4) {
    # nothing needed
  } else if(example.num == 5) {
    loadpred.data$CADDL <- loadpred.data$EADDL
  } else if(example.num == 6) {
    loadpred.data$CADDL1 <- loadpred.data$EADDL1
    loadpred.data$CADDL2 <- loadpred.data$EADDL2
  } else if(example.num == 7) {
    # nothing needed
  }
  
  loadpreds <- list()
  for(const in 1:LOADEST.inputs$header.inp$NCONST) {
    loadpreds[[constnames[const]]] <- do.call("predLoad",list(
      fit=loadregs[[constnames[const]]],      
      newdata=loadpred.data,      
      by="unit"      
    ))
  }
  
  concpreds <- list()
  for(const in 1:LOADEST.inputs$header.inp$NCONST) {
    concpreds[[constnames[const]]] <- do.call("predConc",list(
      fit=loadregs[[constnames[const]]],      
      newdata=loadpred.data,      
      by="unit"      
    ))
  }
  
  return(c(list(exampleNumber=example.num), list(loadRegs=loadregs), list(loadPreds=loadpreds), list(concPreds=concpreds)))
}
# LOADEST_example_inputs <- read_LOADEST_input_examples()
# rloadest_example_1 <- run_rloadest_from_LOADEST(example.num=1)
#rloadest_example_1 <- run_rloadest_from_LOADEST(example.num=1, use.Date=FALSE) # use.Date can be T or F for example 1, 2
# rloadest_example_2 <- run_rloadest_from_LOADEST(example.num=2)
# rloadest_example_3 <- run_rloadest_from_LOADEST(example.num=3, use.Date=FALSE) # use.Date=F is required for example 3
# rloadest_example_4 <- run_rloadest_from_LOADEST(example.num=4)
# rloadest_example_5 <- run_rloadest_from_LOADEST(example.num=5)
# rloadest_example_6 <- run_rloadest_from_LOADEST(example.num=6)
# rloadest_example_7 <- run_rloadest_from_LOADEST(example.num=7)

run_rloadest_examples <- function(inputs.path="inst/loadest_examples/app") {
  lapply(1:7, function(i) {
    #print(paste("Running rloadest example application",i))
    timing <- system.time(
      rloadest.out <- run_rloadest_from_LOADEST(example.num=i, use.Date=FALSE, inoutputs.path=inputs.path)
    )
    return(list(timing=timing, outs=rloadest.out))
  })
}
# rloadest_results <- run_rloadest_examples()