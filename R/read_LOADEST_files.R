library(testthat)

#### helpers ####

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

#### inputs ####

read_LOADEST_inputs <- function(inputs.path="inst/loadest_examples/app2") {
  read.header.inp <- function(inputs.path) {
    
    all.lines <- readLines(paste0(inputs.path,"/header.inp"))
    code.lines <- all.lines[!grepl("^#",all.lines) & nchar(all.lines)>0]
    header.list <- list()
    header.list[["TITLE"]] <- read.fwf(textConnection(code.lines[1]), widths=80, as.is=TRUE, comment.char="")[1,1]
    header.list[["PRTOPT"]] <- as.integer(read.fwf(textConnection(code.lines[2]), widths=5, as.is=TRUE)[1,1])
    header.list[["SEOPT"]] <- as.integer(read.fwf(textConnection(code.lines[3]), widths=5, as.is=TRUE)[1,1])
    header.list[["LDOPT"]] <- as.integer(read.fwf(textConnection(code.lines[4]), widths=5, as.is=TRUE)[1,1])
    if(header.list[["LDOPT"]] %in% c(1L,3L)) {
      header.list[["NSEAS"]] <- as.numeric(read.fwf(textConnection(code.lines[5]), widths=5, as.is=TRUE)[1,1])
      line.num <- 6
      header.list[["SBEG_SEND"]] <- setNames(read.csv(textConnection(code.lines[line.num:(line.num + header.list[["NSEAS"]] - 1)]), sep="", as.is=TRUE, header=FALSE, colClasses=c("character","character")),c("SBEG","SEND"))
      line.num <- line.num + header.list[["NSEAS"]]
    } else {
      header.list[["NSEAS"]] <- NULL
      header.list[["SBEG_SEND"]] <- NULL
      line.num <- 5
    }
    header.list[["MODNO"]] <- as.integer(read.fwf(textConnection(code.lines[line.num]), widths=5, as.is=TRUE)[1,1])
    line.num <- line.num + 1
    if(header.list[["MODNO"]] %in% c(10L,11L)) {
      header.list[["PBMON_PEMON"]] <- setNames(read.fwf(textConnection(code.lines[line.num]), widths=c(5,5), as.is=TRUE),c("PBMON","PEMON"))
      line.num <- line.num + 1
    } else {
      header.list[["PBMON_PEMON"]] <- NULL
    }
    if(header.list[["MODNO"]] == 99L) {
      header.list[["NADDL"]] <- read.fwf(textConnection(code.lines[line.num]), widths=5, as.is=TRUE)[1,1]
      header.list[["NEXPL"]] <- read.fwf(textConnection(code.lines[line.num+1]), widths=5, as.is=TRUE)[1,1]
      line.num <- line.num + 2
      if(header.list[["NEXPL"]] > 0) {
        header.list[["DVNAME_TRANS"]] <- setNames(read.csv(textConnection(code.lines[line.num:(line.num+header.list[["NEXPL"]]-1)]), sep="", as.is=TRUE, header=FALSE, colClasses=c("character","character")),c("DVNAME","TRANS"))
        line.num <- line.num + header.list[["NEXPL"]]
      }
    } else {
      header.list[["NADDL"]] <- header.list[["NEXPL"]] <- header.list[["DVNAME_TRANS"]] <- NULL
    }
    header.list[["NCONST"]] <- as.integer(read.fwf(textConnection(code.lines[line.num]), widths=5, as.is=TRUE)[1,1])
    line.num <- line.num + 1
    header.list[["CNAME_UCFLAG_ULFLAG"]] <- setNames(read.fwf(textConnection(code.lines[line.num:(line.num+header.list[["NCONST"]]-1)]), widths=c(45,5,5), as.is=TRUE, colClasses=c("character","integer","integer")),c("CNAME","UCFLAG","ULFLAG"))    
    header.list[["CNAME_UCFLAG_ULFLAG"]]$CNAME <- trim(header.list[["CNAME_UCFLAG_ULFLAG"]]$CNAME)
    line.num <- line.num + header.list[["NCONST"]] - 1
    expect_that(line.num, equals(length(code.lines)))
    
    return(header.list)
  }
  header.inp <- read.header.inp(inputs.path)
  
  read.calib.inp <- function(inputs.path) {
    all.lines <- readLines(paste0(inputs.path,"/calib.inp"))
    names.line <- grep("#CDATE", all.lines, fixed=TRUE)[1]
    data.lines <- seq(names.line + 3, length(all.lines))
    toread.lines <- all.lines[c(names.line,data.lines)]
    toread.lines[1] <- substr(toread.lines[1],2,nchar(toread.lines[1]))
    calib.inp <- read.csv(textConnection(toread.lines), as.is=TRUE, header=TRUE, sep="")  
    calib.inp$DATETIME <- strptime(paste0(calib.inp$CDATE, " ", ifelse(nchar(calib.inp$CTIME)==4,"","0"), calib.inp$CTIME), format="%Y%m%d %H%M")
    calib.inp
  }
  calib.inp <- read.calib.inp(inputs.path)
  
  read.est.inp <- function(inputs.path) {
    all.lines <- readLines(paste0(inputs.path,"/est.inp"))
    names.line <- grep("EDATE", all.lines, fixed=TRUE)[1]
    data.lines <- seq(names.line + 3, length(all.lines))
    toread.lines <- all.lines[c(names.line,data.lines)]
    toread.lines[1] <- substr(toread.lines[1],2,nchar(toread.lines[1]))
    est.inp <- read.csv(textConnection(toread.lines), as.is=TRUE, header=TRUE, sep="")  
    est.inp$DATETIME <- strptime(paste0(est.inp$EDATE, " ", ifelse(nchar(est.inp$ETIME)==4,"","0"), est.inp$ETIME), format="%Y%m%d %H%M")
    est.inp
  }
  est.inp <- read.est.inp(inputs.path)
  
  return(list(
    header.inp=header.inp, 
    calib.inp=calib.inp, 
    est.inp=est.inp))
}

read_LOADEST_input_examples <- function(path="inst/loadest_examples/app") {
  lapply(1:7, function(i) {
    read_LOADEST_inputs(paste0(path,i))
  })
}

#### outputs ####

read_LOADEST_outputs <- function(outputs.path="loadest/examples/app3", constituents=NA) {
  if(isTRUE(is.na(constituents))) {
    constituents <- gsub("\\s","",read_LOADEST_inputs(outputs.path)[[1]]$CNAME_UCFLAG_ULFLAG$CNAME)
  }
  names(constituents) <- constituents

  # Save the echo.out filenames; the file is pretty complex for comprehensive parsing
  outfile.list <- list(echo.out=paste0(outputs.path,"/echo.out"))
  
  # Save .out filenames; files are pretty complex for comprehensive parsing
  outfile.list$out <- lapply(constituents, function(const) {
    #const <- constituents[1]
    all.lines <- readLines(paste0(outputs.path,"/",const,".out"))
    
    ## OVERVIEW
    out.list <- list()
    out.list[["TITLE"]] <- read.fwf(textConnection(all.lines[8]), widths=80, as.is=TRUE, comment.char="")[1,1]
    
    line <- grep("Number of Observations[ :]*([[:digit:]]*)", all.lines)
    out.list[["NCALIB"]] <- as.integer(regmatches(all.lines[line], regexec("Number of Observations[ :]*([[:digit:]]*)", all.lines[line]))[[1]][2])
    
    ## LOAD MODEL
    out.list[["LOAD"]] <- list()
    
    out.list[["LOAD"]][["MODNO"]] <- as.numeric(regmatches(paste0(all.lines[1:100],collapse=""), regexec("Model ([# [:digit:]])* selected", paste0(all.lines[1:100],collapse="")))[[1]][2])
    
    line <- grep(" Selected Model:", all.lines)
    out.list[["LOAD"]][["MODEL"]] <- paste0(trim(all.lines[(line+3):(line+grep("where:",all.lines[line+3:12]))]),collapse=" ")
    
    line <- grep("Load  = constituent load \\[", all.lines)
    out.list[["LOAD"]][["UNITS"]] <- regmatches(all.lines[line], regexec("Load[ ]*= constituent load \\[(.*)]", all.lines[line]))[[1]][2]
    
    line <- grep("Model Coefficients", all.lines)
    out.list[["LOAD"]][["COEFS"]] <- read.csv(textConnection(all.lines[line+c(2,4:grep("^$",all.lines[line:(line+10)])[2])]), sep="", header=TRUE)
    
    line <- grep("Summary Stats: Est. and Obs. Loads in", all.lines)
    out.list[["LOAD"]][["QUANTILES"]] <- setNames(read.csv(textConnection(all.lines[line+5:7]), sep="", header=FALSE), c("Type","Min","25th","Med","75th","90th","95th","99th","Max"))
        
    line <- grep("R-Squared [%]", all.lines, fixed=TRUE)[1] # the first one is for load
    out.list[["LOAD"]][["RSQ"]] <- as.numeric(regmatches(all.lines[line], regexec("R-Squared[^:]*[: ]+([[:digit:].]+)", all.lines[line]))[[1]][2])
    
    out.list
  })
  
  outfile.list$ind <- lapply(constituents, function(const) {
    dfnames <- as.character(read.csv(paste0(outputs.path,"/",const,".ind"), skip=6, nrows=1, header=FALSE, sep="", as.is=TRUE))
    df <- read.csv(paste0(outputs.path,"/",const,".ind"), skip=8, header=FALSE, sep="", colClasses=c("character","character",rep("numeric",length(dfnames)-2)))
    names(df) <- dfnames
    df
  })
  
  outfile.list$res <- lapply(constituents, function(const) {
    dfnames <- as.character(read.csv(paste0(outputs.path,"/",const,".ind"), skip=6, nrows=1, header=FALSE, sep="", as.is=TRUE))
    df <- read.csv(paste0(outputs.path,"/",const,".ind"), skip=8, header=FALSE, sep="", colClasses=c("character","character",rep("numeric",length(dfnames)-2)))
    names(df) <- dfnames
    df
  })
  
  return(outfile.list)
}

read_LOADEST_output_examples <- function(path="inst/loadest_examples/app") {
  lapply(1:7, function(i) {
    read_LOADEST_outputs(paste0(path,i))
  })
}
