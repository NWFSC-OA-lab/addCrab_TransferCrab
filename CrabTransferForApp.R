#This make new blank well transfer from from previous transfer data or add crab data
#Input is a filled out transfer from
#Output is a file that can be used to make a new blank transfer form that eleminates dead/removed/lost

library("stringr")

# #set working directory
# # ******
# setwd("/Users/paul.mcelhany/Documents/Ocean Acidification/Experiments/Crab/Crab Data Management/crabs 2018/Feeding day data")
# 
# #input file name. There are 2 types of input files 
# # 1) a filled out transfer or merge form with dead, remove, etc. data
# # 2) an "add crab table" when new crabs are added to new well plates (new experiment)
# # *******
# inputFileName <- "TransferCrab2018.03.19.csv"
# # Is input a new crabs added to new well plates (new experiment) file?
# # For making new transfering data sheets from old transfer or merge sheets, this is false 
# # ******

findIsNewCrab <- function(inputFileName){
  isNewCrabPlates <- FALSE
  testD <- read.csv(inputFileName, nrows = 1)
  if(names(testD)[1] == "plate"){
    isNewCrabPlates <- TRUE
  }
  return(isNewCrabPlates)
}

# #output file 
# # *******
# outputFileName <- "transTest5.csv"

# Date of the last transfer, merge or new crabs in new plates
# # ******
# lastDate <- "2018_01_22"
# # Date of planned  transfer
# # ******
# transferDate <- "2018_01_26"

# # Are the crabs being transfered back into the same chamber?
# # ******
# isSameChamber <- TRUE
# 
# # Are crabs being transfered to same positions in well plate (i.e. not condensing for space)
# # *****
# isSameWellPosition <- FALSE

getLastTransferDate <- function(inputFileName, isNewCrabPlates ){
  dateD <- NULL
  #read the date header from input file transfer file
  if(!isNewCrabPlates){
    dateD <- read.csv(inputFileName, header = FALSE, nrows = 2)
    dateD <- dateD[c(1,2)]
  }
  return(dateD)
}

getLastTransferFile <- function(inputFileName, isSameWellPosition){
  isNewCrabPlates <- findIsNewCrab(inputFileName)
  if(isNewCrabPlates){
    d <- read.csv(inputFileName, stringsAsFactors = FALSE)
  }
  else{
    # read input data table
    d <- read.csv(inputFileName, skip = 2, header = TRUE, stringsAsFactors = FALSE)
    d$Location <- d$startLoc
    # create subset of the input data table that includes all rows with a well location
    #(i.e not dead, removed or missing)
    if(!isSameWellPosition){
      d <- subset(d, Well != "")
      # order the file so it imports to the new form in correct sequence
      d <- d[order(d$Chmbr, d$Ptype, d$Pnum, d$Well),]
      # create composite location for each crab
      d$Location <- paste(d$Chmbr, d$Ptype, d$Pnum, d$Well, sep = "_")
    }
  }
  return(d)
}


# creates the new datadrame
getNewForm <- function(startD, isSameWellPosition, isSameChamber, isLastStage, wellAnalysisD){
  formD <- data.frame(crabID = startD$crabID, startLoc = startD$Location)
  #formD <- formD[order(formD$startLoc, formD$crabID),]
  #View(formD)
  formD$By <- ""
  formD$Molt <- ""
  formD$Dead <- ""
  formD$Remov<- ""
  formD$Lost<- ""
  formD$Chmbr <- ""
  if(isSameChamber){
    formD$Chmbr <- gsub("_.*", "", startD$Location)
  }
  formD$Ptype <- ""
  formD$Pnum <- ""
  formD$Well <- ""
  formD$crabID <- as.character(formD$crabID)
  if("Dead" %in% colnames(startD)){
    startD$Dead[is.na(startD$Dead)] <- ""
    startD$Remov[is.na(startD$Remov)] <- ""
    startD$Lost[is.na(startD$Lost)] <- ""
    formD$startLoc <- paste(startD$Chmbr, startD$Ptype, startD$Pnum, startD$Well, sep = "_")
  }
  if("Dead" %in% colnames(startD)){
    for(i in 1:length(startD$crabID)){
      if(startD$Dead[i] != "" ||  startD$Remov[i] != "" || startD$Lost[i] != ""){
        formD$crabID[i] <- "blank"
      }
    }
  }
  if(isSameWellPosition){
    formD$Ptype <- word(formD$startLoc, 2, sep = "_")
    formD$Well <- word(formD$startLoc, 4, sep = "_")
  }
  if(isLastStage && !is.null(wellAnalysisD)){
    formD <- merge(formD, wellAnalysisD, by = "crabID", all.x = TRUE, all.y = FALSE)
    
    formD <- formD[order(formD$startLoc),]
    formD$Ptype[formD$stageLastDay < 3] <- "W24"
    formD$Ptype[formD$stageLastDay > 3] <- "W12"
  }
  return(formD)
}
#View(formD)
#create date header
writeHeader <- function(outputFileName, lastDate, transferDate){
  lastDate <- format(lastDate, format = "%Y_%m_%d")
  transferDate <- format(transferDate, format = "%Y_%m_%d")
  header <- rbind(paste("Last Date,", lastDate, sep = ""),
                  paste("Transfer Date,", transferDate, sep = ""))
  #write the header
  writeLines(header, outputFileName)
}
#write the main form
writeNewForm <- function(formD, outputFileName){
  formD <- subset(formD, crabID != "")
  write.table(formD, file = outputFileName, append = TRUE, row.names = FALSE, sep = ",", col.names = TRUE, na = "")
}

getOutfileName <- function(inputFileName, nextTransferDate){
  chamber <- word(inputFileName, 2, sep = "_")
  tDate <- format(nextTransferDate, format = "%Y.%m.%d")
  outfileName <- paste("TransferCrab", chamber, tDate, sep = "_")
  outfileName <- paste(outfileName, ".csv", sep = "")
  return(outfileName)
}


getLastStageDF <- function(wellFilePath){
  d <- read.csv(wellFilePath, stringsAsFactors = FALSE)
  dd <- data.frame(crabID = levels(factor(subset(d, exp != "TC")$crabID)))
  dd$stageLastDay <- NA
  for(i in 1:length(dd$crabID)){
    dd$stageLastDay[i] <- max(subset(d, crabID == dd$crabID[i])$stage)
  }
  return(dd)
}
# setwd("/Users/paul.mcelhany/Downloads")
# inputFileName <- "addCrab_CH01_new.2018.03.22.csv"
# d2 <- getLastTransferFile(inputFileName, TRUE)
# View(d2)
# d3 <- getNewForm(d2, TRUE)
# View(d3)








