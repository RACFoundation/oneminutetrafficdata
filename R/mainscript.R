#' @title Batch Data Extraction of One Minute Traffic Count Data
#'
#' @author RAC Foundation, Luke Hudlass-Galley
#'
#' @description Motorway Incident Detection and Automatic Signalling (MIDAS) is a
#' sensor based network along UK motorways, and is designed to collect data regarding
#' traffic flows, average speeds and road occupancy, amongst other features, on the
#' road network. This data can be accessed via the MIDAS website
#' \url{https://www.midas-data.org.uk/} (login required) in the form of .tcd.bz2 files.
#'
#' The function \code{RoadData} is designed to improve the accessibility of the MIDAS data.
#' This function finds relevant .tcd and .tcd.bz2 files stored on
#' the user's machine and extracts desired information from them, returning it to the user.
#'
#' @param startDate The start of the date range (in the format "YYYY-MM-DD") the user
#' wishes to examine the data between (inclusive of this date).
#' @param endDate The end of the date range (in the format "YYYY-MM-DD") the user
#' wishes to examine the data between (inclusive of this date).
#' @param tcdFileRoot A string (without a "/" or "\\" character on the start or end of the string) which points to
#' the directory (from the current working directory) that the user's .tcd or .tcd.bz2 files are stored. These files must be stored
#' within this directory only and cannot be in any subfolders of this directory. Their names must
#' also follow the "XXDDMMYY.tcd" or "XXDDMMYY.tcd.bz2" format as downloaded from the MIDAS website,
#' where "XX" denotes the control office, "DD" denotes the day, "MM" denotes the month, and "YY"
#' denotes the year.
#' @param ... Any number of geographic addresses of the links/sites that the user wishes to examine.
#' The addresses do not need to be formatted in any particular way, and any number of addresses is allowed.
#' The examples show how either single sites or multiple links can be selected in the function.
#'
#' @examples JanuaryData <- RoadData("2016-01-01", "2016-01-31", "tcdFiles", "M25/4393A", "M25/4393B")
#' @examples 2013_A2/8283J <- RoadData("2013-01-01", "2013-12-31", "TrafficData/A2TrafficData", "A2/8283J")
#'
#' @return A list, where each item in the list corresponds to a site (in the order given in the function). These
#' items are matrices which contain the corresponding traffic data, spanning between 00:00 GMT \code{startDate} to 23:59 GMT
#' \code{endDate}.

RoadData <- function(startDate, endDate, tcdFileRoot, ...) {

  # Collect the unknown number of sites into a single vector.
  sites <- c(...)

  # Check that the arguments are valid, if so then proceed with the script.
  if (ValidateArguments(startDate, endDate, tcdFileRoot, sites) == TRUE) {

    # Generate sequence of dates between and including the start and end dates.
    dates <- seq(as.Date(startDate), as.Date(endDate), by = "days")

    # Collect file names based on the sites and the dates required.
    fileNames <- CollectFileNames(tcdFileRoot, sites, dates)

    # Extract the relevant data from the files.
    extractedData <- ExtractData(fileNames, sites, dates)
    return(extractedData)



  } else {
    print("Not valid.")

    ## Tell the user what is wrong with their query.

  }

}







ValidateArguments <- function(startDate, endDate, tcdFileRoot, sites) {

  # Assumes that the arguments are valid until it can be proven otherwise.
  isItValid             <- TRUE
  reasonsWhyItIsNotValid <- c()

  # Validate start and end dates
  dates         <- c(startDate, endDate)
  validateDates <- strsplit(dates, "-")

  for (i in 1:2) {
    if (length(validateDates[[i]]) != 3 || nchar(validateDates[[i]][1]) != 4 || nchar(validateDates[[i]][2]) != 2 || nchar(validateDates[[i]][3]) != 2 || as.numeric(validateDates[[i]][1]) > as.numeric(Sys.Date(), format = "%Y") || as.numeric(validateDates[[i]][2]) > 12 || as.numeric(validateDates[[i]][3]) > 31) {
      if (i == 1) {
        message <- "The start date is formatted incorrectly. It must be formatted as \"YYYY-MM-DD\".\n"
        reasonsWhyItIsNotValid <- rbind(reasonsWhyItIsNotValid, message)
      } else {
        message <- "The end date is formatted incorrectly. It must be formatted as \"YYYY-MM-DD\".\n"
        reasonsWhyItIsNotValid <- rbind(reasonsWhyItIsNotValid, message)
      }
    }
  }

  if (length(reasonsWhyItIsNotValid) == 0) {
    return(TRUE)
  } else {
    return(reasonsWhyItIsNotValid)
  }

}







CollectFileNames <- function(tcdFileRoot, sites, dates) {

  # Initialisation of the set of required files.
  listOfRequiredFiles <- c()

  controlOffice <- ControlOfficeLookupBasic(sites, dates, tcdFileRoot)

  # For each site and date combination, return the corresponding control office.
  for (i in 1:length(sites)) {
    for (j in 1:length(dates)) {

      formattedDate <- format(dates[j], format = "%d%m%y")

      # Generate corresponding file name.
      fileNameStem  <- paste(tcdFileRoot, "/", controlOffice[i], formattedDate, sep = "")

      # Add to list of files.
      if (controlOffice[i] != "Cannot find site" && controlOffice[i] != "Inconsistent office") {
        listOfRequiredFiles <- rbind(listOfRequiredFiles, fileNameStem)
      } else {
        listOfRequiredFiles <- rbind(listOfRequiredFiles, "NoFile")
      }
    }

    if (controlOffice[i] == "Cannot find site") {
      cat(paste("\nWARNING: Unable to find site '", sites[i], "' in the available files. Proceeding without this site.", sep = ""))
    } else if (controlOffice[i] == "Inconsistent office") {
      cat(paste("\nWARNING: No control office for site '", sites[i], "' found consistently over the given dates. Proceeding without this site.", sep = ""))
    }
  }



  #

  # Checks that the files required exist in the directory; if not then present a warning.
  setOfRequiredFiles <- unique(listOfRequiredFiles)
  cat("\n")

  for (k in 1:length(setOfRequiredFiles)) {

    tcdFile <- paste(setOfRequiredFiles[k], ".tcd", sep = "")
    bz2File <- paste(setOfRequiredFiles[k], ".tcd.bz2", sep = "")

    if (!file.exists(tcdFile) && !file.exists(bz2File) && setOfRequiredFiles[k] != "NoFile") {
      cat(paste("\nWARNING: Neither '", tcdFile, "' or '", bz2File, "' exist. Proceeding without these files.\n", sep = ""))
    }
  }

  return(listOfRequiredFiles)
}







ControlOfficeLookupBasic <- function(sites, dates, tcdFileRoot) {

  startDate <- format(dates[1], format = "%d%m%y")
  endDate   <- format(dates[length(dates)], format = "%d%m%y")

  tcdDirectory <- list.files(tcdFileRoot)

  startDateFiles <- c()
  endDateFiles <- c()

  for (i in 1:length(tcdDirectory)) {

    tcdFile <- tcdDirectory[i]

    fileDate <- substr(tcdFile, 3, 8)


    if (fileDate == startDate && (endsWith(tcdFile, "tcd") || endsWith(tcdFile, "tcd.bz2"))) {
      startDateFiles <- rbind(startDateFiles, paste(tcdFileRoot, "/", tcdFile, sep = ""))
    }
    if (fileDate == endDate && (endsWith(tcdFile, "tcd") || endsWith(tcdFile, "tcd.bz2"))) {
      endDateFiles <- rbind(endDateFiles, paste(tcdFileRoot, "/", tcdFile, sep = ""))
    }
  }


  # For each file, read it and find out what sites are in it.

  controlOfficeSitesStart <- c()
  controlOfficeSitesEnd <- c()

  for (j in 1:length(startDateFiles)) {

    cat(paste("\nChecking file '", startDateFiles[j], "' for information regarding control sites...", sep = ""))

    sitesInControlOffice <- c()

    file <- file(startDateFiles[j], "rb")


    # Basic file information.
    dataIdentifier        <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
    officeIdentifier      <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
    fileDate              <- readChar(file, nchars = 8, useBytes = FALSE)
    numberOfSites         <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryOneLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryTwoLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryThreeLength   <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")


    for (i in 1:numberOfSites) {

      geographicAddress <- readChar(file, nchars = 12, useBytes = FALSE)

      sitesInControlOffice <- rbind(sitesInControlOffice, c(geographicAddress, officeIdentifier))


      electronicAddress <- readBin(file, "integer", n = 1, size = 4)
      numberOfLanes     <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
      dataBulk          <- readBin(file, "integer", n = 1440 * (8 + (numberOfLanes * 5)), size = 1, signed = FALSE)
    }

    controlOfficeSitesStart <- rbind(controlOfficeSitesStart, sitesInControlOffice)

    cat(paste("\nInformation collected regarding site '", startDateFiles[j], "' (Progress = ", round(100*j)/(length(startDateFiles) + length(endDateFiles)), "%).\n", sep = ""))

    close(file)
  }




  for (j in 1:length(endDateFiles)) {

    cat(paste("\nChecking file '", endDateFiles[j], "' for information regarding control sites...", sep = ""))

    sitesInControlOffice <- c()

    file <- file(endDateFiles[j], "rb")


    # Basic file information.
    dataIdentifier        <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
    officeIdentifier      <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
    fileDate              <- readChar(file, nchars = 8, useBytes = FALSE)
    numberOfSites         <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryOneLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryTwoLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
    categoryThreeLength   <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")


    for (i in 1:numberOfSites) {

      geographicAddress <- readChar(file, nchars = 12, useBytes = FALSE)

      sitesInControlOffice <- rbind(sitesInControlOffice, c(geographicAddress, officeIdentifier))


      electronicAddress <- readBin(file, "integer", n = 1, size = 4)
      numberOfLanes     <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
      dataBulk          <- readBin(file, "integer", n = 1440 * (8 + (numberOfLanes * 5)), size = 1, signed = FALSE)
    }

    controlOfficeSitesEnd <- rbind(controlOfficeSitesEnd, sitesInControlOffice)

    cat(paste("\nInformation collected regarding site '", endDateFiles[j], "' (Progress = ", round(100*(j + length(startDateFiles))/(length(startDateFiles) + length(endDateFiles))), "%).\n", sep = ""))

    close(file)
  }

  listOfOffices <- c()

  for (i in 1:length(sites)) {

    if (length(controlOfficeSitesStart[which(grepl(sites[i], controlOfficeSitesStart[,1])), 2]) != 0) {

      if (controlOfficeSitesStart[which(grepl(sites[i], controlOfficeSitesStart[,1])), 2] == controlOfficeSitesEnd[which(grepl(sites[i], controlOfficeSitesEnd[,1])), 2]) {
        listOfOffices <- rbind(listOfOffices, controlOfficeSitesStart[which(grepl(sites[i], controlOfficeSitesStart[,1])), 2])
      } else {
        listOfOffices <- rbind(listOfOffices, "Inconsistent office")
      }
    } else {
      listOfOffices <- rbind(listOfOffices, "Cannot find site")
    }

  }

  return(listOfOffices)
}









### EXTRACT DATA - HELPER FUNCTION
ExtractData <- function(fileNames, sites, dates){

  # Initilisation of data list that will be returned.
  dataList <- list()

  for (i in 1:length(sites)) {

    # Initialising the multi-day, single site structure.
    MultiDaySingleSiteData <- c()

    for (j in 1:length(dates)) {

      # For each file, check that there exists a corresponding .CSV file with indexing information.
      fileNumber <- ((i - 1) * length(dates)) + j
      csvIndexingFile <- paste(fileNames[fileNumber], ".csv", sep = "")

      # Each of the possible file formats.
      tcdFile <- paste(fileNames[fileNumber], ".tcd", sep = "")
      bz2File <- paste(fileNames[fileNumber], ".tcd.bz2", sep = "")

      # If a .CSV file does not exist, create one.
      if (file.exists(tcdFile) || file.exists(bz2File)) {
        if (!file.exists(csvIndexingFile)) {
          cat(paste("\nGenerating indexing file for '", fileNames[fileNumber], ".tcd'", sep = ""))
          CreateIndexingFile(fileNames[fileNumber])
          cat("\nGeneration completed. Proceeding to parse data.")
        } else {
          cat(paste("\nIndexing file already generated for '", fileNames[fileNumber], ".tcd'. Proceeding to parse data.", sep = ""))
        }

        singleDaySingleSiteData <- ParseData(fileNames[fileNumber], sites[i])
        MultiDaySingleSiteData <- rbind(MultiDaySingleSiteData, singleDaySingleSiteData)

        cat(paste("\nData successfully parsed. (Total data extraction: ", round(100*fileNumber/(length(sites)*length(dates))), "%)\n", sep = ""))

      } else if (fileNames[fileNumber] != "NoFile") {

        cat(paste("\nWARNING: As neither '", tcdFile, "' or '", bz2File, "' exist, no data from these files has been parsed. (Total data extraction: ", round(100*fileNumber/(length(sites)*length(dates))), "%)\n", sep = ""))

      } else {

        cat(paste("\nWARNING: Unknown or inconsistent control office for site '", sites[i], "' on the date ", dates[j], "; no data has been parsed (Total data extraction: ", round(100*fileNumber/(length(sites)*length(dates))), "%)\n", sep = ""))
      }
    }

    dataList[[length(dataList) + 1]] <- MultiDaySingleSiteData
  }

  return(dataList)
}







### CREATE INDEXING FILE - HELPER FUNCTION
CreateIndexingFile <- function(localFile) {

  # Initialising the data frame that will be saved as a CSV.
  csvData <- c()

  # Each of the possible file formats.
  tcdFile <- paste(localFile, ".tcd", sep = "")
  bz2File <- paste(localFile, ".tcd.bz2", sep = "")

  # Selects either a .tcd or a .tcd.bz2 file to extract the indexing information from.
  if (file.exists(tcdFile)) {

    file <- file(tcdFile, "rb")

  } else if (file.exists(bz2File)) {

    file <- file(bz2File, "rb")

  } else {

    return()
  }

  # Basic file information.
  dataIdentifier        <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
  officeIdentifier      <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
  fileDate              <- readChar(file, nchars = 8, useBytes = FALSE)
  numberOfSites         <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
  categoryOneLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
  categoryTwoLength     <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")
  categoryThreeLength   <- readBin(file, "integer", n = 1, size = 2, signed = FALSE, endian = "big")

  indexNumber <- 1 + 1 + 8 + 2 + 2 + 2 + 2

  for (i in 1:numberOfSites) {

    geographicAddress <- readChar(file, nchars = 12, useBytes = FALSE)
    csvData <- rbind(csvData, cbind(geographicAddress, indexNumber))

    electronicAddress <- readBin(file, "integer", n = 1, size = 4)
    numberOfLanes     <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
    dataBulk          <- readBin(file, "integer", n = 1440 * (8 + (numberOfLanes * 5)), size = 1, signed = FALSE)

    indexNumber <- indexNumber + 12 + 4 + 1 + (1440 * (8 + (numberOfLanes * 5)))
  }

  close(file)

  write.csv(data.frame(csvData), file = paste(localFile, ".csv", sep = ""))
}






### PARSE DATA - HELPER FUNCTION
ParseData <- function(fileStem, site) {

  # Checks if user has the "timeDate" package; if not then it installs it.
  # if(!require(timeDate)){
  #   install.packages("timeDate")
  #   library(timeDate)
  # }
  library(timeDate)

  # Read in the indexing CSV file.
  indexData <- data.frame(read.csv(file = paste(fileStem, ".csv", sep = ""), header = TRUE, sep = ",", colClasses=c("NULL",NA,NA)))

  # Finds the numbers of bytes to skip in the data.
  #sitePosition <- indexData[match(site, indexData$geographicAddress),]$indexNumber
  sitePosition <- indexData[grepl(site, indexData$geographicAddress),]$indexNumber

  # Opens .tcd/.tcd.bz2 file and extracts its relevant content.
  tcdFile <- paste(fileStem, ".tcd", sep = "")
  bz2File <- paste(fileStem, ".tcd.bz2", sep = "")

  if (file.exists(tcdFile)) {

    file <- file(tcdFile, "rb")

  } else if (file.exists(bz2File)) {

    file <- file(bz2File, "rb")

  }

  # Collects information about the file date and times.
  controlOfficeNumber <- substring(fileStem, nchar(fileStem) - 7, nchar(fileStem) - 6)
  dateString <- substring(fileStem, nchar(fileStem) - 5, nchar(fileStem))
  fileDate <- as.Date(dateString, format = "%d%m%y")

  year  <- format(fileDate, format = "%Y")
  month <- format(fileDate, format = "%b")
  day   <- format(fileDate, format = "%d")

  dayOfWeek <- format(fileDate, format = "%a")

  if (dayOfWeek == "Mon" || dayOfWeek == "Tue" || dayOfWeek == "Wed" || dayOfWeek == "Thu" || dayOfWeek == "Fri") {
    typeOfDay <- "Weekday"
  } else {
    typeOfDay <- "Weekend"
  }

  # Format for each row being a minute in the day.
  time <- paste(formatC(floor(0:1439/60), width = 2, flag = "0"), ":", formatC(0:1439 - (floor(0:1439/60) * 60), width = 2, flag = "0"), sep = "")

  # Finds the nearest bank holidays and figures out how many days away it is.
  listOfBankHolidays <- as.Date(holidayLONDON(as.numeric(year)))
  nearestBankHoliday <- which.min(abs(fileDate - listOfBankHolidays))
  daysAfterNearestBankHoliday <- as.numeric(as.POSIXct(fileDate) - as.POSIXct(listOfBankHolidays[nearestBankHoliday]), units = "days")


  # Skips irrelevant data.
  skipData <- readBin(file, "integer", n = sitePosition, size = 1, signed = FALSE)

  # Collect relevant data.
  address           <- readChar(file, nchars = 12, useBytes = FALSE)
  electronicAddress <- readBin(file, "integer", n = 1, size = 4)
  numberOfLanes     <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
  dataBulk          <- readBin(file, "integer", n = 1440 * (8 + (numberOfLanes * 5)), size = 1, signed = FALSE)

  # No need for the file now.
  close(file)

  # Reformat the bulk of data.
  dim(dataBulk) <- c(8 + numberOfLanes*5, 1440)
  dataBulk <- t(dataBulk)

  # Extracting relevant information from the data bulk.
  flowPerCategories <- dataBulk[, c(2,4,6,8)]
  allData <- cbind(controlOfficeNumber, address, year, month, day, dayOfWeek, typeOfDay, daysAfterNearestBankHoliday, time, numberOfLanes, flowPerCategories)

  colnames(allData)[1:14] <- c("Control Office", "Geographic Address", "Year", "Month", "Day", "Day of Week", "Type of Day", "Days After Nearest Bank Holiday", "Time (GMT)", "Number of Lanes", "Flow (Category 1)", "Flow (Category 2)", "Flow (Category 3)", "Flow (Category 4)")

  for (i in 1:numberOfLanes) {

    attributes <- dataBulk[, 5*(i - 1) + c(9, 11, 12, 13)]
    allData <- cbind(allData, attributes)

    colnames(allData)[dim(allData)[2]-3] <- paste("Average Speed (Lane ", i, ")", sep = "")
    colnames(allData)[dim(allData)[2]-2] <- paste("Total Flow (Lane ", i, ")", sep = "")
    colnames(allData)[dim(allData)[2]-1] <- paste("Occupancy (Lane ", i, ")", sep = "")
    colnames(allData)[dim(allData)[2]] <- paste("Average Headway (Lane ", i, ")", sep = "")

  }

  return(allData)
}
