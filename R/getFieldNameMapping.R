#' The function getFieldNameMapping is taken from
#' https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles in 5/02/2024, with
#' some changes
#'
#' getFieldNameMapping Get a data frame that maps between database names and the
#' shorter R names
#'
#' @param downloadFromGitHub TRUE if you want to download the latest data model
#' spreadsheets from GitHub
#' @param gitHubDirectory (Optional) Default value is
#' "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents"
#' @param fileLocation The location you want to save and read the data model
#' spredsheet from
#' @return list of dataframes mapping the variables of RDBES exchange format.
#' @export


# CREATE DATAFRAMES WIHT THE NAME AND ORDER OF EVERY TABLE
getFieldNameMapping <- function(downloadFromGitHub= TRUE,
                                gitHubDirectory = "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents",
                                fileLocation){

  if (downloadFromGitHub){

    myDataModelFiles <- NULL
    myResponse <- httr::GET(gitHubDirectory)
    filesOnGitHub <- httr::content(myResponse)

    for (myFile in filesOnGitHub){
      myGitHubFile <- data.frame(fileName = myFile$name, downloadURL = myFile$download_url)
      if (is.null(myDataModelFiles)){
        myDataModelFiles <- myGitHubFile
      } else {
        myDataModelFiles <- rbind(myDataModelFiles,myGitHubFile)
      }
    }
    # Sub-set to the files we are interested in
    myDataModelFiles <- myDataModelFiles[grepl('^.*Data Model.*xlsx$',myDataModelFiles$fileName),]

    print(paste("Downloading ",nrow(myDataModelFiles), " files from GitHub", sep =""))

    # Download our files
    for (i in 1:nrow(myDataModelFiles)){
      aDataModelFile <- RCurl::getBinaryURL(myDataModelFiles[i,'downloadURL'])
      # save the file locally
      myFileConnection = file(paste(fileLocation,myDataModelFiles[i,'fileName'], sep = ""), "wb")
      writeBin(aDataModelFile, myFileConnection)
      aDataModelFile <- NA
      close(myFileConnection)
    }

    print("Finished downloading")

  }

  # Now we'll read the spreadsheets
  # (Need to find the names of the files again in case we haven't dowloaded them in this function call)

  filesToRead <- list.files(path = fileLocation, pattern = "*.xlsx", recursive = FALSE, full.names = FALSE)

  dataModel <- list()

  # get the contents of each relevent worksheet in our spreadsheets
  for (myfile in filesToRead){

    myFileLocation <- paste(fileLocation,myfile, sep = "")
    myFileSheets <- readxl::excel_sheets(myFileLocation)

    # CE CL
    if (grepl('^.*CL CE.*xlsx$',myfile)){

      print("Loading CL CE names")
      # Add the sheets to the dataModel list
      dataModel[['CE']] <- readxl::read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CE.*",myFileSheets)])
      dataModel[['CL']] <- readxl::read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CL.*",myFileSheets)])
    }
    # VD SL
    else if (grepl('^.*VD SL.*xlsx$',myfile)){

      print("Loading VD SL names")
      # Add the sheets to the dataModel list
      dataModel[['VD']] <- readxl::read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Vessel.*",myFileSheets)])
      dataModel[['SL']] <- readxl::read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Species.*",myFileSheets)])

    }
    # CS
    # else if (myfile == "RDBES Data Model.xlsx"){
    else if (myfile == "RDBES Data Model CS.xlsx"){

      for (aFileSheet in myFileSheets) {
        if (!grepl(".*Model.*",aFileSheet)){
          print(paste("Loading ", aFileSheet, " names", sep = ""))
          myDataModel <- readxl::read_excel(myFileLocation,sheet = aFileSheet)
          dataModel[[aFileSheet]] <- myDataModel
        }

      }
    }
  }

  # Put the field names and R names from each entry in the list into a single data frame

  myNameMappings <- NULL

  for (i in 1:length(dataModel)){

    myDataModelEntry <- dataModel[[i]]
    validColumnNames <- names(myDataModelEntry)[names(myDataModelEntry) %in% c("Field Name","R Name")]

    if (length(validColumnNames == 2)) {
      aNameMapping <- myDataModelEntry[,c("Field Name","R Name")]

      # Add in the Table Name - based on the first 2 letters of the first entry
      if (nrow(aNameMapping)>0) {
        tableName <- substr(aNameMapping[1,1],1,2)
      } else {
        tableName <- NA
      }
      aNameMapping$TableName <- tableName

      myNameMappings <- rbind(myNameMappings,aNameMapping)
    } else {
      print(paste("Not including ",names(dataModel)[i], " in mapping due to invalid column names",sep=""))
    }

  }

  # Remove any NAs
  if (!is.null(myNameMappings)){
    myNameMappings <- myNameMappings[!is.na(myNameMappings[,"Field Name"]) & !is.na(myNameMappings[,"R Name"]),]
  }

  myNameMappings

}
