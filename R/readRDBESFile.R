getAllRequiredTables <- function(){
  getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
}


#'@param fileToRead path to RDBES file to read.
readRDBESFile <- function(fileToRead){
  # Load the validation data
  validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
  #validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

  # 30/8/2021 Temp fix because the validation fields aren't up to date :-(
  validationData[validationData$type == 'tRS_Sex','type'] <- 'tSEXCO'

  # Load the reference data: either refresh from ICES or just use a local copy
  allowedValues <- loadReferenceData(downloadFromICES = FALSE)
  # allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

  # Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
  allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
  # allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

  myExchangeFile <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = fileToRead, RequiredTables = allRequiredTables )

  return (myExchangeFile)
}
