#' Script to check all the data stored in the SIRENO database of on shore sampling is properly exported to
#' the RDBES file, which is uploaded to RDBES database.
#' The data is compared at different levels:
#' - Number of trips by rim stratum
#' - Number of individuals by species
#' - Weights by rim stratum
#' 
#' The files required to run this script are:
#' - rdbes_h5_ices_2023_H5.csv, the H5 RDBES file containing the data to be uploaded to the RDBES database,
#' generated in SIRENO.
#' - IEOUPMUEDESTOTSIRENO_RIM_ICES_2023.TXT, the SIRENO report containing the trips stored in SIRENO database.
#' - IEOUPMUETALSIRENO_RIM_ICES_2023.TXT, the SIRENO report containing the lengths stored in SIRENO database.
#'
#' Those files must be stored in 'input' folder.
#'
#' Other data sets required are stored in 'data' folder.

# LOAD LIBRARIES ----
library(sapmuebase)
library(dplyr)

# utils functions
source("R/report_RDBES_SIRENO_utils.R")

# Load David Currie functions
source("R/RDBES_Functions.R")
# Temporary fix required for a function from icesVocab - otherwise the function breaks when it tries
# to download the EDMO code list (or any list containing carriage returns)
source("R/tempIcesVocabFix.R")

# Function to read the file. In this case, the reference data and the list of
# required tables for each hierarchy are already downloaded from RDBES github.
# The first time this function is used, its necessary to download it. See
# ReadExchangeFileExample_WKRDBEST2.R file to do it.
source("R/readRDBESFile.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen = 500) # big number of digits


# GLOBAL VARIABLES
DATA_PATH <- file.path(getwd(), "data")
INPUT_PATH <- file.path(getwd(), "input")

# FILENAMES ----
strata_file <- "maestro_estrato_tecnico-estrato_rim_SAP__rdbes_estratorim.csv"
species_file <- "rdbes_especies_2024_03_19.txt"
species_not_upload <- "species_not_upload_rdbes_2021__rdbes_especies_not_upload.csv"
new_metiers_file <- "correspondencias_metiers_nuevos.csv"


# ⬇️⬇️⬇️ YOU ONLY HAVE TO CHANGE THE FOLLOWING NAMES OF FILES ⬇️⬇️⬇️ ----
rdbes_file <- "rdbes_h5_ices_2023_H5.csv" # Must end with the hierarchy followed by ".csv"

catches_file <- "IEOUPMUEDESTOTSIRENO_RIM_ICES_2023.TXT"
lengths_file <- "IEOUPMUETALSIRENO_RIM_ICES_2023.TXT"
# ⬆️⬆️⬆️ YOU ONLY HAVE TO CHANGE THE PREVIOUS NAMES OF FILES ⬆️⬆️⬆️  ----


# IMPORT FILES
## strata ----
strata <- read.csv2(file.path(DATA_PATH, strata_file))
strata <- strata[strata$Scheme == "port", ]

## species ----
sps_rdbes <- read.csv2(file.path(DATA_PATH, species_file), sep = ",")
# remove Dipturus flossada from sps_rdbes (I don't know why it is there)
sps_rdbes <- sps_rdbes[sps_rdbes$ESPSIRENO != "Dipturus flossada", ]

## not upload species ----
sps_not_upload <- read.csv2(file.path(DATA_PATH, species_not_upload))

## new metiers ----
new_metiers <- read.csv2(file.path(DATA_PATH, new_metiers_file), sep = ";")

## rdbes files ----
# start_time <- Sys.time()
# h5 <- readRDBESFile(file.path(INPUT_PATH, rdbes_file))
# end_time <- Sys.time()
# time_taken <- end_time - start_time
# time_taken
# saveRDS(h5, paste0(INPUT_PATH, "/h5.rds"))
h5 <- readRDS(paste0(INPUT_PATH, "/h5.rds"))

### rdbes individual tables ----
h5_tables <- getAllRequiredTables()
h5_tables <- h5_tables$H5[1:length(h5_tables$H5) - 1]
# I don't know why the order getted from getAllRequiredTables of
# h5_tables is erroneous, so I have to fix it manually.:
h5_tables <- c("DE", "SD", "OS", "LE", "FT", "SS", "SA", "FM")
DE <- h5$DE
SD <- h5$SD
OS <- h5$OS
LE <- h5$LE
FT <- h5$FT
SS <- h5$SS
SA <- h5$SA
FM <- h5$FM


## sireno files ----
catches <- importRIMCatches(catches_file, INPUT_PATH)
lengths <- importRIMLengths(lengths_file, INPUT_PATH)

# COMPLETE TABLES ----
## h5 complete table ----
# Due to FT is a optional table, the merge of all the tables can't be done by
# Reduce() (this is the way done in H1 hierarchy), so must be done step by step
# with merge.
# FT is a optional table, so must be merged directly with its upper table LE.
# There are a problem whit the fields OSid in LE and FT tables, which is not
# properly filled. Maybe it is due to a error in the readRDBES() function, but I'm
# not sure. To fix this, the LE table is ommited in the initial merges. Later
# the FT and LE tables are merged in order to get the LEnationalFishingActivity
# and the metier6 variables, which are need for the script.

h5_complete <- merge(DE, SD, by = "DEid", all.x = TRUE)
h5_complete <- merge(h5_complete, OS, by = "SDid", all.x = TRUE)
h5_complete <- merge(h5_complete, FT, by = "OSid", all.x = TRUE)
# The LE is omitted:
# h5_complete <- merge(h5_complete, LE, by = "SDid", all.x = TRUE)
h5_complete <- merge(h5_complete, SS, by = "FTid", all.x = TRUE, all.y = TRUE)
h5_complete <- merge(h5_complete, SA, by = "SSid", all.x = TRUE)
h5_complete <- merge(h5_complete, FM, by = "SAid", all.x = TRUE)
# Now, to get the nationalFishingActivity and metier6 variables:
ft_metier <- FT[, c("FTunitName", "FTencryptedVesselCode")]
le_metier <- LE[, c("LEunitName", "LEencryptedVesselCode", "LEnationalFishingActivity", "LEmetier6")]
le_ft <- merge(le_metier,
               ft_metier,
               by.x = c("LEunitName", "LEencryptedVesselCode"),
               by.y = c("FTunitName", "FTencryptedVesselCode"))
h5_complete <- merge(h5_complete,
                     le_ft,
                     by.x = c("FTunitName", "FTencryptedVesselCode"),
                     by.y = c("LEunitName", "LEencryptedVesselCode"),
                     all.x = TRUE)

h5_complete$FMnumberAtUnit <- as.numeric(h5_complete$FMnumberAtUnit)
h5_complete$SAtotalWeightLive <- as.numeric(h5_complete$SAtotalWeightLive)
h5_complete$SAsampleWeightMeasured <- as.numeric(h5_complete$SAsampleWeightMeasured)

empty_columns <- get_empty_columns(h5_complete)

h5_clean <- h5_complete[, !names(h5_complete) %in% empty_columns]

h5_minimum_columns <- c(
  "DEid", "DEsamplingScheme", "DEyear", "DEstratumName", "DEsampled",
  "SDid",
  "OSid", "OSsamplingDate", "OSsampled",
  "LEnationalFishingActivity", "LEmetier6",
  "FTid", "FTsequenceNumber", "FTnumberOfHaulsOrSets", "FTsampled", "FTunitName",
  "SSid", "SSsequenceNumber", "SScatchFraction", "SSsampled",
  "SAid", "SAsequenceNumber", "SAparentSequenceNumber", "SAspeciesCode", "SAspeciesCodeFAO",
  "SAcatchCategory", "SAsex", "SAtotalWeightLive", "SAsampleWeightLive", "SAunitName",
  "SAlowerHierarchy", "SAsampled", "SAtotalWeightMeasured", "SAsampleWeightMeasured",
  "FMid", "FMpresentation", "FMclassMeasured", "FMnumberAtUnit", "FMtypeMeasured",
  "FMaccuracy", "FMconversionFactorAssessment", "FMtypeAssessment"
)

h5_minimum <- h5_complete[, c(h5_minimum_columns)]
h5_minimum$ESTRATO_RIM <- substr(h5_minimum$LEnationalFishingActivity, 4, length(h5_minimum$LEnationalFishingActivity))

## sireno catches table ----
catches <- merge(catches,
  strata,
  by = "ESTRATO_RIM",
  all.x = TRUE
)

# Update METIER_DCF with the updated metiers
catches$METIER_DCF <- apply(catches, 1, function(x) {
  if (x[["ESTRATO_RIM"]] == "RASCO_CN" & x[["METIER_DCF"]] == "GNS_DEF_>=100_0_0") {
    return("GNS_DEF_>=220_0_0")
  } else if (x[["ESTRATO_RIM"]] == "ENMALLE_AC" & x[["METIER_DCF"]] == "GNS_DEF_>=100_0_0") {
    return("GNS_DEF_100-119_0_0")
  } else {
    return(new_metiers[new_metiers[["Metier_DCF_2009.2020"]] == x[["METIER_DCF"]], "Metier_DCF_2021"])
  }
})

## sireno lengths table ----
lengths <- merge(lengths,
  strata,
  by = "ESTRATO_RIM",
  all.x = TRUE
)

# Update METIER_DCF with the updated metiers
lengths$METIER_DCF <- apply(lengths, 1, function(x) {
  if (x[["ESTRATO_RIM"]] == "RASCO_CN" & x[["METIER_DCF"]] == "GNS_DEF_>=100_0_0") {
    return("GNS_DEF_>=220_0_0")
  } else if (x[["ESTRATO_RIM"]] == "ENMALLE_AC" & x[["METIER_DCF"]] == "GNS_DEF_>=100_0_0") {
    return("GNS_DEF_100-119_0_0")
  } else {
    return(new_metiers[new_metiers[["Metier_DCF_2009.2020"]] == x[["METIER_DCF"]], "Metier_DCF_2021"])
  }
})

## sireno catches_lengths table ----
catches_lengths <- merge(catches,
                         lengths,
                         all.x = TRUE)
### Only MT2A samples
catches_lengths_uploaded <- catches_lengths[catches_lengths$COD_TIPO_MUE == 2, ]
### There are some species with validated field as NA, change it to FALSE
catches_lengths_uploaded[is.na(catches_lengths_uploaded$VALIDADO), "VALIDADO"] <- FALSE
### Only registers with valid lengths
catches_lengths_uploaded <- catches_lengths_uploaded[catches_lengths_uploaded$VALIDADO == TRUE, ]
### Remove not upload species
catches_lengths_uploaded <- catches_lengths_uploaded[!(catches_lengths_uploaded$COD_ESP_CAT %in% sps_not_upload$ESPCOD), ]

## sireno uploaded lengths table ----
## Filter not uploaded data
### Not valid hauls are not uploaded
uploaded_lengths <- lengths
uploaded_lengths <- lengths[lengths$VALIDADO == TRUE, ]

### Remove not upload species
uploaded_lengths <- uploaded_lengths[!(uploaded_lengths$COD_ESP_CAT %in% sps_not_upload$ESPCOD), ]

### Only MT2A samples
uploaded_lengths <- uploaded_lengths[uploaded_lengths$COD_TIPO_MUE == 2, ]


# COMPARE RDBES UPLOADED VS SIRENO ----
## Number of trips by rim stratum ----
trips_summary <- get_trips_summary_h5()

## Number of individuals by species ----
ind_by_sp_summary <- get_ind_by_sp_summary_h5()
# TODO: There are some species which can't be uploaded by certain rim stratum, but
# get_ind_by_sp_summary_h5() function doesn't filter by rim stratum, so it must be fixed.


## Weights by species ----
weights_by_rim_stratum_summary <- get_weights_by_rim_stratum_summary_h5()
