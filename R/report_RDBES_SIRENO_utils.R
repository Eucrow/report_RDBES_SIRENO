#' Get empty columns of a data frame.
#' @param df data frame.
#' @return a vector with the names of the empty columns.
get_empty_columns <- function(df) {
  df <- apply(df, 2, function(x) {
    all(is.na(x))
  })
  df <- data.frame(df)
  df <- df[df$df == TRUE, , drop = FALSE]
  row.names(df)
}


#' Get the number of individuals sampled in SIRENO by trip and haul.
#' @param cod_esp_sireno species sireno code. Only one species is allowed.
#' @return data frame whit required data.
#' @require OAB lengths data set from SIRENO imported by importOABLengths
get_ind_trip_haul_sireno <- function(cod_esp_sireno) {
  ind <- lengths %>%
    filter(COD_ESP %in% cod_esp_sireno) %>%
    select(COD_MAREA, COD_LANCE, TIPO_CAPTURA, EJEM_MEDIDOS) %>%
    group_by(COD_MAREA, COD_LANCE, TIPO_CAPTURA) %>%
    summarise("number_sireno" = sum(EJEM_MEDIDOS, na.rm = TRUE))
  return(as.data.frame(ind))
}


#' Get the number of individuals sampled in SIRENO of certain species.
#' @param cod_esp_sireno species sireno code. Only one species is allowed.
#' @return data frame whit required data.
get_ind_sireno <- function(cod_esp_sireno) {
  ind <- uploaded_lengths %>%
    filter(COD_ESP_CAT %in% cod_esp_sireno) %>%
    select(COD_ID, EJEM_MEDIDOS) %>%
    group_by(COD_ID) %>%
    summarise("number_sireno" = sum(EJEM_MEDIDOS, na.rm = TRUE))
  return(as.data.frame(ind))
}


#' Get the number of individuals sampled in H5 of certain species.
#' @param aphia_id_code species aphia id code. Only one species is allowed.
#' @return data frame whit required data.
get_ind_h5 <- function(aphia_id_code) {
  ind <- h5_minimum %>%
    filter(!is.na(SAspeciesCode)) %>%
    filter(SAspeciesCode == aphia_id_code) %>%
    select(FTunitName, FMnumberAtUnit) %>%
    group_by(FTunitName) %>%
    summarise("number_h5" = sum(FMnumberAtUnit, na.rm = TRUE))
  ind$FTunitName <- as.numeric(ind$FTunitName)
  return(as.data.frame(ind))
}


#' Get the number of individuals sampled in H1 hierarchy by trip and haul.
#' @param aphia_id_code species aphia id code. Only one species is allowed.
#' @return data frame whit required data.
#' @require h1_minimum data set. View rest of the code.
get_ind_trip_haul_h1 <- function(aphia_id_code) {
  ind <- h1_minimum %>%
    filter(!is.na(SAspeciesCode)) %>%
    filter(SAspeciesCode == aphia_id_code) %>%
    select(FTunitName, FOunitName, SAcatchCategory, FMnumberAtUnit) %>%
    mutate("TIPO_CAPTURA" = case_when(SAcatchCategory == "Dis" ~ "D",
                                      SAcatchCategory == "Lan" ~ "C"
    )) %>%
    group_by(FTunitName, FOunitName, SAcatchCategory, TIPO_CAPTURA) %>%
    summarise("number_h1" = sum(FMnumberAtUnit, na.rm = TRUE))
  ind$FOunitName <- as.numeric(ind$FOunitName)
  return(as.data.frame(ind))
}


get_trips_summary_h1 <- function() {
  trips_by_rim_stratum_h1 <- h1_complete %>%
    select(DEstratumName, DEsampled, FTunitName, FOnationalFishingActivity) %>%
    unique() %>%
    group_by(DEstratumName, DEsampled, FOnationalFishingActivity) %>%
    summarise(trips_number_H1 = sum(!is.na(FTunitName))) %>%
    mutate("ESTRATO_RIM" = substr(FOnationalFishingActivity, 4, nchar(FOnationalFishingActivity))) %>%
    select(DEstratumName, DEsampled, ESTRATO_RIM, trips_number_H1)

  trips_by_rim_stratum_sireno <- catches %>%
    select(COD_MAREA, ESTRATO_RIM) %>%
    unique() %>%
    group_by(ESTRATO_RIM) %>%
    summarise(., trips_number_SIRENO = n())

  trips_by_rim_stratum <- merge(trips_by_rim_stratum_h1,
                                trips_by_rim_stratum_sireno,
                                by = "ESTRATO_RIM",
                                all.x = TRUE,
                                all.y = TRUE
  )

  trips_by_rim_stratum <- trips_by_rim_stratum[, c("DEstratumName", "DEsampled",
                                                   "ESTRATO_RIM", "trips_number_SIRENO",
                                                   "trips_number_H1")]

  return(trips_by_rim_stratum)

}


#' param @hauls_cathes_df df result of merge hauls data frame and catches data
#'  frame from the SIRENO reports.
get_hauls_summary_h1 <- function() {

  hauls_by_rim_stratum_h1 <- h1_complete %>%
    select(
      DEstratumName, DEsampled,
      FTunitName,
      FOnationalFishingActivity, FOsequenceNumber, FOvalidity, FOmetier6, FOsampled
    ) %>%
    mutate("ESTRATO_RIM" = substr(FOnationalFishingActivity, 4, length(FOnationalFishingActivity))) %>%
    mutate("VALIDO" = case_when(is.na(FOvalidity) == TRUE ~ FALSE,
                                FOvalidity == "V" ~ TRUE,
                                FOvalidity != "V" ~ FALSE
    )) %>%
    mutate("MUESTREADO" = case_when(is.na(FOsampled) == TRUE ~ FALSE,
                                    FOsampled == "Y" ~ TRUE,
                                    FOsampled == "N" ~ FALSE)) %>%
    unique() %>%
    select(DEstratumName, DEsampled,
           FOmetier6, FOsequenceNumber,
           ESTRATO_RIM, VALIDO, MUESTREADO) %>%
    group_by(
      DEstratumName, DEsampled,
      FOmetier6,
      ESTRATO_RIM, VALIDO, MUESTREADO
    ) %>%
    summarise(number_hauls_H1 = sum(!is.na((FOsequenceNumber))))


  hauls_by_rim_stratum_sireno <- hauls_catches %>%
    select(c(ESTRATO_TECNICO, ESTRATO_RIM, COD_MAREA, COD_LANCE, VALIDO, MUESTREADO)) %>%
    unique() %>%
    group_by(ESTRATO_TECNICO, ESTRATO_RIM, VALIDO, MUESTREADO) %>%
    summarise(number_hauls_SIRENO = length(COD_LANCE))

  hauls_by_rim_stratum <- merge(hauls_by_rim_stratum_sireno,
                                hauls_by_rim_stratum_h1,
                                by.x = c("ESTRATO_TECNICO", "ESTRATO_RIM", "VALIDO", "MUESTREADO"),
                                by.y = c("DEstratumName", "ESTRATO_RIM", "VALIDO", "MUESTREADO"),
                                all.x = TRUE,
                                all.y = TRUE)

  hauls_by_rim_stratum <- hauls_by_rim_stratum[, c("ESTRATO_TECNICO", "DEsampled", "ESTRATO_RIM",
                                                   "FOmetier6", "VALIDO", "MUESTREADO",
                                                   "number_hauls_SIRENO", "number_hauls_H1")]

}

get_ind_by_sp_summary_h1 <- function() {
  ## H1
  ind_by_sp_h1 <- h1_complete %>%
    select(SAspeciesCode, SAcatchCategory, SAsex, FMnumberAtUnit) %>%
    filter(!is.na(FMnumberAtUnit)) %>%
    filter(!is.na(SAspeciesCode)) %>%
    group_by(SAspeciesCode, SAcatchCategory, SAsex) %>%
    summarise(ind_measured_h1 = sum(FMnumberAtUnit, na.rm = TRUE))

  ind_by_sp_h1$SEXO <- ifelse(ind_by_sp_h1$SAsex == "F", "H", ind_by_sp_h1$SAsex)

  ind_by_sp_h1 <- ind_by_sp_h1 %>%
    mutate(TIPO_CAPTURA = case_when(
      SAcatchCategory == "Lan" ~ "C",
      SAcatchCategory == "Dis" ~ "D",
    ))
  ind_by_sp_h1 <- merge(ind_by_sp_h1,
                        sps_rdbes,
                        by.x = "SAspeciesCode",
                        by.y = "APHIAID",
                        all.x = TRUE
  )
  ind_by_sp_h1 <- ind_by_sp_h1[, c("ESPCOD", "SAspeciesCode", "TIPO_CAPTURA", "SEXO", "ind_measured_h1")]
  colnames(ind_by_sp_h1) <- c("ESPCOD", "APHIAID", "TIPO_CAPTURA", "SEXO", "ind_measured_h1")

  ## SIRENO
  # filter not uploaded data
  # Lengths of not valid hauls and not sampled hauls (obviously) are not uploaded
  hauls_simply <- unique(hauls[, c("COD_MAREA", "COD_LANCE", "VALIDO", "MUESTREADO")])
  lengths_valid <- merge(lengths, hauls_simply, all.x = TRUE, all.y = TRUE)
  lengths_valid <- lengths_valid[lengths_valid$VALIDO == TRUE, ]
  lengths_valid <- lengths_valid[lengths_valid$MUESTREADO == TRUE, ]
  # All the lengths are from valid hauls? :O

  # Only is uploaded discards and exclusively retained from BACA_GC
  uploaded_lengths <- lengths_valid %>%
    filter(TIPO_CAPTURA == "D" | (TIPO_CAPTURA == "C" & ESTRATO_RIM == "BACA_GC"))

  # Remove not upload species
  uploaded_lengths <- uploaded_lengths[!(uploaded_lengths$COD_ESP %in% sps_not_upload$ESPCOD), ]

  ind_by_sp_sireno <- uploaded_lengths %>%
    select(COD_ESP, TIPO_CAPTURA, SEXO, EJEM_MEDIDOS) %>%
    group_by(COD_ESP, TIPO_CAPTURA, SEXO) %>%
    summarise(ind_measured_sireno = sum(EJEM_MEDIDOS, na.rm = TRUE)) %>%
    select(COD_ESP, TIPO_CAPTURA, SEXO, ind_measured_sireno)

  ## SUMMARY
  ind_by_sp <- merge(ind_by_sp_h1,
                     ind_by_sp_sireno,
                     by.x = c("ESPCOD", "SEXO", "TIPO_CAPTURA"),
                     by.y = c("COD_ESP", "SEXO", "TIPO_CAPTURA"),
                     all.x = TRUE,
                     all.y = TRUE
  )
  ind_by_sp <- merge(ind_by_sp[, c("ESPCOD", "SEXO", "TIPO_CAPTURA",
                                   "ind_measured_h1", "ind_measured_sireno")],
                     sps_rdbes,
                     by = "ESPCOD",
                     all.x = TRUE)
  ind_by_sp <- merge(ind_by_sp,
                     especies,
                     by.x = "ESPCOD",
                     by.y = "COD_ESP",
                     all.x = TRUE)

  ind_by_sp <- ind_by_sp[, c("ESPCOD","ESPSIRENO","APHIAID",
                             "TIPO_CAPTURA","SEXO", "ind_measured_h1",
                             "ind_measured_sireno")]
  ind_by_sp <- ind_by_sp %>%
    arrange(ESPSIRENO, TIPO_CAPTURA, SEXO)
  ind_by_sp$difference <- ind_by_sp$ind_measured_sireno - ind_by_sp$ind_measured_h1

  return(ind_by_sp)

}

#' get_ind_by_sp_summary_h5 function
#'
#' This function create a summary of the number of individuals by species uploaded
#' to rdbes and compare with the number of individuals stored in SIRENO that would be uploaded.
#' @param h5_df A data frame containing H5 data.
#' @param catches_lengths_df A data frame containing catches lengths data.
#' @return A data frame with columns ESPCOD, APHIAID, SEXO, ind_measured_sireno and ind_measured_h5.
get_ind_by_sp_summary_h5 <- function() {
  ## H5
  ind_by_sp_h5 <- h5_minimum %>%
    select(SAspeciesCode, SAcatchCategory, SAsex, FMnumberAtUnit) %>%
    filter(!is.na(FMnumberAtUnit)) %>%
    filter(!is.na(SAspeciesCode)) %>%
    group_by(SAspeciesCode, SAcatchCategory, SAsex) %>%
    summarise(ind_measured_h5 = sum(FMnumberAtUnit, na.rm = TRUE))

  ind_by_sp_h5$SEXO <- ifelse(ind_by_sp_h5$SAsex == "F", "H", ind_by_sp_h5$SAsex)

  ind_by_sp_h5 <- merge(ind_by_sp_h5,
    sps_rdbes,
    by.x = "SAspeciesCode",
    by.y = "APHIAID",
    all.x = TRUE
  )
  ind_by_sp_h5 <- ind_by_sp_h5[, c("ESPCOD", "SAspeciesCode", "SEXO", "ind_measured_h5")]
  colnames(ind_by_sp_h5) <- c("ESPCOD", "APHIAID", "SEXO", "ind_measured_h5")

  ## SIRENO
  ind_by_sp_sireno <- catches_lengths_uploaded %>%
    select(COD_ESP_CAT, SEXO, EJEM_MEDIDOS) %>%
    group_by(COD_ESP_CAT, SEXO) %>%
    summarise(ind_measured_sireno = sum(EJEM_MEDIDOS, na.rm = TRUE)) %>%
    select(COD_ESP_CAT, SEXO, ind_measured_sireno)

  ## SUMMARY
  ind_by_sp <- merge(ind_by_sp_h5,
    ind_by_sp_sireno,
    by.x = c("ESPCOD", "SEXO"),
    by.y = c("COD_ESP_CAT", "SEXO"),
    all.x = TRUE,
    all.y = TRUE
  )

  ind_by_sp <- merge(ind_by_sp[, c("ESPCOD", "SEXO", "ind_measured_h5", "ind_measured_sireno")],
                     sps_rdbes,
                     by = "ESPCOD",
                     all.x = TRUE)
  ind_by_sp <- merge(ind_by_sp,
                     sapmuebase::especies,
                     by.x = "ESPCOD",
                     by.y = "COD_ESP",
                     all.x = TRUE)

  ind_by_sp <- ind_by_sp[, c("ESPCOD", "ESPSIRENO", "APHIAID", "SEXO",
                             "ind_measured_sireno", "ind_measured_h5")]
  ind_by_sp <- ind_by_sp %>%
    arrange(ESPSIRENO, SEXO)
  ind_by_sp$difference <- ind_by_sp$ind_measured_h5 - ind_by_sp$ind_measured_sireno

  return(ind_by_sp)

}

#' Get the h5 summary of trips uploaded compared with the stored in SIRENO,
#' including sampled and not sampled trips.
get_trips_summary_h5 <- function() {
## RDBES summary ----
  trips_by_rim_stratum_h5 <- h5_minimum %>%
    select(
      DEsamplingScheme, DEyear, DEstratumName,
      LEnationalFishingActivity, LEmetier6,
      FTsampled, FTunitName, ESTRATO_RIM
    ) %>%
    unique() %>%
    group_by(
      DEsamplingScheme, DEyear, DEstratumName,
      LEnationalFishingActivity, LEmetier6, ESTRATO_RIM,
      FTsampled
    ) %>%
    summarise(number_trips_h5 = length(unique(FTunitName)))


  ## SIRENO summary ----
  trips_by_rim_stratum_sireno <- catches_lengths_uploaded %>%
    filter(COD_TIPO_MUE == 2) %>%
    select(c(ESTRATO_TECNICO, ESTRATO_RIM, METIER_DCF, COD_ID)) %>%
    unique() %>%
    group_by(ESTRATO_TECNICO, ESTRATO_RIM, METIER_DCF,) %>%
    summarise(number_trips_sireno = length(unique(COD_ID)))

  ## TOTAL summary ----
  trips_by_rim_stratum <- merge(trips_by_rim_stratum_sireno,
                         trips_by_rim_stratum_h5,
                         by.x = c("ESTRATO_TECNICO", "ESTRATO_RIM", "METIER_DCF"),
                         by.y = c("DEstratumName", "ESTRATO_RIM", "LEmetier6"),
                         all.x = TRUE,
                         all.y = TRUE)
  trips_by_rim_stratum <- trips_by_rim_stratum[, c("DEyear", "DEsamplingScheme", "ESTRATO_TECNICO",
                                     "METIER_DCF", "ESTRATO_RIM",  "FTsampled",
                                     "number_trips_sireno", "number_trips_h5")]
  trips_by_rim_stratum$difference <- trips_by_rim_stratum$number_trips_sireno - trips_by_rim_stratum$number_trips_h5


  return(trips_by_rim_stratum)
}

#' Get the h5 summary of weights uploaded by species
get_weights_by_rim_stratum_summary_h5 <- function() {
  # RDBES weights
  weights_h5 <- h5_minimum %>%
    select(
      FTunitName,
      LEnationalFishingActivity,
      SAspeciesCode,
      SAsex,
      SAtotalWeightLive,
    ) %>%
    unique() %>%
    mutate(ESTRATO_RIM = substr(LEnationalFishingActivity, 4, length(LEnationalFishingActivity))) %>%
    select(ESTRATO_RIM, SAtotalWeightLive)%>%
    group_by(ESTRATO_RIM) %>%
    summarise(total_weight_live_h5 = sum(SAtotalWeightLive, na.rm = TRUE))

  # SIRENO weights
  weights_sireno <- catches_lengths_uploaded %>%
    select(COD_ID, ESTRATO_RIM, COD_ESP_MUE, COD_CATEGORIA, P_VIVO) %>%
    unique() %>%
    select(ESTRATO_RIM, P_VIVO) %>%
    group_by(ESTRATO_RIM) %>%
    summarise(total_weight_live_sireno = (sum(P_VIVO, na.rm = TRUE) * 1000))

  # total weights
  weigths_total <- merge(weights_sireno,
                         weights_h5,
                         all.x = TRUE,
                         all.y = TRUE)
  weigths_total$total_weight_live_sireno_Tm <- weigths_total$total_weight_live_sireno/1000000
  weigths_total$total_weight_live_h5_Tm <- weigths_total$total_weight_live_h5/1000000
  weigths_total$difference_Tm <- weigths_total$total_weight_live_sireno_Tm - weigths_total$total_weight_live_h5_Tm
  weigths_total$difference_Tm <- round(weigths_total$difference_Tm, digits = 1)
  weigths_total <- weigths_total[, c("ESTRATO_RIM", "total_weight_live_sireno_Tm", "total_weight_live_h5_Tm", "difference_Tm")]

}
