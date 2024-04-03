
h1 <- import_hierarchy_file(file.path(getwd(), "data"), "IEORDBES_H1_ICES_MARCO_H1.csv")
h1 <- h1[, c("V1", "V2", "V3")]


# h1_david <- h1_complete[, c("DEid", "SDid", "VSid", "FTid", "FOid", "SSid", "SAid", "FMid")]

h1$DEid <- cumsum(h1$V1 == "DE")
h1$SDid <- cumsum(h1$V1 == "SD")
h1[h1$V1 == "DE", "SDid"] <- NA
h1$VSid <- cumsum(h1$V1 == "VS")
h1[h1$V1 %in% c("DE", "SD"), "VSid"] <- NA
h1$FTid <- cumsum(h1$V1 == "FT")
h1[h1$V1 %in% c("DE", "SD", "VS"), "FTid"] <- NA
h1$FOid <- cumsum(h1$V1 == "FO")
h1[h1$V1 %in% c("DE", "SD", "VS", "FT"), "FOid"] <- NA
h1$SSid <- cumsum(h1$V1 == "SS")
h1[h1$V1 %in% c("DE", "SD", "VS", "FT", "FO"), "SSid"] <- NA
h1$SAid <- cumsum(h1$V1 == "SA")
h1[h1$V1 %in% c("DE", "SD", "VS", "FT", "FO", "SS"), "SAid"] <- NA
h1$FMid <- cumsum(h1$V1 == "FM")
h1[h1$V1 %in% c("DE", "SD", "VS", "FT", "FO", "SS", "SA"), "FMid"] <- NA

# TODO: test if this works properly and then split de h1 and add column names.
# In case it works, factorize it.