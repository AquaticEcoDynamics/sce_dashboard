## code to prepare `sensorInfo` dataset goes here
sensorInfo <- read.csv("inst/extdata/sensorInfo_old_2.csv")
colnames(sensorInfo)[1] <- "group"
colnames(sensorInfo)[2] <- "subGroup"
colnames(sensorInfo)[6] <- "colour"
colnames(sensorInfo)[3] <- "sensorCode"
usethis::use_data(sensorInfo, overwrite = TRUE, internal = TRUE)


