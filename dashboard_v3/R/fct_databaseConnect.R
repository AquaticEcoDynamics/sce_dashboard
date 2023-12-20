#' databaseConnect 
#'
#' @description Establishes connection to the scevo PostgreSQL database and fetches data for one or more sensor codes
#'
#' @return The return value, if any, from executing the function.
#' @import RPostgreSQL DBI dbplyr dplyr
#' @noRd

databaseConnect <- function(sensorCodes){
  library("RPostgreSQL")
  library("dbplyr")
  driver <- DBI::dbDriver("PostgreSQL")
  connection <- DBI::dbConnect(
    drv = driver,
    host = get_golem_config("host", config = "database"),
    #port= get_golem_config("port", config = "database"), # commented to use the default port
    dbname= get_golem_config("dbname", config = "database"),
    user= get_golem_config("user", config = "database"),
    password= get_golem_config("password", config = "database")
  )

  sensorCodeField <- get_golem_config("sensor_code_field", config = "database_data")
  dataValueField <- get_golem_config("data_value_field", config = "database_data")
  dataDateField <- get_golem_config("data_date_field", config = "database_data")

  fetchedData  <- data.frame(
    #sensorCodeField = integer(),
    #dataValueField = double(),
    #"datetime" = as.POSIXlt(character())
  )
  
  
  # for(i in sensorCodes){
  #   sensorData <- dplyr::tbl(connection, i) %>% as.data.frame()
  #   sensorData  <- sensorData[,c(sensorCodeField,dataValueField,dataDateField)]
  #   sensorData$s_table_name <- i
  #   fetchedData <- rbind(fetchedData, sensorData)
  # }
  
  for (i in sensorCodes) {
    
  #this is to ensure NaN values are fetched as NaN, otherwise it will be fetched as zero 
  custom_query <- glue::glue("
                             SELECT *,
                             nullif(st_value_1, 'NaN') AS st_value_1_new
                             FROM {i}"
                             )
  
  #st_sensor_code AS st_sensor_code,
  #{dataDateField} AS {dataDateField},
  #select nullif(st_value_1, 'NaN') from {i}")
  #'{i}' AS s_table_name
  
  sensorData <- DBI::dbGetQuery(connection, custom_query)
  sensorData <- sensorData[,c(sensorCodeField,dataValueField,dataDateField)]
  sensorData$s_table_name <- i
  fetchedData <- rbind(fetchedData, sensorData)
  }
  
  fetchedData$datetime  <- as.POSIXlt(
    fetchedData$st_feed_date_jdn*86400,
    origin=structure(-210866760000,
                     class=c("POSIXct", "POSIXt"),
                     tzone="Australia/Perth"),
    tz="Australia/Perth"
    )



  DBI::dbDisconnect(connection)

  return(fetchedData)
}

