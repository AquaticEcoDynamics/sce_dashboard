#' awss3Listfiles 
#'
#' @description Establishes connection to the S3 bucket and fetches data 
#'
#' @return The return value, if any, from executing the function.

#' @noRd


library('aws.s3')

# To enforce HTTPS, should be set to TRUE
Sys.setenv('USE_HTTPS' = TRUE)

# Set details for bucket origin
Sys.setenv(
  'AWS_DEFAULT_REGION' = '', 
  'AWS_S3_ENDPOINT' = 'projects.pawsey.org.au', 
  'AWS_ACCESS_KEY_ID' = '2f1a9d81bdf24a178b2bd18d530e959b', 
  'AWS_SECRET_ACCESS_KEY' = 'e062073c1faf488cb4209ba8de2eb483'
)

bucket = 'scevo-data'

awss3Listfiles <- function(prefix){
  # s3_objects <- aws.s3::get_bucket(bucket = bucket,
  #                                  region = Sys.getenv('AWS_DEFAULT_REGION'), 
  #                                  prefix = prefix,
  #                                  max = Inf)
  s3_objects <- aws.s3::get_bucket(bucket = bucket,
                                   region = '', 
                                   base_url = 'projects.pawsey.org.au',
                                   key = '2f1a9d81bdf24a178b2bd18d530e959b',
                                   secret = 'e062073c1faf488cb4209ba8de2eb483',
                                   prefix = prefix,
                                   max = Inf,
                                   check_region = FALSE,
                                   verbose = TRUE,
                                   show_progress = TRUE,
                                   use_https = TRUE )
  s3_list<- vapply(s3_objects, `[[`, "", "Key", USE.NAMES = FALSE)
  
  # # Now set bucket contents as objects, now for DBCA
  # s3_objects <- aws.s3::get_bucket(bucket = bucket,
  #                                  region = Sys.getenv('AWS_DEFAULT_REGION'), 
  #                                  prefix = 'data-warehouse/dot',
  #                                  max = Inf)
  # s3_list<- vapply(s3_objects, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", s3_list)
  s3_list <- s3_list[!empty]
  s3_list <- grep(".png",s3_list,value = TRUE)
  
  return(s3_list)
  
}

