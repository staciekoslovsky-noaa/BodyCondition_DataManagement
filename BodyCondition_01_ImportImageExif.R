# UAS Body Condition: Import LRF data to DB
# S. Koslovsky

# Set Working Variables
wd <- "\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data"
years <- c(#2021, 
  #2022,
  2023,
  2024
  )

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")
install_pkg("lubridate")
install_pkg("exifr")

# Run code -------------------------------------------------------
setwd(wd)

tags_m2ea_m2p <- c("SourceFile", "FileName", "FileAccessDate", "DateTimeOriginal",
          "GPSLatitude", "GPSLongitude", "GPSAltitude", "RelativeAltitude",
          "FlightYawDegree", "FlightPitchDegree", "FlightRollDegree",
          "GimbalYawDegree", "GimbalPitchDegree", "GimbalRollDegree",
          "LensInfo", "DigitalZoomRatio")

tags_m30t <- c("SourceFile", "FileName", "FileAccessDate", "DateTimeOriginal",
               "GPSLatitude", "GPSLongitude", "GPSAltitude", "RelativeAltitude",
               "FlightYawDegree", "FlightPitchDegree", "FlightRollDegree",
               "GimbalYawDegree", "GimbalPitchDegree", "GimbalRollDegree",
               "LensInfo", "DigitalZoomRatio", "LRFStatus", "LRFTargetDistance", 
               "LRFTargetLon", "LRFTargetLat", "LRFTargetAlt", "LRFTargetAbsAlt", 
               "FocalLength", "FocalLengthIn35mmFormat")

# Get list of already imported data from DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))


# RPostgreSQL::dbSendQuery(con, "DELETE FROM body_condition.geo_images")

imported <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT image_name, \'y\' as in_database FROM body_condition.geo_images")

for (y in 1:length(years)) {
  # Create list of camera folders within which data need to be processed 
  dir <- list.dirs(paste(wd, years[y], sep = "/"), full.names = TRUE, recursive = FALSE)
  dir <- data.frame(path = dir[grep("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[A-z][0-9]", dir)], stringsAsFactors = FALSE) 
  dir <- dir %>%
    mutate(drone = gsub("^.*_", "", path)) %>%
    mutate(path = paste(path, 'Images', sep = "/"))
    
  for (i in 1:nrow(dir)){
    # Process if folder exists
    if(!dir.exists(dir$path[i])) next 
    
    # Get next ID for images data
    processed_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM body_condition.geo_images")
    processed_id$max <- ifelse(is.na(processed_id$max), 0, processed_id$max)
    
    # Get list of images, join to imported data, and identify images that have not been imported to the DB
    images <- list.files(dir$path[i], pattern = "jpg$|JPG$|dng$|DNG$", full.names = TRUE, recursive = FALSE)
    images <- data.frame(path = images, stringsAsFactors = FALSE)
    images <- images %>%
      mutate(image_name = basename(path)) %>%
      left_join(imported, by = "image_name") %>%
      filter(is.na(in_database))
    
    drone <- dir$drone[i]
    
    # Process and import data if any images remain in list
    if (nrow(images) > 0) {
      if (drone == "M30T") {
        tags <- tags_m30t
        original_exif <- exifr::read_exif("\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\test_exif_DO_NOT_DELETE_M30T.JPG", tags = tags)
      } else {
        tags <- tags_m2ea_m2p
        original_exif <- exifr::read_exif("\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\test_exif_DO_NOT_DELETE_M2EA.JPG", tags = tags)
      }
      
      original_exif <- data.frame(SourceFile = original_exif[0, c(1:2)], stringsAsFactors = FALSE)
      
      temp_exif <- exifr::read_exif(images$path, tags = tags)
      temp_exif <- data.frame(lapply(temp_exif, as.character), stringsAsFactors = FALSE)
      
      # Check for missing columns
      missing <- setdiff(tags, names(temp_exif)) 
      temp_exif[missing] <- ''                   
      temp_exif <- temp_exif[tags]
      
      # Merge with other exif data
      original_exif <- bind_rows(original_exif, temp_exif)
      rm(temp_exif)
      
      if(drone == "M30T"){
        original_exif <- original_exif %>%
          mutate(id = 1:n() + processed_id$max,
                 image_name = FileName,
                 image_path = SourceFile,
                 exif_image_dt = format(lubridate::ymd_hms(DateTimeOriginal, tz = "UTC"), tz = "UTC"),
                 exif_latitude = GPSLatitude,
                 exif_longitude = GPSLongitude,
                 exif_altitude_m = as.numeric(GPSAltitude),
                 exif_heading = as.numeric(FlightYawDegree),
                 exif_pitch = as.numeric(FlightPitchDegree),
                 exif_roll = as.numeric(FlightRollDegree),
                 exif_gimbal_heading = as.numeric(GimbalYawDegree),
                 exif_gimbal_pitch = as.numeric(GimbalPitchDegree),
                 exif_gimbal_roll = as.numeric(GimbalRollDegree),
                 exif_lens = LensInfo,
                 exif_zoom_factor = as.numeric(DigitalZoomRatio),
                 use_image_for_lku = 'XX',
                 measurement_status_lku = 'Q',
                 exif_relative_altitude_m = RelativeAltitude,
                 file_access_dt = format(lubridate::ymd_hms(FileAccessDate, tz = "America/Vancouver"), tz = "UTC"),
                 geom = "0101000020E610000000000000000000000000000000000000",
                 lrf_status = LRFStatus, 
                 lrf_target_dist = LRFTargetDistance, 
                 lrf_target_long = LRFTargetLon, 
                 lrf_target_lat = LRFTargetLat, 
                 lrf_target_alt = LRFTargetAlt, 
                 lrf_target_alt_abs = LRFTargetAbsAlt,
                 focal_length = FocalLength,
                 focal_length_35mm = FocalLengthIn35mmFormat) %>%
          select(id, image_name, image_path,
                 exif_image_dt, exif_latitude, exif_longitude, exif_altitude_m,
                 exif_heading, exif_pitch, exif_roll,
                 exif_gimbal_heading, exif_gimbal_pitch, exif_gimbal_roll,
                 exif_lens, exif_zoom_factor,
                 use_image_for_lku, measurement_status_lku, 
                 exif_relative_altitude_m, file_access_dt, geom,
                 lrf_status, lrf_target_dist, lrf_target_long, lrf_target_lat, lrf_target_alt, lrf_target_alt_abs,
                 focal_length, focal_length_35mm)
      } else {
        original_exif <- original_exif %>%
          mutate(id = 1:n() + processed_id$max,
                 image_name = FileName,
                 image_path = SourceFile,
                 exif_image_dt = format(lubridate::ymd_hms(DateTimeOriginal, tz = "UTC"), tz = "UTC"),
                 exif_latitude = GPSLatitude,
                 exif_longitude = GPSLongitude,
                 exif_altitude_m = as.numeric(GPSAltitude),
                 exif_heading = as.numeric(FlightYawDegree),
                 exif_pitch = as.numeric(FlightPitchDegree),
                 exif_roll = as.numeric(FlightRollDegree),
                 exif_gimbal_heading = as.numeric(GimbalYawDegree),
                 exif_gimbal_pitch = as.numeric(GimbalPitchDegree),
                 exif_gimbal_roll = as.numeric(GimbalRollDegree),
                 exif_lens = LensInfo,
                 exif_zoom_factor = as.numeric(DigitalZoomRatio),
                 use_image_for_lku = 'XX',
                 measurement_status_lku = 'Q',
                 exif_relative_altitude_m = RelativeAltitude,
                 file_access_dt = format(lubridate::ymd_hms(FileAccessDate, tz = "America/Vancouver"), tz = "UTC"),
                 geom = "0101000020E610000000000000000000000000000000000000") %>%
          select(id, image_name, image_path,
                 exif_image_dt, exif_latitude, exif_longitude, exif_altitude_m,
                 exif_heading, exif_pitch, exif_roll,
                 exif_gimbal_heading, exif_gimbal_pitch, exif_gimbal_roll,
                 exif_lens, exif_zoom_factor,
                 use_image_for_lku, measurement_status_lku, 
                 exif_relative_altitude_m, file_access_dt, geom)
      }
      
      # Write data to the DB
      RPostgreSQL::dbWriteTable(con, c("body_condition", "geo_images"), original_exif, append = TRUE, row.names = FALSE)
    }
  }
}

# Update geom field
RPostgreSQL::dbSendQuery(con, "UPDATE body_condition.geo_images SET geom = ST_SetSRID(ST_MakePoint(exif_longitude, exif_latitude), 4326)")

# Update flight_id field
####### ADD CODE HERE ONCE tbl_flight populated!!!!!

RPostgreSQL::dbDisconnect(con)
rm(con)
