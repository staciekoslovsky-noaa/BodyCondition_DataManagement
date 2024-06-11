# UAS Body Condition: Import LRF data to DB
# S. Hardy

# Set Working Variables
wd <- "O:\\Data\\UAS\\UAS_BodyCondition\\Data"
years <- c(#2021, 
  #2022,
  #2023,
  2024)

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

# Run code -------------------------------------------------------
setwd(wd)
"%notin%" <- Negate("%in%")
options(digits.secs=3)

# Get list of already imported data from DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              #port = Sys.getenv("pep_port"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))
                              #rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# RPostgreSQL::dbSendQuery(con, "DELETE FROM body_condition.geo_lrf_feed")

imported <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT lrf_file_name FROM body_condition.geo_lrf_feed")

for (y in 1:length(years)) {
  # Create list of camera folders within which data need to be processed 
  dir <- list.dirs(paste(wd, years[y], sep = "/"), full.names = TRUE, recursive = FALSE)
  dir <- data.frame(path = dir[grep("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", dir)], stringsAsFactors = FALSE) 
  dir <- dir %>%
    mutate(path = paste(path, 'LRF_data', sep = "/"))
  
  for (i in 1:nrow(dir)){
    if(!dir.exists(dir$path[i])) next 
    files <- list.files(dir$path[i], full.names = TRUE, recursive = FALSE)
    files <- data.frame(path = files[grep("csv", files)], stringsAsFactors = FALSE)
    files <- files %>%
      mutate(file_name = basename(path))
    
    for (j in 1:nrow(files)){
      # Check if file already imported
      if(files$file_name[j] %in% imported$lrf_file_name) next
      
      processed_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM body_condition.geo_lrf_feed")
      processed_id$max <- ifelse(is.na(processed_id$max), 0, processed_id$max)
      
      # Process and import data
      
      
      if (years[y] == 2021) {
        lrf <- read.table(files$path[j], sep = ",", col.names = c("utc_gps_time", "utc_gps_date", "laser_range_raw_m", "laser_range_median", "laser_range_first_median", "laser_range_last_median", 
                                                                  "laser_signal_strength_first", "laser_signal_strength_last", "laser_background_noise", "laser_lost_signal_confirmation", 
                                                                  "gps_lat", "gps_lat_ns", "gps_lon", "gps_lon_ew", "gps_speed", "gps_track_angle", "gps_magvar", "test01", "test02", "test03"), 
                          skip = 1, header = FALSE, stringsAsFactors = FALSE, blank.lines.skip = TRUE, colClasses = "character", dec = ".", fill = TRUE, strip.white = TRUE)
        lrf <- lrf %>%
          filter(laser_range_raw_m != "-1.00") %>% 
          filter(utc_gps_date != '//') %>%
          filter(gps_lat != "") %>%
          filter(nchar(gps_lat_ns) == 1) %>%
          filter(gps_lon != "") %>%
          filter(nchar(gps_lon_ew) == 1) %>%
          filter(gps_magvar != "$GPRMC") %>%
          mutate(id = 1:n() + processed_id$max,
                 lrf_file_name = tolower(files$file_name[j]),
                 gps_dt = ymd_hms(as.POSIXct(paste(utc_gps_date, utc_gps_time, sep = " "), format = "%m/%d/%y %H:%M:%OS", tz = "UTC"), tz = "UTC"),
                 laser_range_raw_m = as.numeric(laser_range_raw_m),
                 laser_range_median = as.numeric(laser_range_median),
                 gps_latitude = round(as.numeric(stringr::str_sub(gps_lat, 1, 2)) + as.numeric(stringr::str_sub(gps_lat, -nchar(gps_lat) + 2, -1))/60, 9),
                 gps_longitude = round(as.numeric(ifelse(gps_lon_ew == "W", 
                                                         -(as.numeric(stringr::str_sub(gps_lon, 1, 3)) + as.numeric(stringr::str_sub(gps_lon, -nchar(gps_lon) + 3, -1))/60),
                                                         as.numeric(stringr::str_sub(gps_lon, 1, 3)) + as.numeric(stringr::str_sub(gps_lon, -nchar(gps_lon) + 3, -1))/60)), 9),
                 qa_status_lku = 'U',
                 geom = "0101000020E610000000000000000000000000000000000000") %>%
          # Set numeric NA values to -99
          mutate(laser_range_raw_m = ifelse(is.na(laser_range_raw_m), -99, laser_range_raw_m),
                 laser_range_median = ifelse(is.na(laser_range_median), -99, laser_range_median),
                 gps_speed = ifelse(is.na(gps_speed), -99, gps_speed)) %>%
          select(id, lrf_file_name, gps_dt, laser_range_raw_m, laser_range_median, gps_latitude, gps_longitude, gps_speed, qa_status_lku, geom) %>%
          filter(!is.na(gps_dt))
      } else {
        lrf <- read.table(files$path[j], sep = ",", col.names = c("utc_gps_time", "utc_gps_date", "laser_range_raw_m", "laser_range_median", 
                                                                  "gps_lat", "gps_lat_ns", "gps_lon", "gps_lon_ew", "gps_speed", 
                                                                  "imu_pitch", "imu_roll",
                                                                  "raw_gyro_x",	"raw_gyro_y",	"raw_gyro_z",	"raw_accel_x",	"raw_accel_y",	"raw_accel_z",
                                                                  "test01", "test02", "test03"), 
                          skip = 1, header = FALSE, stringsAsFactors = FALSE, blank.lines.skip = TRUE, colClasses = "character", dec = ".", fill = TRUE, strip.white = TRUE)
        lrf <- lrf %>%
          filter(laser_range_raw_m != "-1.00") %>% 
          filter(utc_gps_date != '//') %>%
          #filter(gps_lat != "") %>%
          #filter(nchar(gps_lat_ns) == 1) %>%
          #filter(gps_lon != "") %>%
          #filter(nchar(gps_lon_ew) == 1) %>%
          mutate(id = 1:n() + processed_id$max,
                 lrf_file_name = files$file_name[j],
                 gps_dt = ymd_hms(as.POSIXct(paste(utc_gps_date, utc_gps_time, sep = " "), format = "%m/%d/%y %H:%M:%OS", tz = "UTC"), tz = "UTC"),
                 laser_range_raw_m = as.numeric(laser_range_raw_m),
                 laser_range_median = as.numeric(laser_range_median),
                 gps_latitude = round(as.numeric(stringr::str_sub(gps_lat, 1, 2)) + as.numeric(stringr::str_sub(gps_lat, -nchar(gps_lat) + 2, -1))/60, 9),
                 gps_longitude = round(as.numeric(ifelse(gps_lon_ew == "W", 
                                                         -(as.numeric(stringr::str_sub(gps_lon, 1, 3)) + as.numeric(stringr::str_sub(gps_lon, -nchar(gps_lon) + 3, -1))/60),
                                                         as.numeric(stringr::str_sub(gps_lon, 1, 3)) + as.numeric(stringr::str_sub(gps_lon, -nchar(gps_lon) + 3, -1))/60)), 9),
                 gps_speed = as.numeric(gps_speed),
                 imu_pitch = round(as.numeric(imu_pitch), 9),
                 imu_roll = round(as.numeric(imu_roll), 9),
                 raw_gyro_x = round(as.numeric(raw_gyro_x), 9),
                 raw_gyro_y = round(as.numeric(raw_gyro_y), 9),
                 raw_gyro_z = round(as.numeric(raw_gyro_z), 9),
                 raw_accel_x = round(as.numeric(raw_accel_x), 9),
                 raw_accel_y = round(as.numeric(raw_accel_y), 9),
                 raw_accel_z = round(as.numeric(raw_accel_z), 9),
                 qa_status_lku = 'U',
                 geom = "0101000020E610000000000000000000000000000000000000") %>%
          # Set numeric NA values to -99
          mutate(laser_range_raw_m = ifelse(is.na(laser_range_raw_m), -99, laser_range_raw_m),
                 laser_range_median = ifelse(is.na(laser_range_median), -99, laser_range_median),
                 gps_latitude = ifelse(is.na(gps_latitude), -99, gps_latitude),
                 gps_longitude = ifelse(is.na(gps_longitude), -99, gps_longitude),
                 gps_speed = ifelse(is.na(gps_speed), -99, gps_speed),
                 imu_pitch = ifelse(is.na(imu_pitch), -99, imu_pitch),
                 imu_roll = ifelse(is.na(imu_roll), -99, imu_roll),
                 raw_gyro_x = ifelse(is.na(raw_gyro_x), -99, raw_gyro_x),
                 raw_gyro_y = ifelse(is.na(raw_gyro_y), -99, raw_gyro_y),
                 raw_gyro_z = ifelse(is.na(raw_gyro_z), -99, raw_gyro_z),
                 raw_accel_x = ifelse(is.na(raw_accel_x), -99, raw_accel_x),
                 raw_accel_y = ifelse(is.na(raw_accel_y), -99, raw_accel_y),
                 raw_accel_z = ifelse(is.na(raw_accel_z), -99, raw_accel_z),) %>%
          select(id, lrf_file_name, gps_dt, laser_range_raw_m, laser_range_median, gps_latitude, gps_longitude, gps_speed, imu_pitch, imu_roll, 
                 raw_gyro_x, raw_gyro_y, raw_gyro_z, raw_accel_x, raw_accel_y, raw_accel_z,
                 qa_status_lku, geom) %>%
          filter(!is.na(gps_dt))
      }
      # Write data to the DB
      RPostgreSQL::dbWriteTable(con, c("body_condition", "geo_lrf_feed"), lrf, append = TRUE, row.names = FALSE)
    }
  }
}

dbSendQuery(con, "UPDATE body_condition.geo_lrf_feed SET geom = ST_SetSRID(ST_MakePoint(gps_longitude, gps_latitude), 4326)")

RPostgreSQL::dbDisconnect(con)
rm(con)
