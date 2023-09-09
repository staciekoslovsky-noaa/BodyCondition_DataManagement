# UAS Body Condition: Read exif from images

## IMPORTANT - SET VARIABLES!!!!
images_folder <- 'D:\\2023\\2023-07-11\\Images_TO_BE_RENAMED\\fl08' ## NEEDS TWO (2) BACKSLASHES BETWEEN FOLDER NAMES
  #'\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\2023\\2023-06-29_X_M30Ttesting\\Images\\DJI_202306291933_002'
export_folder <- 'C:\\smk' ## NEEDS TWO (2) BACKSLASHES BETWEEN FOLDER NAMES
export_fileName <- 'imageExif_m30t_20230629_002.csv'

offset_seconds <- 0



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
install_pkg("tidyverse")
install_pkg("lubridate")
install_pkg("exifr")
install_pkg("stringr")
install_pkg("dplyr")


# Run code -------------------------------------------------------
# Read exif data from images
tags <- c("SourceFile", "FileName", "FileAccessDate", "DateTimeOriginal",
          "GPSLatitude", "GPSLongitude", "GPSAltitude", "RelativeAltitude",
          "FlightYawDegree", "FlightPitchDegree", "FlightRollDegree",
          "GimbalYawDegree", "GimbalPitchDegree", "GimbalRollDegree",
          "LensInfo", "DigitalZoomRatio", "FocalLength", "FocalLengthIn35mmFormat", 
          "ImageSource", "GpsStatus", "AltitudeType", "AbsoluteAltitude", "RelativeAltitude",
          "LRFStatus", "LRFTargetDistance", "LRFTargetLon", "LRFTargetLat", "LRFTargetAlt", "LRFTargetAbsAlt")

images2process <- list.files(images_folder, pattern = "jpg$|JPG$|dng$|DNG$", full.names = TRUE, recursive = FALSE)
images2process <- data.frame(path = images2process, stringsAsFactors = FALSE)
images2process <- images2process %>%
  mutate(image_name = basename(path))

images <- exifr::read_exif(images2process$path, tags = tags)

images <- images %>%
  mutate(image_name = FileName,
         image_path = SourceFile,
         exif_image_dt = lubridate::ymd_hms(DateTimeOriginal, tz = "UTC"),
         exif_latitude = GPSLatitude,
         exif_longitude = GPSLongitude,
         exif_altitude_m = as.numeric(GPSAltitude),
         exif_relative_altitude_m = as.numeric(RelativeAltitude),
         exif_heading = as.numeric(FlightYawDegree),
         exif_pitch = as.numeric(FlightPitchDegree),
         exif_roll = as.numeric(FlightRollDegree),
         exif_gimbal_heading = as.numeric(GimbalYawDegree),
         exif_gimbal_pitch = as.numeric(GimbalPitchDegree),
         exif_gimbal_roll = as.numeric(GimbalRollDegree)) %>%
  rename(lens_info = LensInfo,
         digital_zoom_ratio = DigitalZoomRatio,
         focal_length = FocalLength,
         focal_length_35mm = FocalLengthIn35mmFormat,
         image_source = ImageSource,
         gps_status = GpsStatus,
         altitude_type = AltitudeType,
         absolute_altitude = AbsoluteAltitude,
         relative_altitude = RelativeAltitude,
         lrf_status = LRFStatus,
         lrf_target_distance_m = LRFTargetDistance,
         lrf_target_latitude = LRFTargetLat,
         lrf_target_longitude = LRFTargetLon,
         lrf_target_altitude = LRFTargetAlt,
         lrf_target_absolute_altitude = LRFTargetAbsAlt) %>%
  mutate(adjusted_image_dt = exif_image_dt + offset_seconds) %>%
  select(image_name, image_path,
         image_source, lens_info, digital_zoom_ratio, focal_length, focal_length_35mm,
         exif_image_dt, adjusted_image_dt, exif_latitude, exif_longitude, exif_altitude_m, exif_relative_altitude_m,
         exif_heading, exif_pitch, exif_roll,
         exif_gimbal_heading, exif_gimbal_pitch, exif_gimbal_roll,
         gps_status, altitude_type, absolute_altitude, relative_altitude,
         lrf_status, lrf_target_distance_m, lrf_target_latitude, lrf_target_longitude, lrf_target_altitude, lrf_target_absolute_altitude)

write.csv(images, paste(export_folder, export_fileName, sep = "\\"), row.names = FALSE)
