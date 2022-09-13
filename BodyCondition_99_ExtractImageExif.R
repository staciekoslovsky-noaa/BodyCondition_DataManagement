# UAS Body Condition: Read exif from images

## IMPORTANT - SET VARIABLES!!!!
images_folder <- '\\\\nmfs\\akc-nmml\\Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\2022\\2022-07-29_H33FlightTest\\M2EA_images' ## NEEDS TWO (2) BACKSLASHES BETWEEN FOLDER NAMES
export_folder <- 'C:\\skh' ## NEEDS TWO (2) BACKSLASHES BETWEEN FOLDER NAMES
export_fileName <- 'imageExif_H33FlightTest_M2EA.csv'

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
          "LensInfo", "DigitalZoomRatio")

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
  mutate(adjusted_image_dt = exif_image_dt + offset_seconds) %>%
  select(image_name, image_path,
         exif_image_dt, adjusted_image_dt, exif_latitude, exif_longitude, exif_altitude_m, exif_relative_altitude_m,
         exif_heading, exif_pitch, exif_roll,
         exif_gimbal_heading, exif_gimbal_pitch, exif_gimbal_roll)

write.csv(images, paste(export_folder, export_fileName, sep = "\\"), row.names = FALSE)
