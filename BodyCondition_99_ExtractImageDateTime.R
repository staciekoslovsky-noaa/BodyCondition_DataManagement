# UAS Body Condition: Extract date/time from drone images
# S. Hardy

# Set Working Variables
wd <- "\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\2022\\2022-06-01_X_GPS_dronetesting\\Images"

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

# Run code -------------------------------------------------------
setwd(wd)

tags <- c("SourceFile", "FileName", "DateTimeOriginal")

# Get list of images
images <- list.files(wd, pattern = "jpg$|JPG$|dng$|DNG$", full.names = TRUE, recursive = FALSE)
images <- data.frame(path = images, stringsAsFactors = FALSE)
    
exif <- exifr::read_exif("\\\\akc0ss-n086\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Data\\test_exif_DO_NOT_DELETE.JPG", tags = tags)
exif <- data.frame(SourceFile = exif[0, c(1:2)], stringsAsFactors = FALSE)
      
exif <- exifr::read_exif(images$path, tags = tags)
exif <- data.frame(lapply(exif, as.character), stringsAsFactors = FALSE)
      
exif <- exif %>%
  rename(image_name = FileName,
         image_path = SourceFile) %>%
  mutate(exif_image_dt = format(lubridate::ymd_hms(DateTimeOriginal, tz = "UTC"), tz = "UTC")) %>%
  select(image_path, image_name, exif_image_dt)

write.csv(exif, "C:\\skh\\image_exif.csv", row.names = FALSE)
