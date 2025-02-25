# Rename UAS body condition images 

# Set variables -------------------
wd <- "C:/smk/"
date_folder <- "2024-04-25_X_M30T_NotReady4Import"
image_prefix <- "norse"

#process <- "move+rename"
process <- "rename_only" # keeps the images in the fl## folder -- will likely be rarely used as an option

# NEED C:\\Strawberry installed before running

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

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Install libraries ----------------------------------------------
install_pkg("exifr")
install_pkg("tidyverse")
install_pkg("stringr")

# Rename images -------------------
path <- paste(wd, date_folder, "Images", sep = "//")
setwd(path)
      
flights <- list.dirs(path, full.names = FALSE, 
                     recursive = FALSE) # use most of the time
                     # recursive = TRUE)  # use when images are split into camera folders within flight
flights <- flights[grep("fl", flights)]

tags <- c("SourceFile", "FileName", "DateTimeOriginal", "Model", "LRFStatus", "LRFTargetDistance", "LRFTargetLon", "LRFTargetLat", "LRFTargetAlt", "LRFTargetAbsAlt")

exif <- exifr::read_exif(paste(wd, "test_exif_DO_NOT_DELETE.JPG", sep = ""), tags = tags)
exif$flight <- ''
exif <- data.frame(exif[0, c(1:5)], stringsAsFactors = FALSE)

for (i in 1:length(flights)){
  images <- list.files(flights[i], pattern = "jpg$|JPG$|dng$|DNG$", full.names = TRUE, recursive = TRUE)
  temp_exif <- exifr::read_exif(images, tags = tags) %>%
    mutate(flight = flights[i])
  exif <- bind_rows(exif, temp_exif)
  rm(temp_exif)
}

exif <- exif %>%
  arrange(DateTimeOriginal) %>%
  mutate(image_num = 1,
         file_ext = tools::file_ext(FileName),
         image_name_num = gsub("_", "", str_extract(FileName, "_[0-9][0-9][0-9][0-9]_")),
         image_name_type = gsub(".JPG", "", gsub("_", "", str_extract(FileName, "_[A-Z].JPG"))))

for (i in 2:nrow(exif)) {
  exif$image_num[i] <- ifelse(exif$image_name_num[i] == exif$image_name_num[i-1], exif$image_num[i-1], exif$image_num[i-1] + 1)
}

exif <- exif %>%
  mutate(NewName = paste(image_prefix, "_", str_replace(str_replace(substr(date_folder, 1, 10), "-", ""), "-", ""), "_", flight, 
                         "_", sprintf("%04d", image_num), "_", image_name_type, ".", file_ext, 
                         sep = "")) %>%
  select(SourceFile, FileName, NewName, flight)

for (i in 1:nrow(exif)){
  if (process == "move+rename") {
    file.rename(exif$SourceFile[i], exif$NewName[i])
  } else if (process == "rename_only") {
    file.rename(exif$SourceFile[i], paste(exif$flight[i], exif$NewName[i], sep = "//"))
  }
}

write.table(exif, paste(path, "\\_RenamedImages_From", date_folder, ".csv", sep = ""), sep = ",", row.names = FALSE)