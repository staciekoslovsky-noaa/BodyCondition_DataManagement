# UAS Body Condition: Import LRF data to DB
# S. Koslovsky

# Set Working Variables
wd <- "Y:\\NMML_Polar\\Data\\UAS\\UAS_BodyCondition\\Measurements"
years <- c(2021, 2022, 2023)

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
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

images_targets <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM body_condition.summ4import_measurements")
imported <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT measurement_file_name FROM body_condition.tbl_measurements_nadir")

failed_to_import <- list("")

for (y in 1:length(years)) {
  # Create list of camera folders within which data need to be processed 
  dir <- list.dirs(paste(wd, years[y], sep = "/"), full.names = TRUE, recursive = TRUE)
  dir <- data.frame(path = dir[grep("fl[0-9][0-9]_[0-9][0-9][0-9][0-9]$", dir)], stringsAsFactors = FALSE) 
  
  for (i in 1:nrow(dir)){
    if(!dir.exists(dir$path[i])) next 
    image_name <- paste0(basename((dir$path[i])), ".JPG")
    
    files <- list.files(dir$path[i], full.names = TRUE, recursive = FALSE)
    files <- data.frame(path = files[grep("csv", files)], stringsAsFactors = FALSE)
    files <- files %>%
      mutate(file_name = basename(path))
    
    for (j in 1:nrow(files)){
      # Check if file already imported
      if(files$file_name[j] %in% imported$measurement_file_name) next
      
      processed_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM body_condition.tbl_measurements_nadir WHERE id < 9000000")
      processed_id$max <- ifelse(is.na(processed_id$max), 0, processed_id$max)
      
      if(grepl("lengths-widths", files$file_name[j], fixed = TRUE) == TRUE) {
        lw <- read.table(files$path[j], sep = ",", col.names = c("image_id",
                                                                 "image_path",
                                                                 "focal_length",
                                                                 "altitude",
                                                                 "pixel_dimension",
                                                                 "type",
                                                                 "length_name",
                                                                 "length_px",
                                                                 "length_m",
                                                                 "W_090pct_px",
                                                                 "W_181pct_px",
                                                                 "W_272pct_px",
                                                                 "W_363pct_px",
                                                                 "W_454pct_px",
                                                                 "W_545pct_px",
                                                                 "W_636pct_px",
                                                                 "W_727pct_px",
                                                                 "W_818pct_px",
                                                                 "W_909pct_px",
                                                                 "W_090pct_m",
                                                                 "W_181pct_m",
                                                                 "W_272pct_m",
                                                                 "W_363pct_m",
                                                                 "W_454pct_m",
                                                                 "W_545pct_m",
                                                                 "W_636pct_m",
                                                                 "W_727pct_m",
                                                                 "W_818pct_m",
                                                                 "W_909pct_m"), 
                         skip = 1, header = FALSE, stringsAsFactors = FALSE, blank.lines.skip = TRUE, colClasses = "character", dec = ".", fill = TRUE, strip.white = TRUE) %>%
          mutate(measurement_file_name = files$file_name[j]) %>%
          mutate(length_name = toupper(length_name))
        
        temp_lw <- lw %>%
          select(length_name, length_px, measurement_file_name)
        
        temp_tl <- lw %>%
          filter(length_name == "TL") %>%
          select(measurement_file_name, W_090pct_px, W_181pct_px, W_272pct_px, W_363pct_px, W_454pct_px, W_545pct_px, W_636pct_px, W_727pct_px, W_818pct_px, W_909pct_px) %>%
          pivot_longer(!measurement_file_name, names_to = "length_name") %>%
          rename(length_px = value) %>%
          select(length_name, length_px, measurement_file_name) %>%
          mutate(length_name = ifelse(length_name == "W_090pct_px", "W01", length_name)) %>% 
          mutate(length_name = ifelse(length_name == "W_181pct_px", "W02", length_name)) %>% 
          mutate(length_name = ifelse(length_name == "W_272pct_px", "W03", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_363pct_px", "W04", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_454pct_px", "W05", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_545pct_px", "W06", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_636pct_px", "W07", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_727pct_px", "W08", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_818pct_px", "W09", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_909pct_px", "W10", length_name))
        
        temp_sl <- lw %>%
          filter(length_name == "SL") %>%
          select(measurement_file_name, W_090pct_px, W_181pct_px, W_272pct_px, W_363pct_px, W_454pct_px, W_545pct_px, W_636pct_px, W_727pct_px, W_818pct_px, W_909pct_px) %>%
          pivot_longer(!measurement_file_name, names_to = "length_name") %>%
          rename(length_px = value) %>%
          select(length_name, length_px, measurement_file_name) %>%
          mutate(length_name = ifelse(length_name == "W_090pct_px", "S01", length_name)) %>% 
          mutate(length_name = ifelse(length_name == "W_181pct_px", "S02", length_name)) %>% 
          mutate(length_name = ifelse(length_name == "W_272pct_px", "S03", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_363pct_px", "S04", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_454pct_px", "S05", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_545pct_px", "S06", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_636pct_px", "S07", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_727pct_px", "S08", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_818pct_px", "S09", length_name)) %>%  
          mutate(length_name = ifelse(length_name == "W_909pct_px", "S10", length_name)) 
        
        toDB <- temp_lw %>%
          rbind(temp_tl) %>%
          rbind(temp_sl) %>%
          mutate(id =  1:n() + processed_id$max,
                 image_name = image_name) %>%
          left_join(images_targets, by = "image_name") %>%
          filter(!is.na(target_id)) %>%
          rename(measurement_type_lku = length_name,
                 pixels_counted = length_px) %>%
          mutate(measured_by = gsub("_", "", str_extract(measurement_file_name, "_[A-Z][A-Z][A-Z]_")),
                 measurement_date = as.Date(sapply(strsplit(measurement_file_name, "_"), function(x) x[6]), format = "%Y%m%d")) %>%
          select(id, target_id, measurement_type_lku, pixels_counted, measured_by, measurement_date, image_id, target_posture_lku, measurement_file_name) %>%
          mutate(pixels_counted = ifelse(is.nan(pixels_counted) | pixels_counted == '', -99, pixels_counted))
      }
      
      if(grepl("areas", files$file_name[j], fixed = TRUE) == TRUE) {
        toDB <- read.table(files$path[j], sep = ",", col.names = c("image_id",
                                                                 "image_path",
                                                                 "focal_length",
                                                                 "altitude",
                                                                 "pixel_dimension",
                                                                 "type",
                                                                 "area_name",
                                                                 "area_px",
                                                                 "area_m"), 
                         skip = 1, header = FALSE, stringsAsFactors = FALSE, blank.lines.skip = TRUE, colClasses = "character", dec = ".", fill = TRUE, strip.white = TRUE) %>%
          select(-image_id) %>%
          mutate(measurement_file_name = files$file_name[j]) %>%
          mutate(id =  1:n() + processed_id$max,
                 image_name = image_name) %>%
          left_join(images_targets, by = "image_name") %>%
          filter(!is.na(target_id)) %>%
          rename(measurement_type_lku = area_name,
                 pixels_counted = area_px) %>%
          mutate(measured_by = gsub("_", "", str_extract(measurement_file_name, "_[A-Z][A-Z][A-Z]_")),
                 measurement_date = as.Date(sapply(strsplit(measurement_file_name, "_"), function(x) x[6]), format = "%Y%m%d")) %>%
          select(id, target_id, measurement_type_lku, pixels_counted, measured_by, measurement_date, image_id, target_posture_lku, measurement_file_name) %>%
          mutate(pixels_counted = ifelse(is.nan(pixels_counted), -99, pixels_counted))
      }
      
      if(unique(is.na(toDB$pixels_counted)) == TRUE) {
        failed_to_import <- append(basename(files$path[j]), failed_to_import)
      } else {
        # Write data to the DB
        RPostgreSQL::dbWriteTable(con, c("body_condition", "tbl_measurements_nadir"), toDB, append = TRUE, row.names = FALSE)
      }
    }
  }
}

RPostgreSQL::dbDisconnect(con)
rm(con)