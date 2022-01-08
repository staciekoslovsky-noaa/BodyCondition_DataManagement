# UAS Body Condition: Import LRF data to DB
# S. Hardy

# Set Working Variables
wd <- "O:\\Data\\UAS\\UAS_BodyCondition"

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

# Run code -------------------------------------------------------
setwd(wd)
"%notin%" <- Negate("%in%")

# Get list of already imported data from DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              #port = Sys.getenv("pep_port"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

imported <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT lrf_file_name FROM body_condition.tbl_lrf_feed")

# Create list of camera folders within which data need to be processed 
dir <- list.dirs(wd, full.names = TRUE, recursive = FALSE)
dir <- data.frame(path = dir[grep("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", dir)], stringsAsFactors = FALSE) 
dir <- dir %>%
  mutate(path = paste(path, 'LRF_data', sep = "/"))
for (i in 1:nrow(dir)){
  if(!dir.exists(dir$path[i])) next 
  files <- list.files(dir$path[i], full.names = TRUE, recursive = FALSE)
  files <- data.frame(path = files, stringsAsFactors = FALSE)
  files <- files %>%
    mutate(file_name = basename(path))
  
  for (j in 1:nrow(files)){
    # Check if file already imported
    if(files$file_name[j] %in% imported$lrf_file_name) next
    
    # Process and import data
    
  }
}
