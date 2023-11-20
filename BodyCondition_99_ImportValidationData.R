# UAS Body Condition: Import validation data from Colleen
# S. Koslovsky

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

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")

# Run code -------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

# Prepare data for import
data <- read.csv("C:\\smk\\BodyCondition_UASvalidation_FromColleen.csv", skip = 2, header = FALSE, stringsAsFactors = FALSE, 
                 col.names = c("field_efforts_id", "animal_identifier", "week_of", "relative_week", 
                               "mass_kg", "std_length_cm", "str_length_cm", 
                               "curv_length_cm_nose2neck", "curv_length_cm_nose2ax", "curv_length_cm_nose2mid", "curv_length_cm_nose2umbi", 
                               "curv_length_cm_nose2pelvis", "curv_length_cm_nose2ankle", "curv_length_cm_nose2tail",
                               "girth_cm_neck", "girth_cm_ax", "girth_cm_mid", "girth_cm_umbi", "girth_cm_pelvis", "girth_cm_ankle",
                               "photo_std_length_cm_right", "photo_curv_length_cm_right", 
                               "photo_std_length_cm", "photo_std_length_cm_nose2flipper", "photo_std_length_cm_nose2neck", "photo_std_length_cm_neck2ax", 
                               "photo_std_length_cm_ax2mid", "photo_std_length_cm_mid2umbi", "photo_std_length_cm_umbi2pelvis", 
                               "photo_std_length_cm_pelvis2ankle", "photo_std_length_cm_ankle2tail", "photo_std_length_cm_tail2hind",
                               "photo_width_cm_neck", "photo_width_cm_ax", "photo_width_cm_mid", "photo_width_cm_umbi", "photo_width_cm_pelvis", "photo_width_cm_ankle",
                               "blubber_dorsal_cm_neck", "blubber_dorsal_cm_ax", "blubber_dorsal_cm_mid", "blubber_dorsal_cm_umbi", "blubber_dorsal_cm_pelvis",
                               "blubber_dorsal_cm_ankle", "blubber_lateral_cm_neck", "blubber_lateral_cm_ax", "blubber_lateral_cm_mid", "blubber_lateral_cm_umbi",
                               "blubber_lateral_cm_pelvis", "blubber_lateral_cm_ankle", "blubber_sternal_cm",
                               "body_condition_analysis", "pct_blubber", "validation_comments"))

data2DB <- data %>%
  mutate(field_efforts_id = as.integer(ifelse(field_efforts_id == "2021 LML SEPT", "1",
                                   ifelse(field_efforts_id == "2022 ASLC JUN", "4",
                                          ifelse(field_efforts_id == "2022 ALSC SEP", "5",
                                                 ifelse(field_efforts_id == "2023 LML JULY", "6", "-99")))))) %>%
  mutate(week_of = as.Date(week_of, "%m/%d/%y")) %>%
  mutate_all(str_replace_all, "x", "-99") %>% 
  mutate(photo_std_length_cm_neck2ax = ifelse(photo_std_length_cm_neck2ax == "25.4.", "25.4", photo_std_length_cm_neck2ax)) %>%
  mutate_if(is_all_numeric,as.numeric) %>%
  mutate(id = 1:nrow(data)) %>%
  select(id, 1:54)

# Write data to the DB
RPostgreSQL::dbWriteTable(con, c("body_condition", "tbl_measurements_validation"), data2DB, overwrite = TRUE, row.names = FALSE)

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con)