# Body Condition Data Management

This repository stores code and processing information related to the body condition project.

The Datasheets folder contains datasheets for individual field efforts. 

R code for processing data are stored in the root of the folder. Code numbered 0+ are intended to be run sequentially as the data are available for processing. Code numbered 99 are stored for longetivity, but are intended to only be run once to address a specific issue or run as needed, depending on the intent of the code.

The data management processing code is as follows:
* **BodyCondition_00_RenameImages.R** - code to rename images collected by the M2EA drone
* **BodyCondition_00_RenameImages_M30T.R** - code to rename images collected by the M30T drone; currently in development and on-hold until we decide how we will move forward with handling and processing those images
* **BodyCondition_01_ImportImageExif.R** - code to import image-associated exif data into the DB; must be imported before measurements can begin (for the M2EA drone configuration)
* **BodyCondition_01_ImportLaserRangefinderData.R** - code to import LRF data into the DB; must be imported before measurements can begin (for the M2EA drone configuration)
* **BodyCondition_02a_QualityCheckLaserRangefinderData.txt** - code to run QA/QC on the LRF data (for the M2EA drone configuration); this code is run in PGAdmin
* **BodyCondition_02b_AssignImages2Flight.txt** - code to assign images to flight data; the data from the field datasheets MUST be entered into the DB before this code is run!!
* **BodyCondition_02c_CalculateAdjustedImageTime.txt** - code to update image date/time based on known GPS offset; the data from the field datasheets MUST be entered into the DB before this code is run!!
* **BodyCondition_03_ImportMeasurements.R** - code to import measurements (after they have been completed or updated) into the DB; if a change is made to the measurements data, it is up to the person measuring the seal or object to delete the incorrect measurements from the DB!

Other code in the repository includes:
* Code to generate reports (in the field or afterwards) to review the data collected for potential issues:
	* BodyCondition_99_AltitudePitchRollComparisons_basedOnDB.Rmd
	* BodyCondition_99_AltitudePitchRollComparisons_basedOnFiles.Rmd
	* BodyCondition_99_AltitudePitchRollComparisons_preDyson2022tests.Rmd
* Generic code to extract information from the image exif data:
	* BodyCondition_99_ExtractImageDateTime.R - for date/time information
	* BodyCondition_99_ExtractImageExif.R - for all image exif information
* Code used to help evaluate the LRF data and identify QA/QC criteria:
	* BodyCondition_99_LRF_EvaluateQAQC.qmd
* Code to evaluate error, based on measurements made from photos taken of a fixed length object (engine box):
	* BodyCondition_99_Measurements_EngineBox.Rmd
	* BodyCondition_99_Measurements_EngineBox_2ndTest.Rmd
* Code to import validation data for captive animals:
	* BodyCondition_99_ImportValidationData.R
* Code to create the dataset used for analysis:
	* BodyCondition_99_Query4ModelDevelopmentExport.txt
	
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.