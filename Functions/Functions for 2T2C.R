#### Functions for use with 2T2C
#### Author: Ted Aplin
#### Feel free to modify to best suit your needs, but don't come crying to me if it no longer works!

### Converting the Input data format into neatly wrangled Output
DataWrangling <- function(FileName, Input, TimeInterval, SensorNames, ROINames){
  ## creating full data-set
  # calculations for identifying ROIs
  Count <- as.numeric(sum(Input$Frame == 1))
  ROICount <- Count/max(Input$Ch)
  # setting ROI numbers
  Output <- Input %>% mutate(X = X - (Ch-1)*ROICount - (Frame-1)*Count)
  # fixing column names
  colnames(Output) <- c("ROI", "Area", "Mean", "Min", "Max", "Channel", "Frame")
  
  ## adding user specified data
  # adding time data
  if (TimeInterval == FALSE){
    cat("No time interval data presented.", "\n", "\n")
  }
  else{
    if (data.class(TimeInterval) != "numeric"){
      cat("ERROR: Time interval data not added as not numeric, please write a number without speechmarks, or FALSE if you don't want to add time interval data.", "\n", "\n")
    }
    else{
      Output$Time <- (Output$Frame-1) * TimeInterval
      cat("Time interval data added successfully!", "\n", "\n")
    }
  }
  # adding sensor names
  if (all(SensorNames == FALSE)){
    cat("No sensor name data presented.", "\n", "\n")
  }
  else{
    if (length(SensorNames) != max(Output$Channel)){
      cat('ERROR: Sensor name data entered incorrectly, ensure it is done in the form c("name for sensor 1", "name for sensor 2", ...) and that you have put a name for each sensor present. \n')
      cat("you have", max(Output$Channel), "Channels that need names", "\n", "\n")
    }
    else{
      Output$SensorName <- SensorNames[Output$Channel]
      cat("Sensor name data added successfully!", "\n", "\n")
    }
  }
  # adding ROI names
  if (all(ROINames == FALSE)){
    cat("No ROI name data presented.", "\n", "\n")
  }
  else{
    if (length(ROINames) != max(Output$ROI)){
      cat('ERROR: ROI name data entered incorrectly, ensure it is done in the form c("name for ROI 1", "name for ROI 2", ...) and that you have put a name for each ROI present. \n')
      cat("you have", max(Output$ROI), "ROIs that need names", "\n", "\n")
    }
    else{
      Output$ROIName <- ROINames[Output$ROI]
      cat("ROI name data added successfully!", "\n", "\n")
    }
  }
  
  ## Outputting data-set
  # creating FileName
  OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Wrangled.csv")
  # Saving as a CSV
  write.csv(Output, here("Output", "Data", OutputName))
  cat(".csv file successfully saved as", here("Output", "Data", OutputName))
  # returning the data-set
  return(Output)
}