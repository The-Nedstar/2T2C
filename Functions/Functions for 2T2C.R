#### Functions for use with 2T2C
#### Author: Ted Aplin
#### Feel free to modify to best suit your needs, but don't come crying to me if it no longer works!

### spaces to _
Underscore <- function(x) {
  return(gsub(" ", "_", x))
}


### Extending the Input data to include more information
DataExtending <- function(FileName, Input, TimeInterval, SensorNames, ROINames){
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
  if (TimeInterval == 0){
    cat("No time interval data presented.", "\n")
    Output$Time <- Output$Frame
  }
  else{
    if (data.class(TimeInterval) != "numeric"){
      Output$Time <- Output$Frame
      message("\n ERROR: Time interval data not added as not numeric, please write a number without speechmarks, or 0 if you don't want to add time interval data.", "\n")
    }
    else{
      Output$Time <- (Output$Frame-1) * TimeInterval
      cat("Time interval data added successfully!", "\n")
    }
  }
  # adding sensor names
  if (all(SensorNames == 0)){
    Output$SensorName <- Output$Channel
    cat("No sensor name data presented.", "\n")
  }
  else{
    if (length(SensorNames) != max(Output$Channel)){
      message('ERROR: Sensor name data entered incorrectly, ensure it is done in the form c("name for sensor 1", "name for sensor 2", ...) and that you have put a name for each sensor present. \n')
      message("you entered names for ", length(SensorNames), " channels but need names for ", max(Output$Channel), " channels.", "\n")
      Output$SensorName <- Output$Channel
    }
    else{
      Output$SensorName <- SensorNames[Output$Channel]
      cat("Sensor name data added successfully!", "\n")
    }
  }
  # adding ROI names
  if (all(ROINames == 0)){
    cat("No ROI name data presented.", "\n")
    Output$ROIName <- Output$ROI
  }
  else{
    if (length(ROINames) != max(Output$ROI)){
      message('ERROR: ROI name data entered incorrectly, ensure it is done in the form c("name for ROI 1", "name for ROI 2", ...) and that you have put a name for each ROI present. \n')
      message("you entered names for ", length(ROINames), " ROIs but need names for ", max(Output$ROI), " ROIs.", "\n")    
      Output$ROIName <- Output$ROI
    }
    else{
      Output$ROIName <- ROINames[Output$ROI]
      cat("ROI name data added successfully!", "\n")
    }
  }
  Output$ROIName <- as.factor(Output$ROIName)
  Output$SensorName <- as.factor(Output$SensorName)
  ## Outputting data-set
  # creating FileName
  OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Extended.csv")
  # Saving as a CSV
  write.csv(Output, here("Output", "Data", OutputName))
  cat(".csv file successfully saved as", here("Output", "Input", OutputName))
  # returning the data-set
  return(Output)
}


### Wrangling the data into a format more useful for analysis
DataWrangling <- function(OGInput, CalciumChannel, BackgroundChannel, BackgroundROI, FileName){
  if (data.class(BackgroundROI) != "numeric" | data.class(BackgroundChannel) != "numeric" | data.class(CalciumChannel) != "numeric" ){
    if(data.class(CalciumChannel) != "numeric"){
      message("\n ERROR: Calcium channel data not added as not numeric, please write a number without speechmarks", "\n")    
    }
    if(data.class(BackgroundChannel) != "numeric"){
      message("\n ERROR: Background channel data not added as not numeric, please write a number without speechmarks, or 0 if you don't want to add Background channel data.", "\n")  
    }
    if(data.class(BackgroundROI) != "numeric"){
    message("\n ERROR: Background ROI data not added as not numeric, please write a number without speechmarks, or 0 if you don't want to add Background ROI data.", "\n")
    }
  }
  else{
    if (BackgroundROI > max(OGInput$ROI) | CalciumChannel > max(OGInput$Channel) | BackgroundChannel > max(OGInput$Channel)){
      if(CalciumChannel > max(OGInput$Channel)){
        message("\n ERROR: Calcium channel entered does not exist, make sure the number entered is not too high \n")    
      }
      if(BackgroundChannel > max(OGInput$Channel)){
        message("\n ERROR: Background channel entered does not exist, make sure the number entered is not too high, or 0 if you don't want to add Background channel data.", "\n")  
      }
      if(BackgroundROI > max(OGInput$ROI)){
        message("\n ERROR: Background ROI entered does not exist, make sure the number entered is not too high, or 0 if you don't want to add Background ROI data.", "\n")
      }
    }
    else{
      if(CalciumChannel == 0){
        message("\n ERROR: No Calcium Channel data added, there can be no analysis without calcium data! \n")
      }
      else{
        if(CalciumChannel == BackgroundChannel){
          message("\n ERROR: Calcium and Background have been set to the same channel, please ensure they are on different channels \n")
        }
        else{
          ## wrangling the data
          Output <- OGInput %>% 
            # selecting only the relevant channels
            filter(Channel %in% c(CalciumChannel, BackgroundChannel)) %>% 
            # adding a new column to say if a value is from the calcium sensor or background one
            mutate(Sensor = case_when(
              Channel == CalciumChannel ~ "Mean_Calcium",
              Channel == BackgroundChannel ~ "Mean_Background"
            )) %>% 
            # selecting only relevant columns
            select(Time, ROIName, ROI, Sensor, Mean) %>% 
            # merging rows with the same ROI
            pivot_wider(names_from = Sensor,
                        values_from = Mean) %>% 
            arrange(Time, ROIName)
          cat("Input wrangled successfully! \n")
          
          ## calculations
          # calculating Ratio
          if (BackgroundChannel == 0){
            cat("No Background channel set, Ratio Will be set to Raw calcium data \n")
            Output$Ratio <- Output$Mean_Calcium
          }
          else{
            Output$Ratio <- Output$Mean_Calcium/Output$Mean_Background
            cat("Ratio added correctly! \n")
          }
          
          # creating a column of entirely background Input
          if (BackgroundROI == 0){
            cat("No Background ROI set \n")
          }
          else{
            Output$ROI[Output$ROI == BackgroundROI] <- 0
            cat("Background set correctly! \n")
          }
          Output <- Output[order(Output$Time, Output$ROI), ]
          OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Wrangled.csv")
          # Saving as a CSV
          write.csv(Output, here("Output", "Data", OutputName))
          cat(".csv file successfully saved as", here("Output", "Data", OutputName))
          return(Output)
        }
      }
    }
  }
}


OctaveFile <- function(Input, FileName){
  Output <- Input %>% 
    select(ROI, Time, Normalised_Ratio) %>% 
    pivot_wider(names_from = ROI,
                values_from = Normalised_Ratio) %>% 
    arrange(Time)
  cat("Table created successfully!")
  OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Octave_Input.csv")
  # Saving as a CSV
  write.csv(Output, here("Output", "Data", OutputName))
  cat(".csv file successfully saved as", here("Output", "Data", OutputName))
}


LineGraph <- function(Input, FileName, i=0){
  Output <- ggplot(filter(Input, ROI != 0) , aes(x = Time, y = Normalised_Ratio, colour = ROIName)) +
    geom_line()
  if (i == 0){
    FileNamePNG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph.png")
    FileNameSVG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph.svg")
  }
  else{
    FileNamePNG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROIName[1], ".png")
    FileNameSVG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROIName[1], ".svg")
  }
  png(filename = here("Output","PNGs", FileNamePNG), width = 2000, height = 1000, res = 200)
  print(Output)
  dev.off()
}


MultiPlot <- function(Input, FileName){
  for(i in 1:max(Input$ROI)){
    LineGraph(filter(Input, ROI == i), FileName)
  }
}