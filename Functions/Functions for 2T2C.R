#### Functions for use with 2T2C
#### Author: Ted Aplin
#### Feel free to modify to best suit your needs, but don't come crying to me if it no longer works!

### spaces to _
Underscore <- function(x) {
  return(gsub(" ", "_", x))
}


### Extending the Input data to include more information
DataExtending <- function(FileName , TimeInterval, SensorNames, ROINames){
  ## creating full data-set
  # calculations for identifying ROIs
  if (endsWith(InputFileName, ".csv") != TRUE){
    message('ERROR: You must enter a filename in the form "file name.csv"', "\n")
    return()
  } 
  if (file.exists(here("Input", "Individual", FileName)) == FALSE){
    message('ERROR: This file does not exist, check the Input folder to see if the file is present and has the right name!', "\n")
  return()
  }
  
  ## loading
  Input <- read.csv(here("Input", "Individual", FileName))
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
  write.csv(Output, here("Output", "Data", OutputName), row.names = FALSE)
  cat(".csv file successfully saved as", here("Output", "Input", OutputName))
  # returning the data-set
  return(Output)
}


### Wrangling the data into a format more useful for analysis
DataWrangling <- function(Input, CalciumChannel, BackgroundChannel, BackgroundROI, FileName){
  ## Error checking
  Error <- FALSE
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
    Error <- TRUE
  }
  if (any(BackgroundROI > max(Input$ROI)) | CalciumChannel > max(Input$Channel) | BackgroundChannel > max(Input$Channel)){
    if(CalciumChannel > max(Input$Channel)){
      message("\n ERROR: Calcium channel entered does not exist, make sure the number entered is not too high \n")
      message("There are ", max(Input$Channel), "channels present \n")
    }
    if(BackgroundChannel > max(Input$Channel)){
      message("\n ERROR: Background channel entered does not exist, make sure the number entered is not too high, or 0 if you don't want to add Background channel data.", "\n")  
      message("There are ", max(Input$Channel), "channels present \n")
    }
    
    if(any(BackgroundROI > max(Input$ROI))){
      message("\n ERROR: Background ROI entered does not exist, make sure the number entered is not too high, or 0 if you don't want to add Background ROI data.", "\n")
      message("There are ", max(Input$ROI), "ROIs present \n")
    }
    Error <- TRUE
  }
  if(CalciumChannel == 0){
    message("\n ERROR: No Calcium Channel data added, there can be no analysis without calcium data! \n")
    Error <- TRUE
  }
  if(CalciumChannel == BackgroundChannel){
    message("\n ERROR: Calcium and Background have been set to the same channel, please ensure they are on different channels \n")
    Error <- TRUE
  }
  if (Error == TRUE){
    return()
  }
  
  ## wrangling the data
  Output <- Input %>% 
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
  if (all(BackgroundChannel == 0)){
    cat("No Background channel set, Ratio Will be set to Raw calcium data \n")
    Output$Ratio <- Output$Mean_Calcium
  }
  else{
    Output$Ratio <- Output$Mean_Calcium/Output$Mean_Background
    cat("Ratio added correctly! \n")
  }
  
  # creating a column of entirely background Input
  if (all(BackgroundROI == 0)){
    cat("No Background ROI set \n")
  }
  else{
    Output$ROI[Output$ROI %in% BackgroundROI] <- 0
    cat("Background set correctly! \n")
  }
  Output <- Output[order(Output$Time, Output$ROI), ]
  OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Wrangled.csv")
  # Saving as a CSV
  write.csv(Output, here("Output", "Data", OutputName), row.names = FALSE)
  cat(".csv file successfully saved as", here("Output", "Data", OutputName))
  return(Output)
}


### Creating a dataset that is useable with the octave protocol, which can be found in the "Octave" folder
OctaveFile <- function(Input, FileName){
  Output <- filter(Input, ROI != 0) %>% 
    select(ROI, Time, Ratio) %>% 
    pivot_wider(names_from = ROI,
                values_from = Ratio) %>% 
    arrange(Time)
  cat("Table created successfully!")
  OutputName <- paste0(substr(FileName, 1, nchar(FileName) - 4), "-Octave_Input.csv")
  # Saving as a CSV
  write.csv(Output, here("Output", "Data", OutputName), row.names = FALSE)
  cat(".csv file successfully saved as", here("Output", "Data", OutputName))
}


### creates a linegraph based on the data provided
LineGraph <- function(Input, FileName, GraphName, YLimit, Resolution, i=0){
  GraphData <- filter(Input, ROI != 0)
  if (YLimit == 0)
    YLimit <- 1.1 * max(GraphData$Ratio)
  Output <- ggplot(GraphData , aes(x = Time, y = Ratio, colour = ROIName)) +
    geom_line() +
    ylim(0, YLimit) +
    ggtitle(GraphName) +
    theme_bw() +
    theme(scale_colour_manual(values = alphabet())) +
    (if (i != 0) theme(legend.position = "none", scale_colour_manual(values = alphabet()))) +
    labs(x = "Time (Seconds)", y = "Relative Intensity")  # Use provided x and y labels

  
  
  if (i == 0){
    FileNamePNG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph.png")
    FileNameSVG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph.svg")
  }
  else{
    FileNamePNG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROIName[1], ".png")
    FileNameSVG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROIName[1], ".svg")
  }
  png(filename = here("Output","PNGs", FileNamePNG), width = Resolution[1], height = Resolution[2], res = Resolution[3])
  print(Output)
  dev.off()
  svglite(filename = here("Output","SVGs", FileNameSVG), width = (Resolution[1]/150), height = (Resolution[2]/150), scaling = (Resolution[3])/150)
  print(Output)
  dev.off()
  return(YLimit)
}

### Creates a Dataset from multiple for use in normalisation
BatchDataset <- function(SampleNames, DisplayNames, FileName){
  # error messages
  if (length(SampleNames) < 1 |  data.class(SampleNames) != "character"){
    message('ERROR: Sample Name Data entered incorrectly, ensure that it is in the form c("sample 1", "sample 2", ...) \n')
    return()
  }
  if (length(DisplayNames) < 1 | all((data.class(DisplayNames) != "character" & DisplayNames != 0))){
    message('ERROR: Display Name Data entered incorrectly, ensure that it is in the form c("sample 1", "sample 2", ...), or put 0 if you want to keep the same names as in the file names \n')
    return()
  }
  if (length(DisplayNames) != length(SampleNames) & DisplayNames != 0){
    message('ERROR: You have not entered the correct amount of Names for Display, if you want to take the names from the file names then set it as 0 \n')
    message("You have entered ", length(DisplayNames), " display names ", length(SampleNames), " sample names \n")
    return()
  }
  if (DisplayNames == 0){
    DisplayNames <- SampleNames
    cat("No display names entered, so sample names used instead \n")
  }
  # getting filenames
  List <- list.files(path = here("Input", "Batch"))
  FileList <- character(0)
  for (i in 1:length(List)) {
    if (endsWith(List[i], ".csv")) {      # Check for file extension
      FileList <- c(fileList, List[i])      # Append matching files to fileList
    }
  }
  if(length(FileList) == 0){
    message("ERROR: No .csv files detected in the folder Input/Batch \n")
    return()
  }
  else{
    cat(length(FileList), " .csv files found in Input/Batch \n")
  }
  Names <- 
  for (i in 1:length(FileList)){
    Temp <- read.csv(here("Input", "Batch", FileList[i]))
  }
}


######## Unifinshed functions
MultiPlot <- function(Input, FileName, GraphName){
  for(i in 1:max(Input$ROI)){
    Output <- filter(Input, ROI == i)
    OutputName <- paste0(GraphName, " - ", Output$ROIName[1])
    LineGraph(Output, FileName, Outputname, i)
  }
}

BackgroundTable <- function(Input, Sample, FileName, InputName){
  Input <- filter(Input, ROI == 0)
  if (nrow(Input) == 0){
    Message("ERROR: There is no background ROI presented in the given data \n")
  }
  else{
    if (file.exists(here("Output", "Data", FileName)) == FALSE){
      write.table(matrix(c("Data", "Sample", "Mean"), ncol = 3, nrow = 1), 
                  file = here("Output", "Data", FileName), sep = ",",
                  append = FALSE, col.names = FALSE, row.names = FALSE, quote = TRUE)
    }
    Mean <- mean(Input$Ratio)
    Output <- matrix(c(InputName, Sample, Mean), ncol = 3, nrow = 1)
    write.table(Output, file = here("Output", "Data", FileName), sep = ",",
                append = TRUE, col.names = FALSE, row.names = FALSE, quote = TRUE)
  }
}