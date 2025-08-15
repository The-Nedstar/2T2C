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
    Output$ROI_Name <- Output$ROI
  }
  else{
    if (length(ROINames) != max(Output$ROI)){
      message('ERROR: ROI name data entered incorrectly, ensure it is done in the form c("name for ROI 1", "name for ROI 2", ...) and that you have put a name for each ROI present. \n')
      message("you entered names for ", length(ROINames), " ROIs but need names for ", max(Output$ROI), " ROIs.", "\n")    
      Output$ROI_Name <- Output$ROI
    }
    else{
      Output$ROI_Name <- ROINames[Output$ROI]
      cat("ROI name data added successfully!", "\n")
    }
  }
  Output$ROI_Name <- as.factor(Output$ROI_Name)
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
  write.csv(Output, here("Output", "Data", OutputName), row.names = FALSE, col.names = FALSE)
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
    FileNamePNG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROI_Name[1], ".png")
    FileNameSVG <- paste0(substr(FileName, 1, nchar(FileName) - 4), " graph-", Input$ROI_Name[1], ".svg")
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
  ## error messages
  if (length(SampleNames) < 1 |  data.class(SampleNames) != "character"){
    message('ERROR: Sample Name Data entered incorrectly, ensure that it is in the form c("sample 1", "sample 2", ...) \n')
    return()
  }
  if (length(DisplayNames) < 1 | all((data.class(DisplayNames) != "character" & DisplayNames != 0))){
    message('ERROR: Display Name Data entered incorrectly, ensure that it is in the form c("sample 1", "sample 2", ...), or put 0 if you want to keep the same names as in the file names \n')
    return()
  }
  if (all(length(DisplayNames) != length(SampleNames) & all(DisplayNames != 0))){
    message('ERROR: You have not entered the correct amount of Names for Display, if you want to take the names from the file names then set it as 0 \n')
    message("You have entered ", length(DisplayNames), " display names ", length(SampleNames), " sample names \n")
    return()
  }
  if (all(DisplayNames == 0)){
    DisplayNames <- SampleNames
    cat("No display names entered, so sample names used instead \n")
  }
  
  ## getting filenames
  UnusedFiles <- data.frame(
    File_Name = character(),
    Reason = character(),
    stringsAsFactors = FALSE)
  List <- list.files(path = here("Input", "Batch"))
  FileList <- character(0)
  
  ## checking if .csv
  for (i in 1:length(List)) {
    if (endsWith(List[i], ".csv")) {      # Check for file extension
      FileList <- c(FileList, List[i])      # Append matching files to FileList
    }
    else{
      UnusedFiles <- rbind(UnusedFiles, data.frame(File_Name = List[i], 
                                                   Reason = "Not a .csv files"))
    }
  }
  
  ## outputting number of suitable files
  if(length(FileList) == 0){
    message("ERROR: No .csv files detected in the folder Input/Batch \n")
    return()
  }
  else{
    cat(length(FileList), " .csv files found in Input/Batch \n")
  }
  
  ## adding data to batch dataset
  Names <- c("Time", "ROI_Name", "ROI", "Mean_Calcium", "Mean_Background", "Ratio")
  Output <- data.frame(
    Sample <- character(),
    Time = numeric(),
    ROI_Name = character(),
    ROI = numeric(),
    Mean_Calcium = numeric(),
    Mean_Background = numeric(),
    Ratio = numeric(),
    File_Name = character(),
    stringsAsFactors = TRUE)
  
  Counter <- data.frame(
    Sample = SampleNames,
    Count = 0)
  
  Counter <- rbind(Counter, data.frame(
    Sample = "Failed to add",
    Count = 0))

  for (i in 1:length(FileList)){
    Unused <- TRUE
    Temp <- read.csv(here("Input", "Batch", FileList[i]))
    if (any(colnames(Temp) != Names)){
      UnusedFiles <- rbind(UnusedFiles, data.frame(File_Name = List[i], 
                                                   Reason = "File incorrectly formatted"))
      Unused <- FALSE
    }
    else{
      for (u in 1:length(SampleNames)){
        if (grepl(SampleNames[u], FileList[i])){
          Output <- rbind(Output, data.frame(Sample = DisplayNames[u],
                                             Time = Temp$Time,
                                             ROI_Name = Temp$ROI_Name,
                                             ROI = Temp$ROI,
                                             Mean_Calcium = Temp$Mean_Calcium,
                                             Mean_Background = Temp$Mean_Background,
                                             Ratio = Temp$Ratio,
                                             File_Name = FileList[i],
                                             stringsAsFactors = TRUE))
          Counter$Count[u] <- Counter$Count[u] + 1
          Unused <- FALSE
        }
      }
    }
    if (Unused == TRUE){
      UnusedFiles <- rbind(UnusedFiles, data.frame(File_Name = List[i], 
                                                   Reason = "File did not contain any given sample names"))
    }
  }
  
  ## saving .csv file
  if (endsWith(FileName, ".csv") != TRUE){
    FileName <- paste0(FileName, ".csv")
  }
  write.csv(Output, here("Output", "Data", FileName), row.names = FALSE)
  
  ## showing the progress
  cat("Batch file created as: ",here("Output", "Data", FileName), ", information shall now be presented: \n")
  cat("In total ", sum(Counter$Count), " files were correctly processed, a breakdown of how many are in each sample has been created for you to view \n")
  message("In total ", nrow(UnusedFiles), " files were not able to be added, a breakdown of each file and the reason for not adding has been created too")
  
  Counter$Count[nrow(Counter)] <- nrow(UnusedFiles)
  view(UnusedFiles)
  view(Counter)
  return(Output)
}

### Histogram funciton
Boxplot <- function(Input){
  Data <- Input %>%
    filter(ROI == 0) %>%
    group_by(File_Name) %>%
    summarise(Background_Intensity = mean(Mean_Calcium, na.rm = TRUE), Sample = Sample[1])
  Output <- ggplot(Data, aes(x = Sample, y = Background_Intensity, colour = Sample)) +
    geom_boxplot() +
    geom_beeswarm() +
    theme_bw()
  print(Output)
  return(Data)
}



######## Unifinshed functions
MultiPlot <- function(Input, FileName, GraphName){
  for(i in 1:max(Input$ROI)){
    Output <- filter(Input, ROI == i)
    OutputName <- paste0(GraphName, " - ", Output$ROI_Name[1])
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