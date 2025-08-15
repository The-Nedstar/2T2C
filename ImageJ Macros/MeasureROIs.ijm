// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Ted Aplin

#@ File (label = "Select folder to export csv files to", style="directory") output //defines output directory for exported data

roiManager("Deselect");
roiManager("multi-measure measure_all");
title = getTitle();
name = substring(title, 0 , title.length-4);
saveAs("Results",  output + File.separator + name +".csv");
//roiManager("Show All with labels");
//run("RGB color");
//selectImage(title);