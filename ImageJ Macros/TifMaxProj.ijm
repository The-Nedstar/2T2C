// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Ted Aplin
#@ File (label = "Select folder with raw data", style="directory") input //defines folder with raw data
#@ File (label = "Select folder to export maximum projection files to", style="directory") output //defines output directory for exported data

//cleanup
setBatchMode(true);
close("*");

//creates a list of only specific fileformat files

list = getFileList(input);
fileList = newArray(0);
for (l = 0; l < list.length; l++) {
   	if (endsWith(list[l], ".tif")){
   	fileList = Array.concat(fileList, list[l]);
	}
}

//stops if there are no suitable files
if (fileList.length == 0) {
	exit("I am missing my purpose! (no .tif files in input directory)");
}

//the macro
for (b = 0; b < fileList.length; b++) {
	name=fileList[b];
	fname_with_path = input + File.separator + fileList[b];
	open(fname_with_path);
	print("processing Image " + name);
	run("Z Project...", "projection=[Max Intensity] all");
	selectImage("MAX_"+fileList[b]);
	title = getTitle();
	saveAs("Tiff",  output + File.separator + title);
	print("successfully processed " + name);
	close("*");
}
showMessage("Allan please add details");




