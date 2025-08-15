// **INFO** //
// version: 07/2025 
// loops through files in a directory and sets auto brightness and contrast to those with only one slice
// author: Ted Aplin

#@ File (label = "Select folder with data", style="directory") input //defines folder with raw data
#@ File (label = "Select folder for output", style="directory") output //defines folder with raw data

//cleanup
setBatchMode(true);
close("*");

//creates a list of only specific fileformat files

list = getFileList(input);
fileList = newArray(0);
for (l = 0; l < list.length; l++) {
   	if (endsWith(list[l], ".tif"))
   	fileList = Array.concat(fileList, list[l]);
}

//stops if there are no suitable files
if (fileList.length == 0) {
	exit("I am missing my purpose! (no .tif files in input directory)");
}

//the macro
for (b = 0; b < fileList.length; b++) {
	name=fileList[b];
	fname_with_path = input + File.separator + fileList[b];
	savename = substring(name, 0 , name.length-4);
	outputname = output + File.separator + savename + ".avi";
	run("Bio-Formats Macro Extensions");
	run("Bio-Formats Importer", "open=["+ fname_with_path + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT series_1 use_virtual_stack");
	print("Analyzing Image " + name);
	selectImage(name);
	getDimensions(width, height, channels, slices, frames);
	if (channels == 2){
		run("Split Channels");
		selectImage("C1-" + name);
	}
	run("Brightness/Contrast...");
	run("Enhance Contrast", "saturated=0.35");
	run("AVI... ", "compression=JPEG frame=60 save=[" + outputname + "]");
	close("*");
}
showMessage("And God said 'Let there be light!'");


