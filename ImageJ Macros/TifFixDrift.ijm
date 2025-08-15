// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Tereza Belinova (IOF), Institute of Science and Technology Austria

#@ File (label = "Select folder with unstable data", style="directory") input //defines folder with raw data
#@ File (label = "Select folder to export stabilised files to", style="directory") output //defines output directory for exported data
#@ int (label = "Which channel do you want to use for the stabilisation?") channel //defines channel for stabilisation
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

error = 0
//the macro
for (b = 0; b < fileList.length; b++) {
	name=fileList[b];
	fname_with_path = input + File.separator + name;
	open(fname_with_path);
	getDimensions(width, height, channels, slices, frames);
	if (channel > channels || channel == 0){
		print("No channel ", channel, "found in", name);
		Error = 1
	}
	else {
		print("Stabilising Image " + name);
		run("Correct 3D drift", "channel=" + channel + " only=0 lowest=1 highest=1 max_shift_x=1 max_shift_y=1 max_shift_z=0");
		print("Successfully stabilised Image " + name);
		selectImage("registered time points");
		saveAs("Tiff",  output + File.separator + name);
		print("Saved image "+ name);
		close("*");
	}
}
if (error == 0){
	showMessage("All files stabilised");
}
if (error == 1){
	showMessage("Error with at least 1 file");
}




