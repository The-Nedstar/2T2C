// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Tereza Belinova (IOF), Institute of Science and Technology Austria

#@ File (label = "Select folder with raw data", style="directory") input //defines folder with raw data
#@ File (label = "Select folder to export tif files to", style="directory") output //defines output directory for exported data

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
	exit("I am missing my purpose! (no .lif files in input directory)");
}

//the macro
for (b = 0; b < fileList.length; b++) {
	name=fileList[b];
	fname_with_path = input + File.separator + fileList[b];
	open(fname_with_path);
	print("Analyzing Image " + name);
	width = getWidth();
	if (width > 3000) {
		run("Bin...", "x=3 y=3 z=1 bin=Average");
	}
	else {
		run("Bin...", "x=2 y=2 z=1 bin=Average");
	}
	title = getTitle();
	name = substring(title, 0 , title.length-3);
	saveName = name + "_" + "bin";
	saveAs("Tiff",  output + File.separator + saveName +".tif");
	close("*");
}
showMessage("Where's the bin??? Where it's always bin");




