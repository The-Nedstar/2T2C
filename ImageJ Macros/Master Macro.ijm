// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Tereza Belinova (IOF), Institute of Science and Technology Austria



#@ File (label = "Select folder with raw data", style="directory") input //defines folder with raw data
#@ File (label = "Select folder to export tif files to", style="directory") output //defines output directory for exported data
#@ string (label = "Reduce file sizes > 4GB", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Binning
#@ string (label = "Maximum intensity projection", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") MIP
#@ string (label = "Auto brightness/contrast", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Bright
#@ string (label = "Reduce drift", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Stabilisation


//cleanup
setBatchMode(true);
close("*");

//creates a list of only specific fileformat files

list = getFileList(input);
fileList = newArray(0);
for (l = 0; l < list.length; l++) {
   	if (endsWith(list[l], extension))
   	fileList = Array.concat(fileList, list[l]);
}

//stops if there are no suitable files
if (fileList.length == 0) {
	exit("I am missing my purpose! (no .lif files in input directory)");
}

//the macro
for (b = 0; b < fileList.length; b++) {
	name=fileList[b];
	fname_with_path = input + File.separator +fileList[b];
	run("Bio-Formats Macro Extensions");
	run("Bio-Formats Importer", "open=["+ fname_with_path + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT series_1");
	seriesCount = 0;
	Ext.setId(fname_with_path)
	Ext.getSeriesCount(seriesCount);
	print("Analyzing Image " + name);
	close("*");
	for (i = 1; i <= seriesCount; i++) {
		run("Bio-Formats Importer", "open=["+ fname_with_path + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT series_"+i);
		//renaming
		title = getTitle();
		name = substring(title, 0 , title.length-3);
		saveName = name + "_" + i;
		saveAs("Tiff", output + saveName +".tif");
		//counter
		print(i+" of "+seriesCount+" series processed.");
	}
	close("*");
}
showMessage("These WERE the droids you were looking for!");




