// **INFO** //
// version: 08/2025
// Multiple functions for processing images from .tif files or multiple proprietary filetypes
// author: Ted Aplin (Adapted from Tereza Belinova (IOF), Institute of Science and Technology Austria)



#@ File (label = "Select folder with raw data", style="directory") input //defines folder with raw data
#@ string (label = "File type for processing", choices={"all",".tif",".lif",".nd2", ".czi", ".vsi"}, style="radioButtonHorizontal") Extension
#@ string (label = "Reduce file sizes > 4GB", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Binning
#@ string (label = "Maximum intensity projection", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") MIP
#@ string (label = "Reduce drift (files < 4GB)", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Stabilisation
#@ string (label = "Add scale bar", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") ScaleBar
#@ int number (label = "Size of scale bar") Scale
#@ string (label = "Units for scale bar", choices={"mm", "um", "nm"}, style="radioButtonHorizontal") ScaleUnits
#@ string (label = "Auto brightness/contrast", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Bright
#@ string (label = "merge channels 1+2", choices={"no", "yes", "yes and save"}, style="radioButtonHorizontal") Merge
#@ string (label = "Save PNG/AVI", choices={"no", "yes"}, style="radioButtonHorizontal") PNG
#@ int number (label = "framerate for AVI") Framerate



//cleanup
setBatchMode(true);
close("*");

//creates a list of only specific fileformat files
list = getFileList(input);
fileList = newArray(0);
if (Extension == "all") {
	for (l = 0; l < list.length; l++) {
	   	if (endsWith(list[l], ".tif") | endsWith(list[l], ".lif") | endsWith(list[l], ".vsi") | endsWith(list[l], ".czi") | endsWith(list[l], ".nd2"))
	   	fileList = Array.concat(fileList, list[l]);
	}
}
else {
	for (l = 0; l < list.length; l++) {
	   	if (endsWith(list[l], Extension))
	   	fileList = Array.concat(fileList, list[l]);
	}
}

//stops if there are no suitable files
if (fileList.length == 0) {
	exit("I am missing my purpose! (no useable files in input directory)");
}

//create a folder for each save point save in specific folder
// set all to input folder



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
		if (!(endsWith(name, ".tif")))
		saveAs("Tiff", output + saveName +".tif");
		//counter
		// if binning not no
		// bin + save if save = yes
		// and so on...
		print(i + " of " + seriesCount + " series processed.");
	}
}
showMessage("These WERE the droids you were looking for!");




