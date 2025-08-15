// **INFO** //
// version: 07/2024 
// loops through multi-series files in a directory and saves each serie as a separate tif file
// author: Tereza Belinova (IOF), Institute of Science and Technology Austria



#@ File (label = "Select folder with raw data", style="directory") input //defines folder with raw data
#@ String (label="Image files extension:", choices={".lif",".nd2", ".czi", ".vsi"}, style="radioButtonHorizontal") extension //defines extension of files to be processed


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
		run("Bio-Formats Importer", "open=["+ fname_with_path + "] color_mode=Default rois_import=[ROI manager] split_channels view=Hyperstack stack_order=XYCZT series_"+i);
		//renaming
		title = getTitle();
		trunctitle = substring(title, 0, title.length-1);
		selectImage(trunctitle + "0");
		selectImage(trunctitle + "2");
		run("Merge Channels...", "c1=[" + trunctitle + "2] c2=[" + trunctitle + "0] create");
		name = substring(fname_with_path, 0 , fname_with_path.length-3);
		saveName = name + "_" + i;
		saveAs("Tiff", saveName);
		saveAs("PNG", saveName);
		//counter
		print(i+" of "+seriesCount+" series processed.");
	}
	close("*");
}
showMessage("These WERE the droids you were looking for!");




