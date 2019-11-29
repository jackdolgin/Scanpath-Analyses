# Separating the Influence of Budget and Numeric Priming on Willingness to Pay


### Motivation:

The files in this repository can be used to download and analyze eye tracking data. These files were designed to work specifically with data from my undergraduate thesis, which matches a behavioral task to attentional data.


### In each file:

The first file, [Jack_code_Raw_Data->Organized.R](Jack_code_Raw_Data->Organized.R), is used to sync the raw behavioral with the attentional data, and it produces an organized table of where on the computer screen participants looked during each trial of the experiment.

The file [Jack data table -> scatter.R](Jack%20data%20table%20->%20scatter.R) converts this data table into a .png scatter plot for each participant's eye locations on the screen. These plots are temporally colored so that trials that took place earlier appear brighter.

The file  [Jack_code_Data_Analyses.R](Jack_code_Data_Analyses.R) calculates several analyses of the behavioral and attentional data—number of items bought at each budget, percent of fixations at each region of interest, a transition matrix, the validity of different participants' data, and a list of any trials that did not have any registered fixations.

The final file, [Jack_code_Temporal_Patterns.R](Jack_code_Temporal_Patterns.R), calculates a trending scanpath for each participant. Essentially, it modifies a pre-existing algorithm called the Scanpath Trend Analysis (STA) to determine temporal patterns in participants across trials. (see http://wel-data.cs.manchester.ac.uk/data_files/18)


### Tech:

These four programs are written in R and require the installation of dplyr, tidyr, and ggplot2.


### Authors:

These programs were written by Jack Dolgin under the guidance of Dianna Amasino and Professor Scott Huettel at Duke University.
