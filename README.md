# MAPS-to-Models: Developing population models with mark-recapture data

`MAPS-to-Models` is an R script that provides functions to analyze data from marked individuals (mark-recapture data) to develop a stage-structured, stochastic, density dependent population model.

For more information, please see Ryu et al. (2016), who describe the methods and demonstrate their use with data from the MAPS [(Monitoring Avian Productivity and Survivorship)] (http://www.birdpop.org/pages/maps.php) program (hence the "MAPS" in `MAPS-to-Models`).

The program allows estimating survival rates; fecundities; density dependence in survival rate; density dependence in fecundity; temporal variability in survival rate; temporal variability in fecundity.  Main features include (1) estimating true survival based on apparent survival estimates and population trend estimates; (2) fecundity as an unbiased estimate of juvenile:adult ratio by using the relative capture probabilities of juveniles and adults; (3) estimating density dependence in survival and fecundity; (4) estimating natural temporal variability in survival and fecundity (excluding sampling variability); (5) creating ready-to-run demographic model files; (6) incorporating uncertainties and preparing the files necessary for a global sensitivity analysis.

## Program input
`MAPS-to-Models` inputs data from 3 data files for each species, all in CSV (comma separated values) format:

(1) A file with all the banding and capture data.

(2) A file with effort data, quantifying the level of effort at each station and each year/month.

(3) A file that shows the correspondance between stations and populations. Often, there would be multiple banding stations within each biological population (or subpopulation) of the species.  This file allows the user to group stations into populations.

For examples of these data files, see the [dataset folder] (https://github.com/Akcakaya/MAPS-to-Models/tree/master/Public%20dataset), which include the data used in Ryu et al. (2016).

## Program output
`MAPS-to-Models` creates the following files that include the results of the analysis:

(1) Population model summary: a text file that includes all the components of the population model estimated by the program.

(2) Debug file: a text file with error and warning messages created during the analysis.

(3) Intermediate results file: a text file that contains all output and intermediate results created during the analysis.

(4) MP files: input files for RAMAS Metapop and [RAMAS GIS](http://ramas.com/software.htm) Software.  Three input files are created.  One includes the best estimates of all parameters of the population model.  Two other files include the lower and upper bounds of the parameters; these can be used with the R package `demgsa` [(github.com/mlammens/demgsa)](https://github.com/mlammens/demgsa) to perform a global sensitivity analysis (Aiello-Lammens & Akçakaya 2016). Note: These models use the user-defined density-dependence function [AvianDD.dll](https://github.com/Akcakaya/MAPS-to-Models/blob/master/AvianDD.dll), whose source code is [AvianDD.DPR](https://github.com/Akcakaya/MAPS-to-Models/blob/master/AvianDD.DPR).

## Getting started

(1) Get all the files to your computer (click on "Download ZIP" or "Clone in Desktop").

(2) In `MAPS_MainScript.r`, specify the folder where you saved the files as the `BASE_DIRECTORY`.

(3) Follow other instructions in `MAPS_MainScript.r`, modify the settings as needed, and run this script in R.

## References
Aiello-Lammens, M.A. and H.R. Akçakaya. 2016. Using global sensitivity analysis of demographic models for ecological impact assessment. *Conservation Biology* (in press).

Ryu, H.Y., K.T. Shoemaker, É. Kneip, A.M. Pidgeon, P.J. Heglund, B.L. Bateman, W.E. Thogmartin, and H.R. Akçakaya. 2016.  [Developing population models with data from marked individuals](http://authors.elsevier.com/a/1Sm4n_9CgPwpw). *Biological Conservation* 197: 190–199.

