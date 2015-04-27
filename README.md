# MAPS-to-Models
Methods for Developing Population Models with Mark-Recapture Data from the MAPS program

`MAPS-to-Models` is an R script that provides functions to analyze 
data from marked individuals (mark-recapture data) to develop a 
stage-structured, stochastic, density dependent population model.

For more information, please see Ryu et al. (2015), who describe the methods and demonstrate their use with data from the MAPS (Monitoring Avian Productivity and Survivarship) program (hence "MAPS" in `MAPS-to-Models`).

The program allows estimating survival rates; fecundities; density dependence in survival rate; density dependence in fecundity; temporal variability in survival rate; temporal variability in fecundity.

## Program output
`MAPS-to-Models` creates the following files:

(1) Population model summary: a text file that includes all the components of the population model.

(2) Debug file: a text file  with error and warning messages created during the analysis.

(3) Intermediate results file: a text file that contains all output and intermediate results created during the analysis.

(4) MP files: input files for RAMAS Metapop and [RAMAS GIS](http://ramas.com/software.htm) Software.  Three input files are created.  One includes the best estimates of all parameters of the population model.  Two other files include the lower and upper bounds of the parameters.  These can be used with the R package `demgsa` to perform a global sensitivity analysis.
