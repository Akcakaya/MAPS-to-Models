# MAPS-to-Models
Methods for Developing Population Models with Mark-Recapture Data from the MAPS program

`MAPS-to-Models` is an R script that provides functions to analyze 
data from marked individuals (mark-recapture data) to develop a 
stage-structured, stochastic, density dependent population model.

For more information, please see Ryu et al. (2015).

The program allows estimating the following:
survival rates
  fecundities
  density dependence in survival rate
  density dependence in fecundity
  temporal variability in survival rate
  temporal variability in fecundity

sensitivity analysis (GSA) of demographic models created via the 
[RAMAS GIS](http://ramas.com/software.htm) Software. `demgsa` includes 
functions to make easier each major step of a GSA:
