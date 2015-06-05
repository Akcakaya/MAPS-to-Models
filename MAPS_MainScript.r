#######################################################################
##### Main script: MAPS to Models
#####
##### Objective: use MAPS database (Monitoring Avian Productivity and Survivorship) to parameterize 
#####            a full, stochastic population model for one or more focal species. 
#####
##### Reference: Ryu et al., "Developing Population Models with Data from Marked Individuals", submitted to MEE, April 2015
#######################################################################

#################################################################################################################
# SET GLOBAL VARIABLES
#################################################################################################################

rm(list=ls())    # clear the workspace

##########################################
### GETTING STARTED:
### FIRST, get all the files to your computer (click on "Download ZIP" or "Clone in Desktop").
### SECOND, in the following line, specify the folder where you saved the files:
BASE_DIRECTORY <- "C:\\xxxxxxx\\"
### THIRD, in the following line, specify the folder containing the WinBUGS executable
BUGSdir <- "C:\\xxxxxxx\\" 
### FOURTH, modify the settings in the lines below as needed.
### FIFTH, run this script (MAPS_MainScript.r) in R.


#########################
####  Define the scope of the analysis

## MAPS datasets in GitHub (CSV files) are available for the following species:
##                'BCCH','CACH','COYE','GRCA','HOWA','NOCA','WEVI','WOTH','YBCH'
## For reference: for MAPS dataset in GitHub, data are available for the period 1994-2012 
##               (exception is BCCH, for which data are available until 2010)

SPECIES_CODE <- "YBCH"           # Focal Species
BEGINYEAR    <- 1994             # First year of study period
ENDYEAR      <- 2012             # Final year of study period

#########################
###   Population simulation modeling parameters

METAPOP_VERSION <- "6.0"         # Version of Ramas Metapop (should be Version 6.0- has not been tested with earlier versions)
REPLICATES <- 1000							 # Number of simulation replicates
TIMESTEPS <- 50									 # Number of simulation time steps
INITIAL_ABUNDANCE <- 1000				 # Simulation initial abundance
CARRYING_CAPACITY <- 10000 			 # Simulation carrying capacity (assumed to be fixed throughout the simulation)
CORRELATION <- 1.0     					 # Set correlation between S and F  (in this study, we assumed full correlation)

#########################
###   Set MCMC parameters (for estimating fecundity)
ni <- 1000000      # Number of MCMC iterations per chain
nc <- 3           # Number of chains
nb <- 500000       # Burn-in length
nt <- 100          # Thinning rate

#################################################################################################################
# SET DIRECTORIES (and create the directory if it doesn't exist)
#################################################################################################################

PUBLICDATA_DIRECTORY <- paste(BASE_DIRECTORY,"Public dataset",sep="")
if(is.na(file.info(PUBLICDATA_DIRECTORY)[1,"isdir"])) dir.create(PUBLICDATA_DIRECTORY,recursive=T)        
POPMODELS_DIRECTORY <- paste(BASE_DIRECTORY,"Pop models",sep="")
if(is.na(file.info(POPMODELS_DIRECTORY)[1,"isdir"])) dir.create(POPMODELS_DIRECTORY,recursive=T)                      # create new directory if it doesn't exist already
RESULTS_DIRECTORY <- paste(BASE_DIRECTORY,"Results",sep="")  # for now, put the results in a separate folder
if(is.na(file.info(RESULTS_DIRECTORY)[1,"isdir"])) dir.create(RESULTS_DIRECTORY,recursive=T)
DATA_DIRECTORY <- paste(BASE_DIRECTORY,"Datasets",sep="")  # for now, put the results in a separate folder
if(is.na(file.info(DATA_DIRECTORY)[1,"isdir"])) dir.create(DATA_DIRECTORY,recursive=T)
CODE_DIRECTORY <- paste(BASE_DIRECTORY,"Code",sep="")  # for storing WinBUGS code and other code for the project
if(is.na(file.info(CODE_DIRECTORY)[1,"isdir"])) dir.create(CODE_DIRECTORY,recursive=T)

#################################################################################################################
# LOAD FUNCTIONS AND PACKAGES
#################################################################################################################

setwd(BASE_DIRECTORY)
source("MAPS_AllFunctions.r")     # functions loaded from GitHub

loadPackage("RMark")
loadPackage("R2WinBUGS")
loadPackage("MASS")
loadPackage("gtools")
loadPackage("foreign")
loadPackage("RODBC")
loadPackage("doBy")
loadPackage("msm")

#################################################################################################################
#  DEFINE ASSOCIATION BETWEEN BANDING STATIONS AND DISTINCT POPULATIONS
#################################################################################################################
# Format as CSV file with two columns:
# -- COL1 labeled "station", indicating the names of banding (e.g., MAPS) stations
# -- COL2 labeled "population", designating the corresponding biological population.
# This file should be located in DATA_DIRECTORY and called "station_pop.csv".   

setwd(PUBLICDATA_DIRECTORY)
population_map = read.csv("station_pop.csv",header=T)      # user-defined csv file where stations are grouped into separate biological populations
                                                           # In the manuscript, we chose to treat each location (comprised of multiple stations) as its own biological population

#################################################################################################################
# LOAD REGIONAL TREND DATA  (we used estimates derived from BBS data- see http://www.mbr-pwrc.usgs.gov/bbs/trend/tf13.html)
#################################################################################################################

### for reference:  BBS trend for focal species from Ryu et al. paper
# spec,trend,lcl,ucl
# NOCA,0.30,0.16,0.43
# WEVI,0.51,0.19,0.84
# GRCA,0.46,0.28,0.64
# COYE,-0.85,-1.08,-0.62
# WOTH,-2.16,-2.42,-1.89
# HOWA,1.85,1.11,2.64
# YBCH,-0.27,-0.58,0.04
# CACH,-0.37,-0.72,-0.02
# BCCH,0.29,-0.20,0.80

# estimate = central point estimate   (default is zero)
# lcl = lower confidence limit
# ucl = upper confidence limit
BBS.Trend <- Assign.Trend(estimate=-2.16, lcl=-2.42, ucl=-1.89)

#################################################################################################################
# SET UP INFORMATIVE 'DEBUG' FILE
#################################################################################################################

DEBUG_FILENAME <- paste(SPECIES_CODE,"_debug.txt",sep="")
InitializeDebugFile(dir=DATA_DIRECTORY,filename=DEBUG_FILENAME)

##################################################
# SET UP INFORMATIVE 'INTERMEDIATE RESULTS' FILE
##################################################

RESULTS_FILENAME <- paste(SPECIES_CODE,"_intermediateResults.txt",sep="")
InitializeResultsFile(dir=RESULTS_DIRECTORY,filename=RESULTS_FILENAME)

##################################################
# SET UP 'POPULATION MODEL SUMMARY' FILE
##################################################

METAPOP_FILENAME <- paste(SPECIES_CODE, ".mp", sep="")   
POPMODELSUMMARY_FILENAME <- paste(SPECIES_CODE,"_popmodelsummary.txt",sep="")
InitializePopModelFile(dir=RESULTS_DIRECTORY,filename=POPMODELSUMMARY_FILENAME)

#################################################################################################################
# READ IN RAW DATA FROM CSV FILES
#################################################################################################################

BandDataFile <- sprintf("%s_BandDataAllMonths.csv",SPECIES_CODE)    # name of CSV file containing raw band recapture data
EffortDataFile <- "Effort.csv"                                       # name of CSV file containing raw information on survey effort at each banding station 
MAPSData <- ReadRawData(dir=PUBLICDATA_DIRECTORY, BandData=BandDataFile, EffortData=EffortDataFile, StationToPop=population_map)    

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "MAPSData.RData", sep="_")
save(MAPSData, file=filename)

#######################################################################################################
# CREATE DATA STRUCTURES FOR CAPTURE MARK RECAPTURE FROM THE RETRIEVED MAPS DATA (capture history format)
#######################################################################################################

CMRData <- FormatForCMR(MAPSData=MAPSData, dir=DATA_DIRECTORY)       # NOTE: With a 2.4GHz processor, this code takes about 20 minutes to run.

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "CMRData.RData", sep="_")
save(CMRData, file=filename)

#######################################################################################################
# CREATE DATA STRUCTURES FOR RMARK FROM THE CAPTURE MARK RECAPTURE DATA
#######################################################################################################

RMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=MAPSData, dir=DATA_DIRECTORY, AddDensity=FALSE)

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "RMarkData.RData", sep="_")
save(RMarkData, file=filename)


######################################################################################################
# RUN MARK MODELS TO COMPUTE SURVIVAL RATE 
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, only run the initial MARK models. No density, just time-dependent models.
MarkResults_Time <- Run.Models(RMarkData=RMarkData, initial=0, DensityModel = FALSE) # NOTE: With a 2.4GHz processor, this code takes about 10 minutes to run.   

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Time.RData", sep="_")
save(MarkResults_Time, file=filename)


######################################################################################################
# EXTRACT CAPTURE PROBABILITIES FROM MARK RESULTS
######################################################################################################
# Use model S (~st + time + st:time + first_cap_bin:trans), p(~st + effort) 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time
p.table <- PCapResults(RMarkResults=MarkResults_Time, maps.ddl=RMarkData$maps.ddl, band.data=MAPSData$band.data, model.no=1)   # NOTE: With a 2.4GHz processor, this code takes about 20 minutes to run for species with a large dataset.

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "PTable.RData", sep="_")
save(p.table, file=filename)

#######################################################################################################
# ADD DENSITY COVARIATE TO THE RMARK DATA --- FOR ESTIMATING DENSITY-DEPENDENCE IN FINAL SURVIVAL ANALYSIS 
######################################################################################################

finalRMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=MAPSData, dir=DATA_DIRECTORY, AddDensity=TRUE) # set dir to load up the previous RMarkData

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "finalRMarkData.RData", sep="_")
save(finalRMarkData, file=filename)


######################################################################################################
# RUN MARK MODELS TO COMPUTE DENSITY-DEPENDENCE FUNCTION FOR SURVIVAL
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, run the density-dependent MARK models. 
MarkResults_Density <- Run.Models(RMarkData=finalRMarkData, initial=0, DensityModel = TRUE)    # NOTE: With a 2.4 GHz processor, this code takes about 5 minutes to run.

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Density.RData", sep="_")
save(MarkResults_Density, file=filename)


######################################################################################################
# EXTRACT (APPARENT) SURVIVAL RATES FROM MARK RESULTS
######################################################################################################
# Use S(~st+first_cap_bin:trans), p(~st+effort) 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time
SurvivalResults <- ApparentS(RMarkResults=MarkResults_Time, model.no = 2) 

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SurvivalResults.RData", sep="_")
save(SurvivalResults, file=filename)


######################################################################################################
# RUN VARIANCE COMPONENT ANALYSIS TO ESTIMATE TEMPORAL VARIABILITY IN SURVIVAL
######################################################################################################
# Use S (~st + time + st:time + first_cap_bin:trans), p(~st + effort) 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time
SurvivalTempVarResults <- VarianceComponent (RMarkResults=MarkResults_Time, model.no = 1) 

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SurvivalTempVarResults.RData", sep="_")
save(SurvivalTempVarResults, file=filename)
  

######################################################################################################
# RUN WINBUGS MODELS TO COMPUTE FECUNDITY
######################################################################################################

FecundityResults <- EstimateFecundity(p.table=p.table, MinAdults=4) # set number of captured adults under which the data will be discarded
                                                                    # if model does not run, try different priors and initial values
                                                                    # NOTE: With a 2.4 GHz processor, this code takes about 100 minutes to run.
# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "FecundityResults.RData", sep="_")
save(FecundityResults, file=filename)


######################################################################################################
# PRINT OUT KEY RESULTS FOR POPULATION MODELING (stage matrix, temporal variability, density dependence)
######################################################################################################
# Calculate density function for S from model S(~st + maps_density + first_cap_bin:trans)p(~st + effort)
MarkResults_Density

PopModelData <- SummarizeForPopModel(RMarkData=finalRMarkData, AppS=SurvivalResults, STempVar=SurvivalTempVarResults, 
                                     MarkResults=MarkResults_Density, Fec=FecundityResults, model.no=1)       

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "PopModelData.RData", sep="_")
save(PopModelData, file=filename)


######################################################################################################
# CREATE STAGE MATRIX, SD MATRIX AND CORRECT FOR APPARENT SURVIVAL AND TEMPORAL VARIABILITY OF JUVENILES USING CV METHOD
######################################################################################################

SummaryMP<-SummaryMP(Data=PopModelData, TrendData=BBS.Trend)   

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SummaryMP.RData", sep="_")
save(SummaryMP, file=filename)


######################################################################################################
# BUILD RAMAS ".MP" FILE WITH PARAMETERS FROM THE ABOVE ANALYSES 
######################################################################################################

WriteMasterMPFile(Result=SummaryMP)  



################################### END OF CODE ################################################



#######################################################################################################
#######################################################################################################
# DICTIONARY OF WORKSPACE VARIABLES
#######################################################################################################

#############
# Directory variables

# BASE_DIRECTORY                 -- A complete directory path that must include files named "code", "Data", and "results".
# CODE_DIRECTORY                 -- The directory that contains all R and WinBUGS source code for running the analysis
# DATA_DIRECTORY                 -- The directory that contains raw and intermediate data used for the analysis. (Note: MAPS database can be stored anywhere, but must be registered locally as a named ODBC data source)
# POPMODELS_DIRECTORY            -- The directory that contains the RAMAS population models (and the project-specific DLL)
# RESULTS_DIRECTORY              -- The directory that contains all raw, intermediate, and final results.
# CHLOE, KEVIN, RESIT, EVA, etc. -- User names, used as a flag to adjust file paths (specifically, setting the base directory)

#############
# Data extraction controls

# BEGINYEAR                      -- Initial year for which data must be extracted from the MAPS database
# ENDYEAR                        -- Final year for which data must be extracted from the MAPS database
# MAPSyears                      -- Numeric vector containing the complete set of all years of interest (real years, e.g., 2001)
# SPECIES_CODE                   -- Standard abbreviation code for bird species: e.g., NOCA for Northern Cardinal

#############
# Simulation controls

# CARRYING_CAPACITY              -- Carrying capacity to be specified in the population model. Not a fitted parameter. Can be changed post hoc if desired.
# CORRELATION                    -- Degree to which stochastic fluctuations in survival and fecundity are correlated. 
                                    # (e.g., year with high survival tends to also associated with high fecundity)		   
# INITIAL_ABUNDANCE              -- Inital total abundance to be specified in the population model. Not a fitted parameter. Can be changed post hoc if desired.		
# METAPOP_VERSION                -- Version of Ramas Metapop. Should be version 6.0 compiled for 64 bit architecture
# REPLICATES                     -- Number of simulation replicates. Can be changed post hoc if desired.							   

############
# Data structures

# population_map                 -- Data frame associating each MAPS station in the study with specific populations. In effect, parameters will be estimated separately for each population specified in this file.
# ProcessedData                  -- List of data frames containing data extracted from the MAPS database
# CMRData	                       -- List that includes the master capture history data, and three vectors denoting the time periods for primary and secondary sampling occasions 
# RMarkData                      -- input files for MARK without density covariates
# finalRMarkData                 -- input files for MARK with density covariates
# p.table                        -- table of capture probabilities and abundances calculated based on initial MARK time models 

############
# Functions

# InitializeDebugFile            -- Initialize a text file for storing debug information. Saved in the data directory
# ToDebugFile                    -- Appends text to the debug file
# InitializeResultsFile          -- Initialize a text file for storing results information. Saved in the results directory
# ToResultsFile                  -- Appends text to the results file
# RetrieveData                   -- Retrieves relevant data from the MAPS database 
# FormatForCMR                   -- Turn the data extracted from the MAPS database into a "capture history" format 
# FormatForRMark                 -- Converts capture history data into the appropriate format for RMark (.ddl, .process)
# Run.Models                     -- Runs MARK models for survival analysis
# PCapResults                    -- Calculates capture probabilities separately for juveniles and adults based on initial MARK model result
# ApparentS                      -- Computes and extracts apprarent survival rates based on MARK model result
# VarianceComponent              -- Estimates temporal variability in survival using variance component analysis 
# EstimateFecundity              -- Runs WinBUGS for fecundity analysis 
# SummarizeForPopModel           -- Collects all parameter values required to build a population model
# WriteMasterMPFile              -- Writes an MP file to run population simulations/projections for the focal species, using parameters derived from the MAPS data.
######################################################################################################################
































