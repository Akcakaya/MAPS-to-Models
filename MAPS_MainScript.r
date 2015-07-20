#######################################################################
##### Main script: MAPS to Models
#####
##### Objective: Use MAPS database (Monitoring Avian Productivity and Survivorship) to parameterize 
#####            a full, stochastic population model for one or more focal species. 
#####
##### Reference: Ryu et al. "Developing Population Models with Data from Marked Individuals"
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

### THIRD, download and install WinBUGS and in the following line, specify the folder containing the WinBUGS executable

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
nc <- 3            # Number of chains
nb <- 500000       # Burn-in length
nt <- 100          # Thinning rate

#################################################################################################################
# SET DIRECTORIES (and create the directory if it doesn't exist)
#################################################################################################################

PUBLICDATA_DIRECTORY <- paste(BASE_DIRECTORY,"Public dataset",sep="") 
if(is.na(file.info(PUBLICDATA_DIRECTORY)[1,"isdir"])) dir.create(PUBLICDATA_DIRECTORY,recursive=T)        
POPMODELS_DIRECTORY <- paste(BASE_DIRECTORY,"Pop models",sep="")      # MP files are created and stored in this directory. NOTE: To run RAMAS, dll file should also be in this folder.
if(is.na(file.info(POPMODELS_DIRECTORY)[1,"isdir"])) dir.create(POPMODELS_DIRECTORY,recursive=T)                      
RESULTS_DIRECTORY <- paste(BASE_DIRECTORY,"Results",sep="")           # Population model summary, Intermediate Results text file, and MARK output and model results are stored in this directory.
if(is.na(file.info(RESULTS_DIRECTORY)[1,"isdir"])) dir.create(RESULTS_DIRECTORY,recursive=T)
DATA_DIRECTORY <- paste(BASE_DIRECTORY,"Datasets",sep="")             # Debug text file and intermediate results (e.g., data structures for RMark, capture probabilities, etc.) are stored in this directory. 
if(is.na(file.info(DATA_DIRECTORY)[1,"isdir"])) dir.create(DATA_DIRECTORY,recursive=T)
CODE_DIRECTORY <- paste(BASE_DIRECTORY,"Code",sep="")                 # WinBUGS code are stored in this directory.
if(is.na(file.info(CODE_DIRECTORY)[1,"isdir"])) dir.create(CODE_DIRECTORY,recursive=T)
>>>>>>> origin/master

#################################################################################################################
# LOAD FUNCTIONS AND PACKAGES
#################################################################################################################
# Load functions from GitHub
setwd(BASE_DIRECTORY)
source("MAPS_AllFunctions.r")     

# Check if packages are already installed. If not, install and load.
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
# -- COL1 labeled "station" indicating the names of banding MAPS stations
# -- COL2 labeled "population" as numbers designating biological population in which the station belongs to.
# This file should be located in DATA_DIRECTORY and called "station_pop.csv".   

setwd(PUBLICDATA_DIRECTORY)
population_map = read.csv("station_pop.csv",header=T)      # A user-defined csv file where stations are grouped into separate biological populations.
                                                           # In the manuscript, we chose to treat each MAPS location (comprised of multiple stations) as its own biological population.

#################################################################################################################
# LOAD REGIONAL TREND DATA  (we used estimates derived from BBS data- see http://www.mbr-pwrc.usgs.gov/bbs/trend/tf13.html)
#################################################################################################################

### For reference:  BBS trend for focal species from Ryu et al.2015 ###
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

BBS.Trend <- Assign.Trend(estimate=0.29, lcl=-0.20, ucl=0.80) # Assign trend (point estimate, lcl, and ucl) for each species based on BBS Trend. 

#################################################################################################################
# SET UP INFORMATIVE 'DEBUG' FILE
#################################################################################################################

DEBUG_FILENAME <- paste(SPECIES_CODE,"_debug.txt",sep="")
InitializeDebugFile(dir=DATA_DIRECTORY,filename=DEBUG_FILENAME)

#################################################################################################################
# SET UP INFORMATIVE 'INTERMEDIATE RESULTS' FILE
#################################################################################################################

RESULTS_FILENAME <- paste(SPECIES_CODE,"_intermediateResults.txt",sep="")
InitializeResultsFile(dir=RESULTS_DIRECTORY,filename=RESULTS_FILENAME)

#################################################################################################################
# SET UP 'POPULATION MODEL SUMMARY' FILE
#################################################################################################################

METAPOP_FILENAME <- paste(SPECIES_CODE, ".mp", sep="")   
POPMODELSUMMARY_FILENAME <- paste(SPECIES_CODE,"_popmodelsummary.txt",sep="")
InitializePopModelFile(dir=RESULTS_DIRECTORY,filename=POPMODELSUMMARY_FILENAME)

#################################################################################################################
# READ IN RAW DATA FROM CSV FILES
#################################################################################################################

BandDataFile <- sprintf("%s_BandDataAllMonths.csv",SPECIES_CODE)  # Name of CSV file containing raw band recapture data
EffortDataFile <- "Effort.csv"                                    # Name of CSV file containing raw information on survey effort at each banding station 
MAPSData <- ReadRawData(dir=PUBLICDATA_DIRECTORY, BandData=BandDataFile, EffortData=EffortDataFile, StationToPop=population_map)    

# Save output in DATA_DIRECTORY.
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "MAPSData.RData", sep="_")
save(MAPSData, file=filename)

################################################################################################################
# CREATE DATA STRUCTURES FOR CAPTURE MARK RECAPTURE FROM THE RETRIEVED MAPS DATA (IN CAPTURE HISTORY FORMAT)
################################################################################################################

CMRData <- FormatForCMR(MAPSData=MAPSData, dir=DATA_DIRECTORY)       # NOTE: With a 2.4GHz processor, this code takes about 20 minutes to run.

# Save output in DATA_DIRECTORY.
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "CMRData.RData", sep="_")
save(CMRData, file=filename)

#######################################################################################################
# CREATE DATA STRUCTURES FOR RMARK FROM THE CAPTURE MARK RECAPTURE DATA
#######################################################################################################

RMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=MAPSData, dir=DATA_DIRECTORY, AddDensity=FALSE)

# Save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "RMarkData.RData", sep="_")
save(RMarkData, file=filename)

#######################################################################################################
# CREATE DATA STRUCTURES FOR RMARK FOR TREND ESTIMATION (LAMBDA)
#######################################################################################################

RMarkDataTrend <- FormatForRMarkTrend(CMRData=CMRData, dir=DATA_DIRECTORY)

# Save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "RMarkDataTrend.RData", sep="_")
save(RMarkDataTrend, file=filename)

######################################################################################################
# RUN MARK MODELS TO COMPUTE SURVIVAL RATE 
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, only run the initial MARK models. No density, just time-dependent models.
MarkResults_Time <- Run.Models(RMarkData=RMarkData, initial=0, DensityModel = FALSE) # NOTE: With a 2.4GHz processor, this code takes about 10 minutes to run.   

# Save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Time.RData", sep="_")
save(MarkResults_Time, file=filename)

######################################################################################################
# RUN MARK MODEL TO COMPUTE TREND 
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, only run the Pradel model with RMarkData formatted for the analysis
MarkResults_Trend <- Run.Trend.Model(RMarkData=RMarkDataTrend) 

# Save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Trend.RData", sep="_")
save(MarkResults_Trend, file=filename)

# Assign trend for correction for all demographic parameters
Estimated.Trend <- Assign.Trend(estimate=MarkResults_Trend[[1]]$results$real$estimate[2], 
                               lcl=MarkResults_Trend[[1]]$results$real$lcl[2], 
                               ucl=MarkResults_Trend[[1]]$results$real$ucl[2])
Estimated.Trend

######################################################################################################
# EXTRACT CAPTURE PROBABILITIES FROM MARK RESULTS
######################################################################################################
# Use model "S (~st + time + st:time + first_cap_bin:trans), p(~st + effort)" (model.no=1) to calculate capture probabilities. 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time.
p.table <- PCapResults(RMarkResults=MarkResults_Time, maps.ddl=RMarkData$maps.ddl, band.data=MAPSData$band.data, model.no=1)   # NOTE: With a 2.4GHz processor, this code takes about 20 minutes to run for species with a large dataset.

# Save output in DATA_DIRECTORY.
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "PTable.RData", sep="_")
save(p.table, file=filename)

#######################################################################################################
# ADD DENSITY COVARIATE TO THE RMARK DATA --- FOR ESTIMATING DENSITY-DEPENDENCE IN FINAL SURVIVAL ANALYSIS 
######################################################################################################

finalRMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=MAPSData, dir=DATA_DIRECTORY, AddDensity=TRUE) 

# Save output in DATA_DIRECTORY.
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "finalRMarkData.RData", sep="_")
save(finalRMarkData, file=filename)


######################################################################################################
# RUN MARK MODELS TO COMPUTE DENSITY-DEPENDENCE FUNCTION FOR SURVIVAL
######################################################################################################
setwd(RESULTS_DIRECTORY) # Save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY.

# Here, run the density-dependent MARK models. 
MarkResults_Density <- Run.Models(RMarkData=finalRMarkData, initial=0, DensityModel = TRUE)    # NOTE: With a 2.4 GHz processor, this code takes about 5 minutes to run.

# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Density.RData", sep="_")
save(MarkResults_Density, file=filename)


######################################################################################################
# EXTRACT (APPARENT) SURVIVAL RATES FROM MARK RESULTS
######################################################################################################
# Use "S(~st+first_cap_bin:trans), p(~st+effort)" (model.no=2) to get time-constant apparent survival estimates. 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time.
SurvivalResults <- ApparentS(RMarkResults=MarkResults_Time, model.no = 2) 

# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SurvivalResults.RData", sep="_")
save(SurvivalResults, file=filename)


######################################################################################################
# RUN VARIANCE COMPONENT ANALYSIS TO ESTIMATE TEMPORAL VARIABILITY IN SURVIVAL
######################################################################################################
# Use "S (~st + time + st:time + first_cap_bin:trans), p(~st + effort)" (model.no=1) to get temporal variability of apparent survival rates. 

MarkResults_Time  # Check the model number in MarkResults_Time and set the corresponding model number in the following code.
SurvivalTempVarResults <- VarianceComponent(RMarkResults=MarkResults_Time, model.no = 1) 

# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SurvivalTempVarResults.RData", sep="_")
save(SurvivalTempVarResults, file=filename)
  

######################################################################################################
# RUN WINBUGS MODELS TO COMPUTE FECUNDITY
######################################################################################################

FecundityResults <- EstimateFecundity(p.table=p.table, MinAdults=4) # Set number of captured adults under which the relative density data will be discarded.
                                                                    # If model does not run, try different priors and initial values.
                                                                    # NOTE: With a 2.4 GHz processor, this code takes about 100 minutes to run.
# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "FecundityResults.RData", sep="_")
save(FecundityResults, file=filename)


######################################################################################################
# SELECT OUT KEY RESULTS FOR CALCULATING DEMOGRAPHIC PARAMETERS USED IN POPULATION MODELING 
######################################################################################################
# Calculate density function for S from model "S(~st + maps_density + first_cap_bin:trans)p(~st + effort)" (model.no=1).

MarkResults_Density   # Check the model number in MarkResults_Density and set the corresponding model number in the following code.
PopModelData <- SummarizeForPopModel(RMarkData=finalRMarkData, AppS=SurvivalResults, STempVar=SurvivalTempVarResults, 
                                     MarkResults=MarkResults_Density, Fec=FecundityResults, model.no=1)       

# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "PopModelData.RData", sep="_")
save(PopModelData, file=filename)


######################################################################################################
# CORRECT ALL DEMOGRAPHIC PARAMETERS FOR APPARENT SURVIVAL AND CREATE STAGE AND SD MATRIX
######################################################################################################

SummaryMP<-SummaryMP(Data=PopModelData, TrendData=BBS.Trend)   # correction using BBS Trend

SummaryMP<-SummaryMP(Data=PopModelData, TrendData=Estimated.Trend) # correction using Estimated Trend

# Save output in RESULTS_DIRECTORY.
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "SummaryMP.RData", sep="_")
save(SummaryMP, file=filename)


######################################################################################################
# BUILD RAMAS ".MP" FILE WITH PARAMETERS FROM THE ABOVE ANALYSES 
######################################################################################################

WriteMasterMPFile(Result=SummaryMP)  

################################### END OF CODE ################################################




#######################################################################################################
# DICTIONARY OF WORKSPACE VARIABLES
#######################################################################################################

#############
# Directory variables

# BASE_DIRECTORY                 -- A complete directory path that include folders named "Public Dataset", "Pop models", 
#                                   "Results", "Datasets", and "Code".
# CODE_DIRECTORY                 -- The directory that contains WinBUGS source code for running the analysis.
# DATA_DIRECTORY                 -- The directory that contains raw and intermediate data used for the analysis. 
# POPMODELS_DIRECTORY            -- The directory that contains the RAMAS population models and the project-specific DLL.
# RESULTS_DIRECTORY              -- The directory that contains all final results (Population model summary text file, Intermediate Results text file, and MARK output and model results).

#############
# Data extraction controls

# BEGINYEAR                      -- First year of study period
# ENDYEAR                        -- Final year of study period
# SPECIES_CODE                   -- Standard abbreviation code for bird species: e.g. NOCA for Northern Cardinal

#############
# Simulation controls

# CARRYING_CAPACITY              -- Carrying capacity to be specified in the population model. Not a fitted parameter. Can be changed post hoc if desired.
# CORRELATION                    -- Degree to which stochastic fluctuations in survival and fecundity are correlated. 
#                                   (e.g. year with high survival tends to also be associated with high fecundity)		   
# INITIAL_ABUNDANCE              -- Inital total abundance to be specified in the population model. Not a fitted parameter. Can be changed post hoc if desired.		
# METAPOP_VERSION                -- Version of Ramas Metapop. Should be version 6.0 compiled for 64 bit architecture
# REPLICATES                     -- Number of simulation replicates. Can be changed post hoc if desired.							   

############
# Data structures

# population_map                 -- Data frame associating each MAPS station in the study with specific populations. In effect, parameters will be estimated separately for each population specified in this file.
# MAPSData                       -- List of data frames containing data extracted from the MAPS database, in addition to population_map
# CMRData	                       -- List that includes the master capture history data, and three vectors denoting the time periods for primary and secondary sampling occasions 
# RMarkData                      -- Input files for MARK without density covariates
# finalRMarkData                 -- Input files for MARK with density covariates
# p.table                        -- Table of capture probabilities and abundances calculated based on initial MARK time models 

############
# Functions

# InitializeDebugFile            -- Initializes a text file for storing debug information. Saved in DATA_DIRECTORY.
# InitializeResultsFile          -- Initializes a text file for storing results information. Saved in RESULTS_DIRECTORY.
# InitializePopModelFile         -- Initializes a text file for storing population summary information. Saved in RESULTS_DIRECTORY.
# ReadRawData                    -- Reads in relevant data from the MAPS database. 
# FormatForCMR                   -- Turn the data extracted from the MAPS database into a "capture history" format. 
# FormatForRMark                 -- Converts capture history data into the appropriate format for RMark (.ddl, .process).
# Run.Models                     -- Runs MARK models for survival analysis.
# PCapResults                    -- Calculates capture probabilities separately for juveniles and adults based on initial MARK model result.
# ApparentS                      -- Computes and extracts apprarent survival rates based on MARK model result.
# VarianceComponent              -- Estimates temporal variability in survival using variance component analysis. 
# EstimateFecundity              -- Runs WinBUGS for fecundity analysis. 
# SummarizeForPopModel           -- Collects all parameter values required to build a population model.
# SummaryMP                      -- Corrects all demographic parameters in the population model for apparent survival and creates stage and SD matrix.
# WriteMasterMPFile              -- Writes an MP file to run population simulations/projections for the focal species, using parameters derived from the MAPS data.
######################################################################################################################
































