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

#########################
### Choose user (for managing files/directories)

CHLOE = F
CHLOE_LAPTOP = F
LAB_COMPUTER = F
KEVIN = T
EVA = F
RESIT = F

#########################
####  Define the scope of the analysis

SPECIES_CODE <- "NOCA"           # Focal Species
BEGINYEAR    <- 1994             # First year of study period
ENDYEAR      <- 2012            # Final year of study period
MAPSyears=c(BEGINYEAR:ENDYEAR)   # MAPSyears: vector of all years of interest

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
ni <- 1000      # Number of MCMC iterations per chain
nc <- 1           # Number of chains
nb <- 500       # Burn-in length
nt <- 1          # Thinning rate

if(KEVIN) BUGSdir <- 'C:\\Users\\Kevin\\Documents\\Employment\\ESF\\Bog Turtle\\DATA\\software\\BUGS\\WinBUGS14'    # Local WinBUGS directory 
if(CHLOE) BUGSdir <- "C:/Users/Chloe/Downloads/winbugs14/WinBUGS14"
if(LAB_COMPUTER) BUGSdir <- "C:/Users/Chloe/Downloads/winbugs14/WinBUGS14"

#################################################################################################################
# SET DIRECTORIES AND LOAD PACKAGES
#################################################################################################################

#########################
### Set directory variables (and make the directory if it doesn't exist)

if(KEVIN) BASE_DIRECTORY <- "C:\\Users\\Kevin\\Dropbox\\MAPS Project\\"
if(CHLOE) BASE_DIRECTORY <- "C:\\Users\\Chloe\\Dropbox\\MAPS Project\\"
if(LAB_COMPUTER) BASE_DIRECTORY <- "C:\\Users\\Chloe\\Dropbox\\MAPS Project\\"
if(RESIT) BASE_DIRECTORY <- "C:\\Users\\Resit\\Dropbox\\MAPS Project\\"

POPMODELS_DIRECTORY <- paste(BASE_DIRECTORY,"Pop models",sep="")
if(is.na(file.info(POPMODELS_DIRECTORY)[1,"isdir"])) dir.create(POPMODELS_DIRECTORY)                      # create new directory if it doesn't exist already
RESULTS_DIRECTORY <- paste(BASE_DIRECTORY,"Results\\final",sep="")  # for now, put the results in a separate folder
if(is.na(file.info(RESULTS_DIRECTORY)[1,"isdir"])) dir.create(RESULTS_DIRECTORY)
DATA_DIRECTORY <- paste(BASE_DIRECTORY,"Data\\final",sep="")  # for now, put the results in a separate folder
if(is.na(file.info(DATA_DIRECTORY)[1,"isdir"])) dir.create(DATA_DIRECTORY)
CODE_DIRECTORY <- paste(BASE_DIRECTORY,"code",sep="")
if(is.na(file.info(CODE_DIRECTORY)[1,"isdir"])) dir.create(CODE_DIRECTORY)


setwd(DATA_DIRECTORY)

### If running for the first time, install required packages in R
#install.packages("RMark")
#install.packages("R2WinBUGS")
#install.packages("MASS")
#install.packages("gtools")
#install.packages("foreign")
#install.packages("RODBC")
#install.packages("doBy")

### Load required packages in R
suppressMessages(suppressWarnings(require(RMark)))
suppressMessages(suppressWarnings(require(R2WinBUGS)))
suppressMessages(suppressWarnings(require(MASS)))
suppressMessages(suppressWarnings(require(gtools)))
suppressMessages(suppressWarnings(require(foreign)))
suppressMessages(suppressWarnings(require(RODBC)))
suppressMessages(suppressWarnings(require(doBy)))

########################
###  Define the association between MAPS locations and distinct populations
# Format as CSV file with two columns:
# -- COL1 labeled "loc", indicating the names of MAPS locations
# -- COL2 labeled "pop", designating the corresponding biological population.

setwd(DATA_DIRECTORY)
population_map = read.csv("loc_pop.csv",header=T)      # NOTE: we chose to treat each location as its own biological population


#################################################################################################################
# LOAD REGIONAL TREND DATA  (we used estimates derived from BBS data- see XXXXXX  [website])
#################################################################################################################

############
###  REGIONAL TREND
###      - TODO: see if this can be automated. Doesn't seem to be a web service associated with BBS
###      -   if not, maybe embed a "dictionary" with the focal species trends within the R code?

## From BBS web site, get percent change per year, assign it to BBS.trend (usually a number between -5 and +5)
## Here are the trends used in this study:
  
setwd(BASE_DIRECTORY)
BBS_TrendFile <- "BBS_Trends.csv"            ### TODO: Get the real numbers 
sink(BBS_TrendFile)
cat("SPECIES,  TREND,  LCL, UCL
NOCA,   0.30,   0.20,   0.40
WEVI,   0.51,  0.41,   0.61
GRCA,   0.46,   0.36,   0.56
COYE,   -0.85,   -0.95,   -0.75
WOTH,   -2.16,   -2.26,  -2.06
HOWA,   1.85,   1.75,  1.95
YBCH,   -0.27,   -0.37, -0.17
CACH,   -0.37,   -0.47,  -0.27
BCCH,   0.29,    0.19,   0.39")
sink()
  
BBSTrend_DF <- read.csv(BBS_TrendFile,header=T)
BBSTrend_DF$SPECIES = as.character(BBSTrend_DF$SPECIES)
BBS.trend <- list()
BBS.trend$estimate <- subset(BBSTrend_DF,SPECIES==SPECIES_CODE)$TREND[1]      # central point estimate
BBS.trend$lcl <- subset(BBSTrend_DF,SPECIES==SPECIES_CODE)$LCL[1]      # lower confidence limit
BBS.trend$ucl <- subset(BBSTrend_DF,SPECIES==SPECIES_CODE)$UCL[1]      # upper confidence limit


#################################################################################################################
# LOAD FUNCTIONS
#################################################################################################################
setwd(CODE_DIRECTORY)
source("MAPS_AllFunctions.r")

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
METATPOP_FILENAME <- paste(SPECIES_CODE, ".mp", sep="")

POPMODELSUMMARY_FILENAME <- paste(SPECIES_CODE,"_popmodelsummary.txt",sep="")
InitializePopModelFile(dir=RESULTS_DIRECTORY,filename=POPMODELSUMMARY_FILENAME)

#################################################################################################################
# PROCESS MAPS DATA FROM ACCESS DATABASE VIA ODBC
#################################################################################################################

ProcessedData <- RetrieveData(dir=DATA_DIRECTORY, odbc.source="maps", population_map=population_map)

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "ProcessedData.RData", sep="_")
save(ProcessedData, file=filename)    # store the processed data in permanent storage for later retrieval


#######################################################################################################
# CREATE DATA STRUCTURES FOR CAPTURE MARK RECAPTURE FROM THE RETRIEVED MAPS DATA (capture history format)
#######################################################################################################

CMRData <- FormatForCMR(MAPSData=ProcessedData, dir=DATA_DIRECTORY)       # this function can take a very long time (~20 mins)

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "CMRData.RData", sep="_")
save(CMRData, file=filename)


#######################################################################################################
# CREATE DATA STRUCTURES FOR RMARK FROM THE CAPTURE MARK RECAPTURE DATA
#######################################################################################################

RMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=ProcessedData, dir=DATA_DIRECTORY, AddDensity=FALSE)

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "RMarkData.RData", sep="_")
save(RMarkData, file=filename)


######################################################################################################
# RUN MARK MODELS TO COMPUTE SURVIVAL RATE 
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, only run the initial MARK models. No density, just time-dependent models.
MarkResults_Time <- Run.Models(RMarkData=RMarkData, initial=0, DensityModel = FALSE) # NOTE: may take 10-20 minutes to run    

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Time.RData", sep="_")
save(MarkResults_Time, file=filename)


######################################################################################################
# EXTRACT CAPTURE PROBABILITIES FROM MARK RESULTS
######################################################################################################
# Use model S (~st + time + st:time + first_cap_bin:trans), p(~st + effort) 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time
p.table <- PCapResults(RMarkResults=MarkResults_Time, maps.ddl=RMarkData$maps.ddl, band.data = ProcessedData$band.data, model.no = 1)   # NOTE: may take 10-20 minutes to run

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "PTable.RData", sep="_")
save(p.table, file=filename)


#######################################################################################################
# ADD DENSITY COVARIATE TO THE RMARK DATA --- FOR ESTIMATING DENSITY-DEPENDENCE IN FINAL SURVIVAL ANALYSIS 
######################################################################################################

finalRMarkData <- FormatForRMark(CMRData=CMRData, MAPSData=ProcessedData, dir=DATA_DIRECTORY, AddDensity=TRUE)

# save output in DATA_DIRECTORY
setwd(DATA_DIRECTORY)
filename <- paste(SPECIES_CODE, "finalRMarkData.RData", sep="_")
save(finalRMarkData, file=filename)


######################################################################################################
# RUN MARK MODELS TO COMPUTE DENSITY-DEPENDENCE FUNCTION FOR SURVIVAL
######################################################################################################
setwd(RESULTS_DIRECTORY) # save all MARK output files (.inp, .out, .res) in RESULTS_DIRECTORY

# Here, run the density-dependent MARK models. 
MarkResults_Density <- Run.Models(RMarkData=finalRMarkData, initial=0, DensityModel = TRUE)    # NOTE: may take 10-20 minutes to run

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "MarkResults_Density.RData", sep="_")
save(MarkResults_Density, file=filename)


######################################################################################################
# EXTRACT (APPARENT) SURVIVAL RATES FROM MARK RESULTS
######################################################################################################
# Use S(~st+first_cap_bin:trans), p(~st+effort) 

MarkResults_Time  # Set to the corresponding model.no from MarkResults_Time
SurvivalResults <- ApparentS(RMarkResults=MarkResults_Time, model.no = 3) 

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

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "FecundityResults.RData", sep="_")
save(FecundityResults, file=filename)


######################################################################################################
# PRINT OUT KEY RESULTS FOR POPULATION MODELING (stage matrix, temporal variability, density dependence)
######################################################################################################
# Calculate density function for S from model S(~st + maps_density + first_cap_bin:trans)p(~st + effort)
MarkResults_Density # model 2 in Density Mark Model

PopModelData <- SummarizeForPopModel(RMarkData=finalRMarkData, AppS=SurvivalResults, STempVar=SurvivalTempVarResults, 
                                     MarkResults=MarkResults_Density, Fec=FecundityResults, model.no=2)       

# save output in RESULTS_DIRECTORY
setwd(RESULTS_DIRECTORY)
filename <- paste(SPECIES_CODE, "PopModelData.RData", sep="_")
save(PopModelData, file=filename)


######################################################################################################
# BUILD RAMAS ".MP" FILE WITH PARAMETERS FROM THE ABOVE ANALYSES 
######################################################################################################

WriteMasterMPFile(Data=PopModelData, TrendData=BBS.trend)   


################################### END OF CODE ################################################






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
# METAPOP_VERSION                -- Version of Ramas Metapop. Should be version 6.0 compiled for 64 bit architecture (TODO)
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
































