setwd(DATA_DIRECTORY)

filename <- paste(SPECIES_CODE, "MAPSData.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "CMRData.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "RMarkData.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "PTable.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "finalRMarkData.RData", sep="_")
load(filename)




setwd(RESULTS_DIRECTORY)

filename <- paste(SPECIES_CODE, "MarkResults_Time.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "MarkResults_Density.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "SurvivalResults.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "SurvivalTempVarResults.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "FecundityResults.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "PopModelData.RData", sep="_")
load(filename)

filename <- paste(SPECIES_CODE, "SummaryMP.RData", sep="_")
load(filename)


SPECIES_CODE
FecundityResults$Mean.rD
FecundityResults$SD.rD
