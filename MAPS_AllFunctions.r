########################################################################################################################
####  R Functions used for accessing and processing MAPS data, building datafiles for RMark,
####  estimating demographic parameteters, and assembling a population model. 

####  [From Ryu et al., "Developing Population Models with Data from Marked Individuals"] 
########################################################################################################################

##################################################################################
# FUNCTION 'loadPackage'
# 
# CHECK FOR PACKAGES AND INSTALL/LOAD PACKAGES FROM CRAN
##################################################################################

loadPackage <- function(pkg){

  if(pkg %in% rownames(installed.packages()) == FALSE) {suppressMessages(suppressWarnings(install.packages(pkg)))}
  eval(parse(text=sprintf("suppressMessages(suppressWarnings(require(%s)))",pkg)), envir= .GlobalEnv)

}


##################################################################################
# FUNCTION 'Assign.Trend'
#
# READ IN TREND DATA
##################################################################################

Assign.Trend <- function(estimate=0,lcl=0,ucl=0){
  trend <- list()
  trend$estimate = estimate
  trend$lcl = lcl
  trend$ucl = ucl
  return(trend)
}


##################################################################################
# FUNCTION 'ConvertTrend'
#
# CONVERT BBS TREND (% CHANGE IN LAMBDA) INTO REAL LAMBDA VALUES
##################################################################################

ConvertTrend <- function(estimate, lcl, ucl){
    
  Real.BBSTrend.estimate <- 1.0 + estimate /100.0
  Real.BBSTrend.lcl <- 1.0 + lcl /100.0
  Real.BBSTrend.ucl <- 1.0 + ucl /100.0
  
  Real.BBSTrend <- Assign.Trend(estimate=Real.BBSTrend.estimate, lcl=Real.BBSTrend.lcl, ucl=Real.BBSTrend.ucl) 
  
  return(Real.BBSTrend)
} # END OF FUNCTION 'ConvertTrend'


##################################################################################
# FUNCTION 'ExtractTrend'
#
# READ IN THE RESULT OF TREND ESTIMATE FROM MARK PRADEL MODEL
##################################################################################

ExtractTrend <- function(MarkResults_Trend, model.no){
    
  real<-MarkResults_Trend[[model.no]]$results$real
  
  MAPSTrend.estimate<-real[grep("Lambda",rownames(real)), "estimate"]
  MAPSTrend.lcl<-real[grep("Lambda",rownames(real)), "lcl"]
  MAPSTrend.ucl<-real[grep("Lambda",rownames(real)), "ucl"]
  
  MAPSTrend <- Assign.Trend(estimate=MAPSTrend.estimate, lcl=MAPSTrend.lcl, ucl=MAPSTrend.ucl) 
  
  return(MAPSTrend)
} # END OF FUNCTION 'ExtractTrend'


#################################################################################
# FUNCTION 'tryCatch.W.E'
#
# CATCH AND STORE ERRORS WHILE ALLOWING THE PROGRAM TO KEEP RUNNING
#################################################################################

tryCatch.W.E <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
   }
   list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                    warning = w.handler),warning = W)
}


################################################################################
# FUNCTION 'InitializeDebugFile'
#
# SET UP INFORMATIVE DEBUG FILE
#
# OUTPUT:
#    introductory lines in 'Debug' text file in DATA_DIRECTORY
################################################################################

InitializeDebugFile <- function(dir, filename){
	   # clear file
	setwd(dir)
	sink(filename)
	cat(" ")
             ### file header
	cat("
	----------------------------------------------------------------------
	DEBUG MESSAGES FOR MAPS TO MODELS ANALYSIS (warnings and errors) 

	Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" 

	This document contains details about the \"MAPS to Models\" analysis- warning and error messages. Please read this
	document carefully before running any population models.

	The organization of the document reflects the order of operations in the workflow: 
		DATA EXTRACTION >> SURVIVAL ESTIMATION (Program MARK) >> FECUNDITY ESTIMATION (WinBUGS) >> POPULATION MODEL ASSEMBLY (Ramas Metapop)

    Please notify the authors to suggest improvements to this document! 		
	")
	sink()
}


################################################################################
# FUNCTION 'ToDebugFile'
#
# APPEND TEXTS TO INFORMATIVE DEBUG FILE
################################################################################

ToDebugFile <- function(InText, dir, filename){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}


####################################################################################
# FUNCTION 'InitializeResultsFile'
#
# SET UP INTERMEDIATE RESULTS FILE
#
# OUTPUT:
#    introductory lines in 'Intermediate Results' text file in RESULTS_DIRECTORY
####################################################################################

InitializeResultsFile <- function(dir, filename){
	setwd(dir)
	sink(filename)
	cat(" ")
	      ## Set up the file header

	cat("
----------------------------------------------------------------------
INTERMEDIATE RESULTS FOR MAPS TO MODELS ANALYSIS

Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" 

This document contains details about the \"MAPS to Models\" analysis results. Please read this
document carefully before running any population models derived from this analysis.

Please see the \"popmodelsummary\" folder for a full description of the final population model.

The organization of the document reflects the order of operations in the workflow: 
	DATA EXTRACTION >> SURVIVAL ESTIMATION (Program MARK) >> FECUNDITY ESTIMATION (WinBUGS) >> POPULATION MODEL ASSEMBLY (Ramas Metapop)
	
	")
	sink()
}


####################################################################################
# FUNCTION 'ToResultsFile'
#
# APPEND TEXTS TO INTERMEDIATE RESULTS FILE
####################################################################################

ToResultsFile <- function(InText, dir, filename){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}


####################################################################################
# FUNCTION 'InitializePopModelFile'
#
# SET UP POPULATION MODEL SUMMARY FILE
#
# OUTPUT:
#    introductory lines in 'Population model summary' text file in RESULTS_DIRECTORY
####################################################################################

InitializePopModelFile <- function(dir, filename){
  setwd(dir)
  sink(filename)
  cat(" ")
  ## Set up the file header
  
  cat(
  sprintf("
****************************************
*****  POPULATION MODEL SUMMARY ********
****************************************

Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" 

This file contains a summary of the population model resulting from the analysis of the mark-recapture data. \n
  In addition to this file, check the following files created from this analysis: \n

  1. [%s] is a debug file with error and warning messages created during the analysis.
  2. [%s] contains all output and intermediate results created during the analysis.
  3. [%s] is the input file for RAMAS Metapop and RAMAS GIS that includes the final population model.

  Species: %s

  A. General Assumptions:

  * Population is censused after breeding.
  * Individuals start breeding at age 12 months.
  * First-year (juvenile) survival rate (Sj) may be different than survival rate in later years (adult survival, Sa)
  * Survival rate does not depend on age after the first year.
  * Average fecundity is the same for juveniles (1 year olds) and adults (2+ year olds).
  * Only females are modeled.
  * Fecundity (F) is the number of daughters per female.
  * The stage matrix has the following structure:

 \t\t\t|------|-------| 
 \t\t\t| F*Sj | F*Sa  |
 \t\t\t|------|-------|
 \t\t\t| Sj   | Sa    |  
 \t\t\t|------|-------|
 
	", DEBUG_FILENAME, RESULTS_FILENAME, METAPOP_FILENAME, SPECIES_CODE)
  )
  sink()
}


####################################################################################
# FUNCTION 'ToPopModelFile'
#
# APPEND TEXTS IN POPULATION MODEL SUMMARY FILE
####################################################################################

ToPopModelFile <- function(InText, dir, filename){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}


######################################################################################
# FUNCTION 'ReadRawData'
#
# READ IN THE RAW BAND AND EFFORT DATA AND CREATE NEW EFFORT DATAFRAME BASED ON USER-DEFINED POPULATION
#
# ARGUMENTS:
#			 'dir'          -- project data directory
#      'BandData'     -- text string indicating name of CSV file containing raw capture data
#      'EffortData'   -- text string indicating name of CSV file containing raw effort data 
#      'StationToPop' -- text string indicating name of CSV file containing population information 
######################################################################################

ReadRawData <- function(dir, BandData, EffortData, StationToPop){
   setwd(dir)
   band.data_allMon <- read.csv(BandData,h=T)   
   
   # add 'pop' to band.data_allMon
   band.data_allMon$pop<-NA
   for(i in 1:nrow(band.data_allMon)){
     index<-which(StationToPop$station==as.character(band.data_allMon$station[i]))
     band.data_allMon$pop[i]<-StationToPop$population[index]
   }
   
   # for capture history, select out breeding months (May~August)
   band.data<-band.data_allMon[ which(band.data_allMon$month %in% c(5,6,7,8)), ]  
   effort_bystation <- read.csv(EffortData,h=T)
      
    # re-create effort based on user-defined population
   unique.pop<-sort(unique(StationToPop$population))
   yrs<-sort(unique(effort_bystation$year))
   months<-sort(unique(effort_bystation$month))
    # create a data frame structure for new effort 
   effort<-data.frame(pop=rep(unique.pop, each=length(yrs)*length(months)), year=rep(yrs, each=length(months)), month=rep(months, times=length(yrs)), effort=NA)
    # add pop to effort_bystation
   effort_bystation_pop<-merge(effort_bystation, StationToPop, by="station")
    # sum of effort for stations that belong to same population
    # result is an array
   effort_temp <- with(effort_bystation_pop, tapply(effort, list(population,year,month), sum))
   
    # put the sum(effort) to corresponding year/month 
   for(i in 1:length(unique.pop)){
     for (j in 1:length(yrs)){
       for (k in 1:length(months)){
         index<-which((effort$pop==unique.pop[i])&(effort$year==yrs[j])&(effort$month==months[k]))
         effort$effort[index]<-effort_temp[i, j, k]
       }
     }
   }
   
    # if effort is NA, remove that row
   effort<-na.omit(effort)
   
   data <- list()
   data$band.data_allMon <- band.data_allMon
   data$band.data <-band.data
   data$effort <- effort
   data$station.pop <- StationToPop
   return(data)  
   
} # END OF FUNCTION 'ReadRawData'


######################################################################################
# FUNCTION 'FormatForCMR'
#
# RESHAPE RAW DATA FROM MAPS INTO A 'CAPTURE HISTORY' FORMAT TO PERFORM CAPTURE-RECAPTURE ANALYSIS 
#
# ARGUMENTS:
#			'MAPSData' -- Results from function 'ReadRawData'
#			'dir'      -- Project data directory
# 
# OUTPUT:
#     'CaptureHistory.txt' listing capture histories of all individuals in DATA_DIRECTORY
#     lines in 'Intermediate Results' text file in RESULTS_DIRECTORY
#######################################################################################

FormatForCMR <- function(MAPSData, dir){	
  
    # Extract the processed data from MAPS
	data <- MAPSData$band.data
	band.data_allmon <- MAPSData$band.data_allMon
	effort <- MAPSData$effort
	station.pop <- MAPSData$station.pop
	
	CMR_Data <- list()
	  
		# Structuring input file for processing
	work=data[,c("band","captureyear","month","birthyear","agegroup","actualage","loc","station","pop","CP","BP","freq")]

    # combine captureyear and month
	work2=cbind(work[c("band")], year_month=paste(work[,c("captureyear")],work[,c("month")],sep="_"),  
              work[c("loc","station","pop","birthyear","agegroup","actualage","freq","CP","BP")])

    ###############################
	  ## MAKE "FAKE BIRDS" TO ENSURE THAT ALL POPULATIONS/TIMES ARE REPRESENTED IN THE CAPTURE HISTORY
	  # Making fake birds to create uniform secondary occasions for each primary occasion (year)
	  # make sure at least one individual is in the dataset for each year-month period
	  # if missing, add a "fake bird" with ID "99999"
	
  capNcaps <- nrow(work2)
	ndx <- capNcaps
	work2$year_month <- as.character(work2$year_month)
	capYears <- as.numeric(substr(unique(as.character(work2$year_month)),1,4))
	capNYears <- length(unique(capYears))
	capNOccasions <- length(capYears)
	capMonths <- as.numeric(substr(unique(as.character(work2$year_month)),6,6))
	for(i in min(capYears):max(capYears)){
	  for(j in 5:8){
		test <- length(which((i==capYears)&(j==capMonths)))
		if(test==0){
		  ndx <- ndx+1
		  work2 <- rbind(work2,work2[1,])
		  work2$band[ndx] = 99999
		  work2$year_month[ndx] = paste(i,"_",j,sep="") 
		  
		  work2[ndx,3:ncol(work2)] <- work2[1,3:ncol(work2)] 
		}
	  }
	}
	work2$year_month <- as.factor(work2$year_month)   # reconvert to factor

		# work3 puts the bands in order by year_month 
	work3=work2[order(work2$year_month), ]	
				
    #### make sure only a single obs for each unique individual and time period   [NOTE: takes a while to run!]
		# leave only one capture for each year-month for each individual
	work_reshape<-work3
  
	indivs <- unique(work_reshape$band) 	# unique bands
	tps <- as.character(unique(work_reshape$year_month))	# unique year_month
	flag <- array(0,dim=c(length(indivs),length(tps))) 	# array of capture frequencies (0 or 1) with indivs as rows and year_month as columns
	for(i in 1:length(indivs)){
	  for(t in 1:length(tps)){
		ndx <- which((work_reshape$year_month==tps[t])&(work_reshape$band==indivs[i]))
		flag[i,t] <- length(ndx) 	# the total number captured for indiv[i] in tps[t](for example, the number of band 296 captured in 1994_5)
		if(flag[i,t]>1){			# if captured more than once in year_month, remove
		  newndx <- ndx[-1]   # rows to remove
		  work_reshape <- work_reshape[-newndx,]
		}	
	  }
	}

	  #check which individuals were caught more than once in each year_month time period
	temp <-apply(flag,1,function(t) ifelse(length(which(t>1))>0,1,0))  
	ndx <- which(temp==1) 
	flags <- indivs[ndx] ##individuals caught more than once in each year_month 
	
		# "explode" the dataset so that each survey event gets its own column 
	suppressWarnings(
  test<-reshape(work_reshape, timevar='year_month', direction='wide', idvar='band', v.names=c('agegroup','actualage','CP','BP'), sep='')  
  )
    # determine the columns that will make up the capture history
		# which are columns with 'agegroup' 
	indx=grep("agegroup",x=names(test)) 	#selects columns that contain 'agegroup' in them

		#Take out the year and month for every agegroup for generating primary and secondary trapping occasions for robust design
	rdyear=as.numeric(substr(names(test[,indx]),9,12))	#Take out year (9th to 12th position) from ex) agegroup1994_5
	rdmonth=as.numeric(substr(names(test[,indx]),14,14))	#Take out month (14th position) from ex) agegroup1994_5
	rdn_year=length(unique(rdyear))
	rdn_secondaryoccasion=as.numeric(table(rdyear))		#number of each year appearing in agegroup1994_5, etc.. (for each year 4 because May, June, July, August)
  
		# make a preliminary capture history (not collapsed)
	ch2=apply(test[,indx],c(1,2),function(t) ifelse(is.na(t),0,1))	#for columns in test with agegoup in them, replace NA with 0

		# Create effort matrix to the same structure as capture history
		# from the previous brought-in effort matrix (pop, year, month, effort), navigate for each individual where it was located for specific year_month and put that effort value
		# in the form of a new matrix with individual as rows and year_month as columns 

	occasion_names=work3$year_month
	ch_data=test

		#  generate matrices and summary variables for linking effort with the capture history matrix
	realbout <- as.character(unique(occasion_names))
	realyear <- as.numeric(substr(realbout,1,4))
	realmonth <- as.numeric(substr(realbout,6,6))
	realpop <- as.numeric(ch_data$pop)

	firstyear <- min(realyear)
	lastyear <- max(realyear)
	nbouts <- length(realyear)
	nyears <- length(unique(realyear))
	nind <- nrow(ch_data)

		# shape into matrices with same dimensions as capture history
	yearmat <- matrix(rep(realyear,times=nind),nrow=nind,ncol=nbouts,byrow=T)
	monmat <- matrix(rep(realmonth,times=nind),nrow=nind,ncol=nbouts,byrow=T)
	popmat <- matrix(rep(realpop,times=nbouts),nrow=nind,ncol=nbouts,byrow=F)

    # generate a new effort matrix
	effortmatrix <- matrix(0,nrow=nind,ncol=nbouts)   
	for(i in 1:nind){
	  for(j in 1:nbouts){
		ndx <- which((effort$pop==popmat[i,j])&(effort$year==yearmat[i,j])&(effort$month==monmat[i,j]))
		effortmatrix[i,j] <- ifelse(length(ndx)>0, effort$effort[ndx],NA)
	  }
	}
	effortmat<-effortmatrix
		
	  ######## IMPORTANT ########
		# If NA in effort matrix(ind as rows, year_month as columns), it means there is no trapping event. 
		# For such, replace "1"s and "0"s in ch2 matrix (ind as rows, agegroupyear_month as colums) with "." (=NA) for survey occasions with no data....
	for(i in 1:nind){
		ndx=which(is.na(effortmat[i,]))   # which are NAs in the effort matrix
		ch2[i,ndx]=NA 	#change 0 to NA
	}
	  # the above code might have led to having individuals without capture (no "1"s)
	  # find and remove individuals without capture (comprised of only "." and "0")
	ch3<-ch2	
	ndx<-which(apply(ch2,1,function(t) sum(t,na.rm=TRUE))==0)
	  # if any ndx, then,
	if(length(ndx)>0) ch3 <- ch2[-ndx,]
	  # remove individuals in 'effortmat' as well
	if(length(ndx)>0) effortmat <- effortmat[-ndx,]
	effortdf=as.data.frame(apply(effortmat,c(1,2),function (t) ifelse(is.na(t), log(0.001), log(t))))
	effortcolnames=paste("eff", realyear,rep(c(1,2,3,4),times=nyears),sep="")
	names(effortdf)=effortcolnames
	  # remove individuals in 'test' as well
	test2<-test
	if(length(ndx)>0) test2 <- test[-ndx,]
	  ############################
  
		# make final capture history matrix (with dots for no data)
		# ch2 consists of ind as rows and agegroupyear_months as colums with values of 0,1,NA
		# put those values in a sequences while converting NA to "."
		# 'ch' - real capture history with 0,1, and '.' for all individuals
	ch=apply(ch3,1,function(t) paste(ifelse(is.na(t),".",t),collapse=""))               # FINAL CAPTURE HISTORY MATRIX!!

	setwd(dir)
	filename <- paste(SPECIES_CODE, "CaptureHistory.txt", sep="_")
	write.table(ch, file=filename, sep="\t")
	
	ToResultsFile(
    paste("To view the capture history, go to file location:", dir, "and \n look for file: ", filename), RESULTS_DIRECTORY, RESULTS_FILENAME)


		########## "st" - individual covariate for stage ####################
		###### st (agegroup) is either adult (=0) or juvenile (=1) #########
		# 'agegroupdf' - ind as rows, and only agegroupyear_month from test as columns  
	agegroupdf=test2[,grep("agegroup", names(test2))]		
	yearlab <- as.factor(substr(names(agegroupdf),9,12))		#take year from agegroupyear_month
	agegroupcolnames=paste("st", unique(yearlab),sep="")
	nyears=length(unique(yearlab))
	  # 'ag2' - nind (row), year(col) with 1 for Juv, 2 for Adult
		# 'ag3' - nind (row), year(col) with 1 for Juv, 0 for Adult
	ag2 <- matrix(0,nrow=nrow(test2),ncol=nyears)		#matrix with ind as rows and year as columns
	ag3 <- matrix(0,nrow=nrow(test2),ncol=nyears)
	for (i in 1:nrow(test2)) {
	  temp <-tapply(as.numeric(agegroupdf[i,]),yearlab,function(t) round(mean(t,na.rm=T),0)) # Adult=1; Juv=2
	  ag2[i,] <- ifelse(is.nan(temp),0,temp)		#for NA, convert to 0, for numbers keep them as they are
	  ag2[i,] <- ifelse(ag2[i,]==2,1,ifelse(ag2[i,]==1,2,0))   # Juv=1, Adult=2 (convert 2 to 1, convert 1 to 2, anything else to 0)
	  first <- which(ag2[i,]>0)[1]
	  if(first<nyears) ag2[i,(first+1):nyears] <- 2    # every year after first capture, it's an adult
	  if(first>1) ag2[i,1:(which(ag2[i,]>0)[1]-1)] <- 1        # every year before first capture, it's a juvenile 
	  ag3[i,] = ifelse(ag2[i,]==1,1,0)	# keep Juv=1 as 1, change Adult=2 to 0, and already 0s as 0.
	}	 
	stratadf=ag3
	colnames(stratadf)= agegroupcolnames

	  ########## "trans" - individual covariate for potential transients ###############
	  ###### potential transients #########
	  # 1) captured only once
	  # 2) adults
	  # 3) non-breeding condition: CP=0 and BP=0

	  # 'agegroupdf' - ind as rows, and only CPyear_month from test as columns 
	CP_df=test2[,grep("CP", names(test2))]		
	BP_df=test2[,grep("BP", names(test2))]  	
	yearlab <- as.factor(substr(names(CP_df),3,6))  	#take year from CPyear_month
	nyears=length(unique(yearlab))
	 
	  # select years in which CP was only 0
	CP_df2 <- matrix(0,nrow=nrow(test2),ncol=nyears)  	#matrix with ind as rows and year as columns
	BP_df2 <- matrix(0,nrow=nrow(test2),ncol=nyears)
	suppressWarnings(
		for (i in 1:nrow(test2)) {
		  for (j in 1:nyears){
			# record the largest score during that year
			# if 0, it means it was 0 the whole time during that year (all 0 or mixture of 0 and NA)
			CP_df2[i,j]<-max(CP_df[i,4*j],CP_df[i,4*j-1],CP_df[i,4*j-2],CP_df[i,4*j-3],na.rm=T) 
			BP_df2[i,j]<-max(BP_df[i,4*j],BP_df[i,4*j-1],BP_df[i,4*j-2],BP_df[i,4*j-3],na.rm=T) 
			# if all NA, max is -Inf. Replace -Inf to NA
			CP_df2[i,j]<-ifelse(is.infinite(CP_df2[i,j]),NA,CP_df2[i,j])
			BP_df2[i,j]<-ifelse(is.infinite(BP_df2[i,j]),NA,BP_df2[i,j])
		  }
		}
	)# if warnings(), this is due to all NA resulting in -Inf for their maximum
	
	  # matrix 'trans' - 1 for years in which the individual is a potential transient
	  # get year where individual is A (st=0) and CP=0 and BP=0 
	trans<-matrix(0,nrow=nrow(test2),ncol=nyears) 
	for (i in 1:nrow(test2)){
	  for (j in 1:nyears){
		trans[i,j]<-ifelse((stratadf[i,j]==0)&(CP_df2[i,j]==0)&(BP_df2[i,j]==0),1,0)
		# replace NA with 0
		trans[i,j]<-ifelse(is.na(trans[i,j]),0,trans[i,j])
	  }
	}   

	  # among individuals that trans=1, leave out the individuals that were caught more than once within the same year
	  # first create matrix which indicate whether the individual was recaptured again within the same year
	  # for the same individuals in 'test'
	  # 0 if caught more than once within the same year or when not caught at all
	  # 1 if only caught once within the same year
	recap_within_sameyear<-matrix(0,nrow=nrow(test2),ncol=nyears) 
	colnames(recap_within_sameyear)<-c(BEGINYEAR:ENDYEAR)
	years<-c(BEGINYEAR:ENDYEAR)
	indvs<-test2$band

	for (i in 1:length(indvs)){
	  for (j in 1:nyears){
		index<-which((band.data_allmon$band==indvs[i])&(band.data_allmon$captureyear==years[j]))
		recap_within_sameyear[i,j]<-ifelse(length(index)==1,1,0)
	  }
	}

	  # final 'pot_trans' 
	  # for each individual, whether it is a potential transient in a given year
	pot_trans<-matrix(NA,nrow=nrow(test2),ncol=nyears) 
	for (i in 1:nrow(pot_trans)){
	  for (j in 1:ncol(pot_trans)){
		# if trans=1 and recap_within_sameyear=1, then it is a potential transient
		pot_trans[i,j]<-trans[i,j]*recap_within_sameyear[i,j]   # potential transient=1
	  }
	}

	pot_transcolnames=paste("trans", unique(yearlab),sep="")
	colnames(pot_trans)=pot_transcolnames
	

	  ##################### CMR_Data (structure for sending along) #####################################
		
	 # only st and effort and potential trasient as temporarily varying individual covariate
	CMR_Data$MasterCapHist=data.frame(band=test2[c("band")], ch=ch, freq=test2[c("freq")], pop=test2[c("pop")], station=test2[c("station")], stratadf, effortdf, pot_trans)
	CMR_Data$MasterCapHist$ch=as.character(CMR_Data$MasterCapHist$ch)
   # change 'pop' to a factor variable to feed into MARK
	CMR_Data$MasterCapHist$pop<-as.factor(CMR_Data$MasterCapHist$pop)

	  # remove "fake bird" :)
	  # nrow(CMR_Data[which(CMR_Data$band==99999),])
	if (nrow(CMR_Data$MasterCapHist[which(CMR_Data$band==99999),])!= 0 ) CMR_Data$MasterCapHist=CMR_Data$MasterCapHist[-which(CMR_Data$band==99999),]
    
		# make fake covariate for tricking RMark
	CMR_Data$MasterCapHist$random=as.numeric(paste(seq(1,nrow(CMR_Data$MasterCapHist),1),rep(9999,times=nrow(CMR_Data$MasterCapHist)),sep=""))  
	
    # add other variables that we need to pass along
	CMR_Data$rdn_year <- rdn_year	
	CMR_Data$realyear <- realyear
	CMR_Data$realbout <- realbout
	CMR_Data$stratadf <- stratadf
  
	return(CMR_Data)
  
} # END OF FUNCTION 'FormatForCMR'


##########################################################################################
# FUNCTION 'FormatForRMark'
#
# ARGUMENTS:
#			'CMRData'     -- Results from function 'FormatForCMR': data from the MAPS database, formatted in capture history format
#			'dir'         -- Project data directory
#     'MAPSData'    -- Results from function 'ReadRawData'
#     'AddDensity'  -- Boolean flag (T or F) indicating whether to append density results. Default is FALSE.

#     NOTE: if AddDesity=TRUE, must have an object in workspace named "RMarkData", which contains the results from running "FormatForRMark" with AddDensity=FALSE 
#############################################################################################	
 
  # Generate Design data for Survival estimation - based on Robust Design model

FormatForRMark <- function(CMRData, MAPSData, dir, AddDensity, TrendModel){

    if(AddDensity&!exists("RMarkData"))  cat("WARNING: RMarkData not defined! Please run FormatForRMark with AddDensity=FALSE")

    if(!AddDensity){
		inputFile <- CMRData$MasterCapHist
		rdn_year  <- CMRData$rdn_year
		realyear  <- CMRData$realyear
		realbout  <- CMRData$realbout
		stratadf  <- CMRData$stratadf
		
		effort=MAPSData$effort
		file=inputFile
		startyear=BEGINYEAR
		lastyear=ENDYEAR

		time.interval=rep(c(0,0,0,1), times=rdn_year)[-(4*rdn_year)]  	#last 1 omitted      

		  if(!TrendModel){ 
			  maps.process=process.data(file, model="RDHuggins", time.intervals=time.interval, begin.time=startyear, groups='pop')
			  maps.ddl=make.design.data(maps.process,parameters=list(S=list(pim.type="all")))   
			
			  ########### first capture year #############
			  # identify first year of capture to model transients
				# 'age' in MARK is the year after first capture, not the real age
			  maps.ddl=add.design.data(maps.process,maps.ddl,parameter="S",type="age",bins=c(0,0.5,lastyear-startyear+1),name="first_cap" )   

				#make first_cap binary (0 or 1)
		  	maps.ddl$S$first_cap_bin=numeric(nrow(maps.ddl$S))
			  for(i in 1:nrow(maps.ddl$S)){
				  maps.ddl$S$first_cap_bin[i] = ifelse(maps.ddl$S$first_cap[i]=="[0,0.5]",1,0)
			  } 
		  }else{
			  maps.process=process.data(file, model="RDPdLHuggins", time.intervals=time.interval, begin.time=startyear, groups='pop') # groups by user-defined population
			  maps.ddl=make.design.data(maps.process,parameters=list(Phi=list(pim.type="all")))   
						
		    ########### first capture year #############
			  # identify first year of capture to model transients
				# 'age' in MARK is the year after first capture, not the real age
			  maps.ddl=add.design.data(maps.process,maps.ddl,parameter="Phi",type="age",bins=c(0,0.5,lastyear-startyear+1),name="first_cap" )   

				#make first_cap binary (0 or 1)
			  maps.ddl$Phi$first_cap_bin=numeric(nrow(maps.ddl$Phi))
			  for(i in 1:nrow(maps.ddl$Phi)){
				  maps.ddl$Phi$first_cap_bin[i] = ifelse(maps.ddl$Phi$first_cap[i]=="[0,0.5]",1,0)
			  } 
		  }

			######################ADD DUMMY VARIABLES FOR YEAR (SESSION)
			# add dummy variables for the "p" design data representing the "year" identity of each bout. 	
		newyears=unique(realyear)
		nbouts=length(realbout)
		nyears=ncol(stratadf)
		dummyname=character(nyears)
		for(y in 1:nyears){
		  dummyname[y]=paste("year",newyears[y],sep="")
		  eval(parse(text=paste("maps.ddl$p$",dummyname[y],"=ifelse(maps.ddl$p$session==newyears[y],1,0)", sep="")))
		  eval(parse(text=paste("maps.ddl$c$",dummyname[y],"=ifelse(maps.ddl$c$session==newyears[y],1,0)", sep="")))
		}

			## add another new column to the design data
			# Intercept column with 1s
		maps.ddl$p$newIntercept=rep(1,times=nrow(maps.ddl$p))
		maps.ddl$c$newIntercept=rep(1,times=nrow(maps.ddl$c))
    
		  ##### Add effort to "p" and "c" design data: alternative method for modeling EFFORT
			## This works if the assumption of no movement between locations and stations is met. 
		maps.ddl$p$effort=numeric(nrow(maps.ddl$p))
		maps.ddl$c$effort=numeric(nrow(maps.ddl$c))
		for(i in 1:nrow(maps.ddl$p)){
			index = which((effort$year==maps.ddl$p$session[i])&
							 (effort$pop==maps.ddl$p$pop[i])&((effort$month-4)==maps.ddl$p$time[i]))
			maps.ddl$p$effort[i] = ifelse(length(index>0),log(effort$effort[index]),log(0.001)) 	
		} 	
		for(i in 1:nrow(maps.ddl$c)){
			index = which((effort$year==maps.ddl$c$session[i])&
							 (effort$pop==maps.ddl$c$pop[i])&((effort$month-4)==maps.ddl$c$time[i]))
			maps.ddl$c$effort[i] = ifelse(length(index>0),log(effort$effort[index]),log(0.001))		
		} 
		
			## change the "session" variable and add a year variable to the design data for "p" and "c"
		maps.ddl$p$year=maps.ddl$p$session
		maps.ddl$c$year=maps.ddl$c$session

			# add a "first capture year" component to ddl 
		maps.ddl$GammaDoublePrime$firstYear <- as.factor(ifelse(maps.ddl$GammaDoublePrime$age==1,1,0)) 
		result.maps.ddl=list(ddl=maps.ddl, process=maps.process)
		maps.ddl=result.maps.ddl$ddl
		maps.process=result.maps.ddl$process
    
		## bundle data for return to main workspace
		RMarkData <- list()
		RMarkData$maps.ddl <- maps.ddl
		RMarkData$maps.process <- maps.process
    
	}
	
	  ######### density from MAPS ##############
    if(AddDensity){
	    
		setwd(dir)      # load up the previous RMarkData
		filename<-paste(SPECIES_CODE, "RMarkData.RData", sep="_")    
		load(filename)
    
		maps.ddl=RMarkData$maps.ddl
		maps.process=RMarkData$maps.process

		  # create 'maps_density_frame'
		  # density with both J and A
		maps_density_frame<-p.table[,c("pop","year","maps_rD","maps_prev.rD")]
		# mean(maps_density_frame$maps_rD,na.rm=T)    # should be 1
		  
		  # leave only rows with values for maps_rD - one row for each year
		maps_density_frame2<-maps_density_frame[!is.na(maps_density_frame$maps_rD),] 

		  # determine the value of MAPS density above which ceiling-type density-dependence will be used
		  # This value will be calculated as the mean of max(maps_rD) for each population
		unique.pop<-sort(unique(maps_density_frame2$pop))
		  # first, get maximum for each population across all time periods
		max.maps_rD<-data.frame(pop=unique.pop, max.maps_rD=NA)
		for (i in 1:nrow(max.maps_rD)){
		  index<-which(maps_density_frame2$pop==max.maps_rD$pop[i])
		  max.maps_rD$max.maps_rD[i]<-max(maps_density_frame2$maps_rD[index],na.rm=T)
		}

	    # get the mean of maximums of each population 
		mean_max.maps_rD<-mean(max.maps_rD$max.maps_rD)

		  # leave only rows with values for maps_rD - one row for each year
		maps_density_frame<-maps_density_frame[!is.na(maps_density_frame$maps_rD),] 

		  if(!TrendModel){
			  # add real maps density for each population and time step into maps.ddl$S
			  maps.ddl$S$maps_density=numeric(nrow(maps.ddl$S))
		
			  maps_density_frame$year <- as.character(maps_density_frame$year)    # added because factors weren't aligning.
			  maps.ddl$S$time <- as.character(maps.ddl$S$time)    # added because factors weren't aligning.
			
			  for(i in 1:nrow(maps.ddl$S)){
			    index = which((maps_density_frame$year==maps.ddl$S$time[i])&(maps_density_frame$pop==maps.ddl$S$pop[i])) 
			    maps.ddl$S$maps_density[i] = ifelse(length(index>0),maps_density_frame$maps_rD[index],NA)
			  }
		  }else{
			  # add real maps density for each population and time step into maps.ddl$S
			  maps.ddl$Phi$maps_density=numeric(nrow(maps.ddl$Phi))
		
			  maps_density_frame$year <- as.character(maps_density_frame$year)    # added because factors weren't aligning.
			  maps.ddl$Phi$time <- as.character(maps.ddl$Phi$time)    # added because factors weren't aligning.
			
			  for(i in 1:nrow(maps.ddl$Phi)){
			    index = which((maps_density_frame$year==maps.ddl$Phi$time[i])&(maps_density_frame$pop==maps.ddl$Phi$pop[i])) 
			    maps.ddl$Phi$maps_density[i] = ifelse(length(index>0),maps_density_frame$maps_rD[index],NA)
			  }
		  }

		## bundle data for return to main workspace
		RMarkData <- list()
		RMarkData$maps.ddl <- maps.ddl
		RMarkData$maps.process <- maps.process    
		RMarkData$max.rD <- mean_max.maps_rD
    
	}
	
	  #[NOTE: For later analysis, could add weather information here!!!]

	return(RMarkData)
  
} # END OF FUNCTION 'FormatForRMark'


###########################################################################
# FUNCTION 'Run.Models'
#
# RUN SEVERAL SETS OF MODELS FOR PARAMETER ESTIMATION IN MARK USING ROBUST DESIGN
#
# ARGUMENTS: 
#     'RMarkData'    --  List object with the following components:
#                         maps.ddl: "design data layer" object. Needed for running MARK from RMark (see RMark documentation for details).
#                         maps.process: "process" object (list). Needed for running MARK from RMark (see RMark documentation for details).                        
#     'initial'      --  Set initial values for apparent survival. 
#     'DensityModel' --  Boolean flag (T or F) indicating whether to run density-dependent model. Default is FALSE.
#
# NOTE: 
#      Effort is used as a covariate 2 different ways. When the model includes "effort", it is used from the design matrix and 
#  	   if model includes "eff", it refers to effort as a temporarily varying covariate which is more flexible and does not need
#		   the assumption of no movement between populations.
############################################################################

Run.Models <- function(RMarkData, initial, DensityModel, TrendModel) {

  process <- RMarkData$maps.process
  ddl <- RMarkData$maps.ddl
  
  ###################### TIME MODELS ############################
  # contains only Time related models
  # the resulting estimates of capture probabilities for juveniles/adults will later be used to calculated relative density

    ###################################################
    #    If trend is not estimated here (use BBS instead)
  
    if ((!DensityModel) & (!TrendModel)) {
    
      #################### Survival ########################
      # effect of transients included
      # trans - a temporarily varying individual covariate
      # first_cap_bin - first capture year  
    
      # Stage model - estimates apparent survival for juveniles/adults
      S.st.plus.trans=list(formula=~st+first_cap_bin:trans)
      
      # Time model - estimates temporal variability in S for juveniles/adults
      S.st.plus.time.plus.st.time.time.plus.trans=list(formula=~st+time+st:time+first_cap_bin:trans) # with st:time
      
      #################### Capture probability ########################
      # st - individual covariate for stage (1: juvenile, 0: adult)
      # effort - total number of hours of mistnetting in a given month
      p.st.plus.effort=list(formula=~st+effort,share=TRUE)
      
      #################### Gamma parameters ########################
      # Gamma prime - the probability of being off the study area, unavailable for capture during primary trapping session (i) 
      #      given that the animal was not present on the study area during primary trapping session (i ??? 1), and survives to trapping session (i).
      # Gamma double prime - the probability of being off the study area, unavailable for capture during the primary trapping session (i) 
      #      given that the animal was present during primary trapping session (i ??? 1), and survives to trapping session (i)
      # in a 'no movement' model, ??' is fixed to 1 and ??" is fixed to 0. 
      
      GammaPrime.fixed=list(formula=~1, fixed=1)
      GammaDoublePrime.fixed=list(formula=~1, fixed=0)
  	
      cml=create.model.list("RDHuggins")        
      results=suppressMessages(mark.wrapper(cml,data=process,ddl=ddl, use.initial=TRUE,silent=TRUE)) # takes a long time to run! the values from the previous model used as initial values in the later models
  	
    }

    ################################################
    #    If trend is estimated here (using MAPS data)
    
    if ((!DensityModel) & (TrendModel)) {
      
      #################### Survival ########################
      # Modeled as constant
      # The mark-recapture dataset here only contains adults, therefore, 'st' will not be modeled as covariate
      # Baseline model - estimates apparent survival for adults 
      Phi.baseline=list(formula=~1)
      
      #################### Capture probability ########################
      # The mark-recapture dataset here only contains adults, therefore, 'st' will not be modeled as covariate
      # effort - total number of hours of mistnetting in a given month
      
      p.effort=list(formula=~effort,share=TRUE)
      #p.baseline=list(formula=~1,share=TRUE)
      
      #################### Trend / Lambda ########################  
      # Trend model - estimates regional trend, for correcting survival
      Lambda.regional.trend = list(formula=~1) 
  
      cml=create.model.list("RDPdLHuggins")       
      results=suppressMessages(mark.wrapper(cml,data=process,ddl=ddl, use.initial=TRUE,silent=TRUE)) # takes a long time to run! the values from the previous model used as initial values in the later models
    }


  ###################### DENSITY MODELS ############################
  # contains 'density' as covariate which is calculated based on capture probabilities from MODEL 1
  
    ###################################################
    #    If trend is not estimated here (use BBS instead)
  
    if ((DensityModel) & (!TrendModel)) {   
      
      ###################  Survival #############################
      # effect of transients included
      # trans - a temporarily varying individual covariate
      # first_cap_bin - first capture year
      
      # Density model - measures density-dependence in S for juveniles/adults/adult transients
      S.st.plus.maps_density.plus.trans=list(formula=~st+maps_density+first_cap_bin:trans)
      
      #################### Capture probability ########################
      # st - individual covariate for stage (1: juvenile, 0: adult)
      # effort - total number of hours of mistnetting in a given month
      p.st.plus.effort=list(formula=~st+effort,share=TRUE)
      
      #################### Gamma parameters ########################
      # Gamma prime - the probability of being off the study area, unavailable for capture during primary trapping session (i) 
      #      given that the animal was not present on the study area during primary trapping session (i ??? 1), and survives to trapping session (i).
      # Gamma double prime - the probability of being off the study area, unavailable for capture during the primary trapping session (i) 
      #      given that the animal was present during primary trapping session (i ??? 1), and survives to trapping session (i)
      # in a 'no movement' model, ??' is fixed to 1 and ??" is fixed to 0. 
      
      GammaPrime.fixed=list(formula=~1, fixed=1)
      GammaDoublePrime.fixed=list(formula=~1, fixed=0)
       
      cml=create.model.list("RDHuggins")        
      results=suppressMessages(mark.wrapper(cml,data=process,ddl=ddl, use.initial=TRUE,silent=TRUE))  #the values from the previous model used as initial values in the later models
    }
  

    ################################################
    #    If trend is estimated here (using MAPS data)
  
    if ((DensityModel) & (TrendModel)) {   
      
      ###################  Survival #############################
      # the mark-recapture dataset here only contains adults, therefore, 'st' will not be modeled as covariate
      # Density model - measures density-dependence in Phi
      Phi.maps_density=list(formula=~maps_density)
            
      #################### Capture probability ########################
      # The mark-recapture dataset here only contains adults, therefore, 'st' will not be modeled as covariate
      # effort - total number of hours of mistnetting in a given month
      
      p.effort=list(formula=~effort,share=TRUE)
      #p.baseline=list(formula=~1,share=TRUE)
      
      #################### Trend / Lambda ########################  
      # Trend model - estimates regional trend, for correcting survival
      Lambda.regional.trend = list(formula=~1) 
      
      cml=create.model.list("RDPdLHuggins")        
      results=suppressMessages(mark.wrapper(cml,data=process,ddl=ddl, use.initial=TRUE,silent=TRUE))  #the values from the previous model used as initial values in the later models
    }
  
  return(results)
  
} # END OF FUNCTION 'Run.Models'


###################################################################################
# FUNTION 'PCapResults'
#
# ESTIMATE CAPTURE PROBABILITIES FROM MARK MODEL RESULTS 
# AND CALCULATE ABUNDANCES AND RELATIVE DENSITY BASED ON THE CAPTURE PROBABILITIES
# 
# ARGUMENTS: 
#     'RMarkResults' --   RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object).
#     'maps.ddl'     --   "design data layer" object. Needed for running MARK from RMark (see RMark documentation for details).   
#     'band.data'    --   Banding and capture data from MAPS 
#     'model.no'     --   Number of the time-dependent model from previous MARK result which is used to calculate capture probabilities
###################################################################################

PCapResults <- function (RMarkResults, maps.ddl, band.data, model.no){
  
  # Extract population, year, month, log(effort) information from maps.ddl
  p.table <- maps.ddl$p[,c("pop","session","time","effort")]     
  colnames(p.table) <- c("pop","year","month","log_effort")
  
  # Calculate capture probability for each month/year at each population 
  # the effort values in maps.ddl$p are already log-transformed and the models were fitted to those 
  # use model S (~st + time + st:time + first_cap_bin:trans), p(~st + effort) 
  model <- RMarkResults[[model.no]]
  betas <- RMarkResults[[model.no]]$result$beta
  
  # beta for intercept
  b.itcp <- betas$estimate[grep("p:st", rownames(betas))-1]
  # beta for stage
  b.st <- betas$estimate[grep("p:st", rownames(betas))]
  # beta for effort
  b.effort <- betas$estimate[grep("p:effort", rownames(betas))]

  # beta ucl for intercept
  b.itcp.ucl <- betas$ucl[grep("p:st", rownames(betas))-1] 
  # beta lcl for intercept
  b.itcp.lcl <- betas$lcl[grep("p:st", rownames(betas))-1]   
  # beta ucl for stage
  b.st.ucl <- betas$ucl[grep("p:st", rownames(betas))]
  # beta lcl for stage
  b.st.lcl <- betas$lcl[grep("p:st", rownames(betas))]
  # beta ucl for effort
  b.effort.ucl <- betas$ucl[grep("p:effort", rownames(betas))]
  # beta lcl for effort
  b.effort.lcl <- betas$lcl[grep("p:effort", rownames(betas))]
  
  # Calculate capture probability
  # p.j: capture probability of juvenile at a given time and population
  # p.a: capture probability of adult at a given time and population
  for (i in 1:nrow(p.table)){
    # Juv: beta(intercept)+beta(st)+beta(effort)*log(effort)
    p.table$p.j[i]<-inv.logit(b.itcp + b.st + b.effort* p.table$log_effort[i])
	  p.table$p.j.lcl[i]<-min( inv.logit(b.itcp.lcl + b.st.lcl + b.effort.lcl* p.table$log_effort[i]), 
                             inv.logit(b.itcp.lcl + b.st.lcl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.lcl + b.st.ucl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.lcl + b.effort.lcl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.lcl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.ucl + b.effort.ucl* p.table$log_effort[i])                 
                             ) # all combinations of lcl/ucl required to calculate the min
	  p.table$p.j.ucl[i]<-max( inv.logit(b.itcp.lcl + b.st.lcl + b.effort.lcl* p.table$log_effort[i]), 
	                           inv.logit(b.itcp.lcl + b.st.lcl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.lcl + b.st.ucl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.lcl + b.effort.lcl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.lcl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.st.ucl + b.effort.ucl* p.table$log_effort[i])                 
	                           ) # all combinations of lcl/ucl required to calculate the max
    # Adults: beta(intercept)+beta(effort)*log(effort)
    p.table$p.a[i]<-inv.logit(b.itcp + b.effort* p.table$log_effort[i])
    p.table$p.a.lcl[i]<-min( inv.logit(b.itcp.lcl + b.effort.lcl* p.table$log_effort[i]), 
                             inv.logit(b.itcp.lcl + b.effort.ucl* p.table$log_effort[i]),
                             inv.logit(b.itcp.ucl + b.effort.lcl* p.table$log_effort[i]),
                             inv.logit(b.itcp.ucl + b.effort.ucl* p.table$log_effort[i])                   
                             ) # all combinations of lcl/ucl required to calculate the min
	  p.table$p.a.ucl[i]<-max( inv.logit(b.itcp.lcl + b.effort.lcl* p.table$log_effort[i]), 
	                           inv.logit(b.itcp.lcl + b.effort.ucl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.effort.lcl* p.table$log_effort[i]),
	                           inv.logit(b.itcp.ucl + b.effort.ucl* p.table$log_effort[i])                   
	                           ) # all combinations of lcl/ucl required to calculate the max
  }
  
  # Calculate capture probability for a given year
  # p.j_year: capture probability of juvenile at a given year and population  
  # p.a_year: capture probability of adult at a given year and population
  for (i in 1:(nrow(p.table)/4)){
    p.table$p.j_year[4*i]<-(1-(1-p.table$p.j[4*i-3])*(1-p.table$p.j[4*i-2])*(1-p.table$p.j[4*i-1])*(1-p.table$p.j[4*i]))
  }
  for (i in 1:(nrow(p.table)/4)){
    p.table$p.a_year[4*i]<-(1-(1-p.table$p.a[4*i-3])*(1-p.table$p.a[4*i-2])*(1-p.table$p.a[4*i-1])*(1-p.table$p.a[4*i]))
  }
  
  
  ########### Abundance (Njuv, Nad, Nad_breed) ######################
  # Njuv: total number of juveniles caught in a given year
  # Nad: total number of all adults (non-breeding and breeding) caught in a given year
  # Nad_breed: total number of breeding adults caught in a given year
  
  # first, create a set of band IDs ('band.data') for only breeding adults
  # Adults that are in non-breeding condition (BP=0, CP=0)
  non_breed_A<-which((band.data$agegroup=="A")&(band.data$CP==0)&(band.data$BP==0)) 
  # exclude the non-breeding adults from 'band.data'
  band.data_A_breed<-band.data[-non_breed_A,]
  
  ###### IMPORTANT! #########
  # processing 'band.data' to avoid multiple counting of individuals that were captured multiple times within a year
  # If an individual is caught multiple times, only 1 capture left for each year 
  
  # For juveniles and all adults 
  # band.data2 - each individual with only one capture for each year
  band.data2<-band.data
  indivs <- unique(band.data2$band)   # unique bands
  tps <- as.character(unique(band.data2$captureyear))  # unique year_month
  flag <- array(0,dim=c(length(indivs),length(tps)))   # array of capture frequencies (0 or 1) with indivs as rows and year_month as columns
  for(i in 1:length(indivs)){
    for(t in 1:length(tps)){
      ndx <- which((band.data2$captureyear==tps[t])&(band.data2$band==indivs[i]))
      flag[i,t] <- length(ndx) 	# the total number captured for indiv[i] in tps[t](for example, the number of band 296 captured in 1994)
      if(flag[i,t]>1){			# if captured more than once in year, remove
        newndx <- ndx[-1]   # rows to remove
        band.data2 <- band.data2[-newndx,]
      }	
    }
  }
  
  # For juveniles and only breeding adults 
  # band.data_A_breed2 - each individual with only one capture for each year
  band.data_A_breed2<-band.data_A_breed
  
  indivs_breed <- unique(band.data_A_breed2$band)   # unique bands
  tps_breed <- as.character(unique(band.data_A_breed2$captureyear))  # unique year_month
  flag_breed <- array(0,dim=c(length(indivs_breed),length(tps_breed)))   # array of capture frequencies (0 or 1) with indivs as rows and year_month as columns
  for(i in 1:length(indivs_breed)){
    for(t in 1:length(tps_breed)){
      ndx <- which((band.data_A_breed2$captureyear==tps_breed[t])&(band.data_A_breed2$band==indivs_breed[i]))
      flag_breed[i,t] <- length(ndx) 	# the total number captured for indiv[i] in tps[t](for example, the number of band 296 captured in 1994)
      if(flag_breed[i,t]>1){			# if captured more than once in year, remove
        newndx <- ndx[-1]   # rows to remove
        band.data_A_breed2 <- band.data_A_breed2[-newndx,]
      }	
    }
  }  
  
  ########### calculate Njuv, Nad, Nad_breed #############
  for (i in 1:(nrow(p.table)/4)){
    index = which((band.data2$captureyear==p.table$year[4*i])&(band.data2$pop==p.table$pop[4*i])
                  &(band.data2$agegroup=="J"))
    p.table$Njuv[4*i]<-ifelse(length(index>0),length(index),0) 
  }
  for (i in 1:(nrow(p.table)/4)){
    index = which((band.data2$captureyear==p.table$year[4*i])&(band.data2$pop==p.table$pop[4*i])
                  &(band.data2$agegroup=="A"))
    p.table$Nad[4*i]<-ifelse(length(index>0),length(index),0) 
  }
  for (i in 1:(nrow(p.table)/4)){
    index = which((band.data_A_breed2$captureyear==p.table$year[4*i])&(band.data_A_breed2$pop==p.table$pop[4*i])
                  &(band.data_A_breed2$agegroup=="A"))
    p.table$Nad_breed[4*i]<-ifelse(length(index>0),length(index),0) 
  }
  
  ########### correct for capture probability ###########
  # correct Njuv, Nad, Nad_breed for p.j_year, p.a_year 
  # add 'corr.Njuv', 'corr.Nad', 'corr.Nad_breed'
  for (i in 1:(nrow(p.table)/4)){
    p.table$corr.Njuv[4*i]<-p.table$Njuv[4*i]/p.table$p.j_year[4*i]
  }
  for (i in 1:(nrow(p.table)/4)){
    p.table$corr.Nad[4*i]<-p.table$Nad[4*i]/p.table$p.a_year[4*i]
  }
  for (i in 1:(nrow(p.table)/4)){
    p.table$corr.Nad_breed[4*i]<-p.table$Nad_breed[4*i]/p.table$p.a_year[4*i]
  }
  
  
  ########### calculate N ###########
  # N = Njuv + Nad
  for (i in 1:(nrow(p.table)/4)){
    p.table$N[4*i]<-p.table$Njuv[4*i]+p.table$Nad[4*i]
  }
  
  # N_breed = Njuv + Nad_breed
  for (i in 1:(nrow(p.table)/4)){
    p.table$N_breed[4*i]<-p.table$Njuv[4*i]+p.table$Nad_breed[4*i]
  }
  
  # corr.N = corr.Njuv + corr.Nad
  for (i in 1:(nrow(p.table)/4)){
    p.table$corr.N[4*i]<-p.table$corr.Njuv[4*i]+p.table$corr.Nad[4*i]
  }
  
  # corr.N_breed = corr.Njuv + corr.Nad_breed
  for (i in 1:(nrow(p.table)/4)){
    p.table$corr.N_breed[4*i]<-p.table$corr.Njuv[4*i]+p.table$corr.Nad_breed[4*i]
  }
  
  ###### MAPS density #########
  # maps_rD = corr.N / mean(corr.N) over time for each population
  
  # first calculate carrying capacity (K)
  # K: mean abundance over time for each population
  unique.pop<-sort(unique(p.table$pop)) # unique populations in data
  # K for each population
  mean.abund<-data.frame(unique.pop,K=0)
  for (i in 1:nrow(mean.abund)){
    index<-which(p.table$pop==mean.abund$unique.pop[i])
    mean.abund$K[i]<-mean(p.table$corr.N[index],na.rm=T) # here, K includes the non-breeding adults
  }
  
  # calculate relative density defined as 'maps_rD' = corr.N / K 
  # put the relative densities into corresponding year and location
  for (i in 1:(nrow(p.table)/4)){
    index<-which(mean.abund$unique.pop==p.table$pop[4*i])
    p.table$maps_rD[4*i]<-p.table$corr.N[4*i]/mean.abund$K[index]
  }
  
  # Use the following code, if previous year's density is used to model effect of density on the vital rates
  # put maps_prev.rD into corresponding year and population
  suppressWarnings(
    for (i in 1:(nrow(p.table)/4)){
      index = which((p.table$year==as.numeric(as.character(p.table$year[4*i]))-1)&(p.table$pop==p.table$pop[4*i]))
      index = max(index)
      p.table$maps_prev.rD[4*i] = ifelse(length(index>0),p.table$maps_rD[index],NA)   
    } # warnings() are due to the first year where there is no maps_prev.rD available
  )
  
  ###### MAPS density based on only adult abundance #########
  # juveniles might not have a large effect on the density-dependence relationship
  # maps_rD_ad = corr.Nad/ mean(corr.Nad) for that population
  
  # first calculate carrying capacity (K_ad)
  # K: mean abundance of adults over time for each population
  mean.abund_ad<-data.frame(unique.pop,K_ad=0)
  for (i in 1:nrow(mean.abund_ad)){
    index<-which(p.table$pop==mean.abund_ad$unique.pop[i])
    mean.abund_ad$K_ad[i]<-mean(p.table$corr.Nad[index],na.rm=T) # here, K includes all adults (both breeding and non-breeding adults)
  }
  
  # calculate relative density defined as 'maps_rD_ad' = corr.Nad / K_ad 
  # put the maps_rD_ad = corr.Na by K_ad into corresponding year and location
  for (i in 1:(nrow(p.table)/4)){
    index<-which(mean.abund_ad$unique.pop==p.table$pop[4*i])
    p.table$maps_rD_ad[4*i]<-p.table$corr.Nad[4*i]/mean.abund_ad$K_ad[index]
  }
  
  # Use the following code, if previous year's density is used to model effect of density on the vital rates
  # put maps_prev.rD_ad into corresponding year and location
  suppressWarnings(
    for (i in 1:(nrow(p.table)/4)){
      index = which((p.table$year==as.numeric(as.character(p.table$year[4*i]))-1)&(p.table$pop==p.table$pop[4*i]))
      index = max(index)
      p.table$maps_prev.rD_ad[4*i] = ifelse(length(index>0),p.table$maps_rD_ad[index],NA)   
    } # warnings() are due to the first year (no prev.rD for the beginyear)
  )
  
  return (p.table)
  
} # END OF FUNTION 'PCapResults'


######################################################################################
# FUNCTION 'ApparentS'
#
# ESTIMATES APPARENT SURVIVAL RATES FOR JUVENILES/ADULTS/ADULT TRANSIENTS FROM MARK TIME-CONSTANT MODEL
#
# ARGUMENTS:   
#     'RMarkResults' --  RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object).
#     'model.no.'    --  Number of the time-constant model from previous MARK result which is used to calculate apparent survival rates           								
######################################################################################

ApparentS <- function(RMarkResults, model.no){   
  
    ########## Modify covariate matrix for juveniles/adults/adult transients #########
    # find and extract cells in MARK design matrix containing covariates
    # use S(~st+first_cap_bin:trans)p(~st+effort)
  fc=find.covariates(RMarkResults[[model.no]])  # change number accordingly
  
    # By default, the mean values are put for the covariates
    # In the model, stage is 1 for juveniles, and 0 for adults 
  
    # For juveniles, values for stage=1, and for trans=0
    # For rows that contain "S" in rnames and "st" in var, change value to 1
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-1
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-0
    # Fill design matrix with values for juveniles
  design.j=fill.covariates(RMarkResults[[model.no]],fc)
  
    # For adult residents, values for stage=0, and for trans=0
    # For rows that contain "S" in rnames and "st" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-0
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-0
    # Fill design matrix with values for adult residents
  design.a=fill.covariates(RMarkResults[[model.no]],fc)
  
    # For adult transients, values for stage=0, and for trans=1
    # For rows that contain "S" in rnames and "st" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-0
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-1
    # Fill design matrix with values for adult transients
  design.a_trans=fill.covariates(RMarkResults[[model.no]],fc)

  
    ########### Compute and output real S and p values ################
    # For each row in design matrix, compute real value of S and p
    # For juveniles
  s.p.j=compute.real(RMarkResults[[model.no]],design=design.j, vcv=T) 
    # For adult residents
  s.p.a=compute.real(RMarkResults[[model.no]],design=design.a, vcv=T) 
    # For adult transients
  s.p.a_trans=compute.real(RMarkResults[[model.no]],design=design.a_trans, vcv=T)
  
    # Select apparent S 
    # S for juveniles
  Sjuv<-list()
  Sjuv$estimate=s.p.j$real[1] #this accounts for st:time
  Sjuv$lcl=s.p.j$lcl[1]
  Sjuv$ucl=s.p.j$ucl[1]
    # S for adult residents
  Sad<-list()
  Sad$estimate=s.p.a$real[1]
  Sad$lcl=s.p.a$lcl[1]
  Sad$ucl=s.p.a$ucl[1]
    # S for adult transients
  Sad_trans<-list()
  Sad_trans$estimate=s.p.a_trans$real[1]
  Sad_trans$lcl=s.p.a_trans$lcl[1]
  Sad_trans$ucl=s.p.a_trans$ucl[1]
    
  result<-list(Sjuv=Sjuv, Sad=Sad, Sad_trans=Sad_trans)
  return(result)
  
} # END OF FUNCTION 'ApparentS'


##################################################################################
# FUNCTION 'VarianceComponent'
#
# ESTIMATE TEMPORAL VARIANCE IN APPARENT SURVIVAL USING VARIANCE COMPONENT ANALYSIS
#
# ARGUMENTS:
#     'RMarkResults' -- RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object).
#     'model.no'     -- Number of the time-dependent model in previous MARK results which is used to calculated temporal variance.
#
# OUTPUTS:
#     lines in 'Debug' text file in DATA_DIRECTORY
#     lines 'Intermediate Results' text file in RESULTS_DIRECTORY
##################################################################################

VarianceComponent <- function (RMarkResults, model.no) {
  
    ########## Modify covariate matrix for juveniles/adults/adult transients #########
    # find and extract cells in MARK design matrix containing covariates
    # first, use S(~st+time+st:time+first_cap_bin:trans) 
  fc=find.covariates(RMarkResults[[model.no]])  # change number accordingly
  
    # By default, the mean values are put for the covariates
    # In the model, stage is 1 for juveniles, and 0 for adults 
  
    # For juveniles, values for stage=1, and for trans=0
    # For rows that contain "S" in rnames and "st" in var, change value to 1
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-1
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-0
    # Fill design matrix with values for juveniles
  design.j=fill.covariates(RMarkResults[[model.no]],fc)
  
    # For adult residents, values for stage=0, and for trans=0
    # For rows that contain "S" in rnames and "st" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-0
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-0
    # Fill design matrix with values for adult residents
  design.a=fill.covariates(RMarkResults[[model.no]],fc)
  
    # For adult transients, values for stage=0, and for trans=1
    # For rows that contain "S" in rnames and "st" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("st",fc$var)), c("value")] <-0
    # For rows that contain "S" in rnames and "trans" in var, change value to 0
  fc[(c(1:nrow(fc)) %in% grep("S g",fc$rnames)) & (c(1:nrow(fc)) %in% grep("trans",fc$var)), c("value")] <-1
    # Fill design matrix with values for adult transients
  design.a_trans=fill.covariates(RMarkResults[[model.no]],fc)
  
  
    ########### Compute and output real S and p values ################
    # For each row in design matrix, compute real value of S and p
    # For juveniles
  s.p.j=compute.real(RMarkResults[[model.no]],design=design.j, vcv=T) 
    # For adult residents
  s.p.a=compute.real(RMarkResults[[model.no]],design=design.a, vcv=T) 
    # For adult transients
  s.p.a_trans=compute.real(RMarkResults[[model.no]],design=design.a_trans, vcv=T)
  
    # Select real S for the corresponding time covariate
  time.sel<-c(1:(ENDYEAR-BEGINYEAR))  
  time.sel_trans<-c(1,(ENDYEAR-BEGINYEAR+1):((ENDYEAR-BEGINYEAR+1)+(ENDYEAR-BEGINYEAR-1)-1))
  
    # S for juveniles
  s.j=s.p.j$real[time.sel,] #this accounts for st:time
    # S for adult residents
  s.a=s.p.a$real[time.sel,]
    # S for adult transients
  s.a_trans=s.p.a_trans$real[time.sel_trans,]
  
    # Select the subset in the VCV matrix for the corresponding time covariate
    # VCV matrix for juveniles
  vcv.j=s.p.j$vcv.real[time.sel,time.sel]
    # VCV matrix for adult residents
  vcv.a=s.p.a$vcv.real[time.sel,time.sel]
    # VCV matrix for adult transients
  vcv.a_trans=s.p.a_trans$vcv.real[time.sel_trans,time.sel_trans]
  
  
    ############ Compute process variance ##########
  
    # 1) Compute process variance for juveniles 
    # if error occurs, use CV method to estimate temporal varability in juveniles based on temporal variability in adults
  varc.j.W.E<-tryCatch.W.E(
  {varc.j=var.components(s.j, design=matrix(rep(1,length(s.j)), ncol=1), vcv.j, upper=1500*max(vcv.j))}
    )

    # produce warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.j.W.E$value), collapse=" ")))) >0 ) {
    varc.j.W.E$warning <-"Computing process variance for juveniles with all years failed!"
  } else {varc.j.W.E$warning<-NULL}

    # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.j.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH ALL YEARS
    
    Computing process variance for juveniles with all years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH ALL YEARS
    
    Computing process variance for juveniles with all years failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.j.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.j.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)

  } else {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH ALL YEARS
                  
    Computing process variance for juveniles with all years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
  
    # 2) Compute process variance for adult residents
    # if error occurs, use CV method to estimate temporal varability in juveniles based on temporal variability in adults
  varc.a.W.E<-tryCatch.W.E(
  {varc.a=var.components(s.a, design=matrix(rep(1,length(s.a)), ncol=1), vcv.a, upper=1500*max(vcv.a))} # the upper value is set to 15 * max(vcv.a) to give large bounds for uniroot function
  )

    # produce warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.a.W.E$value), collapse=" ")))) >0 ) {
    varc.a.W.E$warning <-"Computing process variance for adult residents with all years failed!"
  } else {varc.a.W.E$warning<-NULL}

    # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.a.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH ALL YEARS
    
    Computing process variance for adult residents with all years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
    
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH ALL YEARS
    
    Computing process variance for adult residents with all years failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.a.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.a.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
  } else {
    ToResultsFile("
    --------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH ALL YEARS
                  
    Computing process variance for adult residents with all years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
  

    # 3) Compute process variance for juveniles 
    # if error occurs, use CV method to estimate temporal varability in juveniles based on temporal variability in adults
  varc.a_trans.W.E<-tryCatch.W.E(
  {varc.a_trans=var.components(s.a_trans, design=matrix(rep(1,length(s.a_trans)), ncol=1), vcv.a_trans, upper=1500*max(vcv.a_trans))}
  )

    # produce warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.a_trans.W.E$value), collapse=" ")))) >0 ) {
    varc.a_trans.W.E$warning <-"Computing process variance for adult transients with all years failed!"
  } else {varc.a_trans.W.E$warning<-NULL}

  # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.a_trans.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH ALL YEARS
                
    Computing process variance for adult transients with all years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH ALL YEARS
    
    Computing process variance for adult transients with all years failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.a_trans.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.a_trans.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
  } else {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH ALL YEARS
                  
    Computing process variance for adult transients with all years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }

    # time in years
  x_axis=c(BEGINYEAR:(ENDYEAR-1))
    # Table with Year, shrunken S, S, SE, lower limit, upper limit, VCV
    # For juveniles
  if ( "varc.j" %in% ls()){
    S.v.j.table<-data.frame()
    S.v.j.table<-cbind(x_axis, varc.j$betarand$estimate, s.j, s.p.j$se.real[time.sel], 
                      s.p.j$lcl[time.sel], s.p.j$ucl[time.sel], vcv.j)
    colnames(S.v.j.table)<-c("Year","shrunken_Sjuv","Sjuv","SE","lcl","ucl",rep("vcv",(ENDYEAR-BEGINYEAR)))
  } 

    # For adult residents
  if ( "varc.a" %in% ls()){
    S.v.a.table<-data.frame()
    S.v.a.table<-cbind(x_axis, varc.a$betarand$estimate, s.a, s.p.a$se.real[time.sel], 
                      s.p.a$lcl[time.sel], s.p.a$ucl[time.sel], vcv.a)
    colnames(S.v.a.table)<-c("Year","shrunken_Sad","Sad","SE","lcl","ucl",rep("vcv",(ENDYEAR-BEGINYEAR)))
  } 

    # For adult transients
  if ( "varc.a_trans" %in% ls()){
    S.v.a_trans.table<-data.frame()
    S.v.a_trans.table<-cbind(x_axis, varc.a_trans$betarand$estimate, 
                            s.a_trans, s.p.a_trans$se.real[time.sel_trans], 
                            s.p.a_trans$lcl[time.sel_trans], s.p.a_trans$ucl[time.sel_trans], vcv.a_trans)
    colnames(S.v.a_trans.table)<-c("Year","shrunken_Sad_trans","Sad_trans","SE","lcl","ucl",rep("vcv",(ENDYEAR-BEGINYEAR)))
  } 

    # save tables as .csv files
  if ( "S.v.j.table" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.j<-paste(SPECIES_CODE, "Sjuv_VarComp_allyrs", "csv", sep='.')
    write.csv(S.v.j.table, filename.j)
  }

  if ( "S.v.a.table" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.a<-paste(SPECIES_CODE, "Sad_VarComp_allyrs", "csv", sep='.')
    write.csv(S.v.a.table, filename.a)
  }

  if ( "S.v.a_trans.table" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.a_trans<-paste(SPECIES_CODE, "Sad_trans_VarComp_allyrs", "csv", sep='.')
    write.csv(S.v.a_trans.table, filename.a_trans)
  }

    ######## Check for non-convergence ##########
    # It is an indication of non-convergence if the upper, lower limits of real values are in a unreasonable range.
    # if lcl<=0, ucl>=1, remove those years and rerun the variance component analysis

  idx<-which((s.p.j$lcl[time.sel]<=0.000001) | (s.p.j$ucl[time.sel]>=0.99999))
  ifelse( length(idx)>0, time.sel.j.rm<-time.sel[-idx], time.sel.j.rm<-time.sel)
    # output message about the converging years
  if (length(time.sel.j.rm)==length(time.sel)){
          ToResultsFile(paste("
    All years between ", BEGINYEAR, " and ", ENDYEAR, " converged for juveniles! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
          } else { ToResultsFile(paste("
    A total of ",length(idx)," years did not converge between ", BEGINYEAR, " and ", ENDYEAR, " for juveniles! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
          
  idx2<-which((s.p.a$lcl[time.sel]<=0.000001) | (s.p.a$ucl[time.sel]>=0.99999))
  ifelse( length(idx2)>0, time.sel.a.rm<-time.sel[-idx2], time.sel.a.rm<-time.sel)
    # output message about the converging years
  if (length(time.sel.a.rm)==length(time.sel)){
    ToResultsFile(paste("
    All years between ", BEGINYEAR, " and ", ENDYEAR, " converged for adult residents! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
  } else { 
    ToResultsFile(paste("
    A total of ",length(idx2)," years did not converge between ", BEGINYEAR, " and ", ENDYEAR, " for adult residents! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
          
  idx3<-which((s.p.a_trans$lcl[time.sel_trans]<=0.000001) | (s.p.a_trans$ucl[time.sel_trans]>=0.99999))
  ifelse( length(idx3)>0, time.sel.a_trans.rm<-time.sel_trans[-idx3], time.sel.a_trans.rm<-time.sel_trans)
    # output message about the converging years
  if (length(time.sel.a_trans.rm)==length(time.sel_trans)){
    ToResultsFile(paste("
    All years between ", BEGINYEAR, " and ", ENDYEAR, " converged for adult transients! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
  } else { ToResultsFile(paste("
    A total of ",length(idx3)," years did not converge between ", BEGINYEAR, " and ", ENDYEAR, " for adult transients! \n"), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
          
    # S of juveniles for converging years
  s.j.rm=s.p.j$real[time.sel.j.rm,]
    # S of adults residents for converging years
  s.a.rm=s.p.a$real[time.sel.a.rm,]
    # S of adult transients for converging years
  s.a_trans.rm=s.p.a_trans$real[time.sel.a_trans.rm,]

    # Select the subset in the VCV matrix for the converging years
    # VCV matrix for juveniles
  vcv.j.rm=s.p.j$vcv.real[time.sel.j.rm, time.sel.j.rm]
    # VCV matrix for adult residents
  vcv.a.rm=s.p.a$vcv.real[time.sel.a.rm, time.sel.a.rm]
    # VCV matrix for adult transients
  vcv.a_trans.rm=s.p.a_trans$vcv.real[time.sel.a_trans.rm, time.sel.a_trans.rm]


    ############ Compute process variance ##########
    # 1) Compute process variance for juveniles 
    # if error occurs, and no varc.j.rm is obtained, use CV method to estimate temporal varability in juveniles based on temporal variability in adults

  varc.j.rm.W.E<-tryCatch.W.E(
  {varc.j.rm=var.components(s.j.rm, design=matrix(rep(1,length(s.j.rm)),ncol=1), vcv.j.rm, upper=15*max(vcv.j.rm))}
  )

    # output warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.j.rm.W.E$value), collapse=" ")))) >0 ) {
    varc.j.rm.W.E$warning <-"Computing process variance for juveniles with only converging years failed!"
  } else {varc.j.rm.W.E$warning<-NULL}

    # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.j.rm.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH CONVERGING YEARS ONLY
                
    Computing process variance for juveniles with only converging years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH CONVERGING YEARS ONLY
    
    Computing process variance for juveniles with only converging years failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.j.rm.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.j.rm.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
  } else {
    ToResultsFile("
    -------------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - JUVENILES WITH CONVERGING YEARS ONLY
                  
    Computing process variance for juveniles with only converging years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
  
    # 2) Compute process variance for adult residents

  varc.a.rm.W.E<-tryCatch.W.E(
  {varc.a.rm=var.components(s.a.rm, design=matrix(rep(1,length(s.a.rm)),ncol=1), vcv.a.rm, upper=15*max(vcv.a.rm))}  # the upper value is set to 15 * max(vcv.a.rm) to give large bounds for uniroot function
  )

    # output warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.a.rm.W.E$value), collapse=" ")))) >0 ) {
    varc.a.rm.W.E$warning <-"Computing process variance for adult residents with only converging years failed!"
  } else {varc.a.rm.W.E$warning<-NULL}

    # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.a.rm.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH CONVERGING YEARS ONLY
       
    Computing process variance for adult residents with only converging years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
    
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH CONVERGING YEARS ONLY
    
    Computing process variance for adult residents with only converging years failed
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.a.rm.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.a.rm.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
  } else {
    ToResultsFile("
    -------------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT RESIDENTS WITH CONVERGING YEARS ONLY
           
    Computing process variance for adult residents with only converging years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }

    # 3) Compute process variance for adult transients

  varc.a_trans.rm.W.E<-tryCatch.W.E(
  {varc.a_trans.rm=var.components(s.a_trans.rm, design=matrix(rep(1,length(s.a_trans.rm)),ncol=1), vcv.a_trans.rm, upper=15*max(vcv.a_trans.rm))}  # the upper value is set to 15 * max(vcv.a.rm) to give large bounds for uniroot function
  )

    # output warning message if the convergence did not work
  if ( length( grep("Error", (paste(class(varc.a_trans.rm.W.E$value), collapse=" ")))) >0 ) {
    varc.a_trans.rm.W.E$warning <-"Computing process variance for adult transients with only converging years failed!"
  } else {varc.a_trans.rm.W.E$warning<-NULL}

    # record both error and warning messages onto RESULT.txt
  if ( length( grep("Error", (paste(class(varc.a_trans.rm.W.E$value), collapse=" ")))) >0 ) {
    ToResultsFile("
    -------------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH CONVERGING YEARS ONLY

    Computing process variance for adult transients with only converging years failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
    
    ToDebugFile("
    -------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH CONVERGING YEARS ONLY
    
    Computing process variance for adult transients with only converging years failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Error",varc.a_trans.rm.W.E$value$message, sep=": "),"\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    ToDebugFile(paste(paste("Warning",varc.a_trans.rm.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
  } else {
    ToResultsFile("
    -------------------------------------------------------------------------------------
    RESULTS FOR VARIANCE COMPONENT ANALYSIS - ADULT TRANSIENTS WITH CONVERGING YEARS ONLY
                  
    Computing process variance for adult transients with only converging years worked!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
  }

    # Time with only converging years
    # For juveniles
  x_axis.j.rm<-x_axis[time.sel.j.rm]
    # For adult residents
  x_axis.a.rm<-x_axis[time.sel.a.rm]
    # For adult transients
  time.sel.a_trans.rm2<-time.sel.a_trans.rm-(ENDYEAR-BEGINYEAR-1)
  time.sel.a_trans.rm2[1]<-time.sel.a_trans.rm2[1]+(ENDYEAR-BEGINYEAR-1)
  x_axis.a_trans.rm<-x_axis[time.sel.a_trans.rm2]


    # Table with only converging years, shrunken S, S, SE, lower limit, upper limit, VCV
    # For juveniles
  if ("varc.j.rm" %in% ls()){
    S.v.j.table.rm<-data.frame()
    S.v.j.table.rm<-cbind(x_axis.j.rm, varc.j.rm$betarand$estimate , s.j.rm , s.p.j$se.real[time.sel.j.rm], s.p.j$lcl[time.sel.j.rm], s.p.j$ucl[time.sel.j.rm], vcv.j.rm)
    colnames(S.v.j.table.rm)<-c("Year","shrunken_Sjuv","Sjuv","SE","lcl","ucl",rep("vcv",length(time.sel.j.rm)))
  } 

    # For adult residents
  if ("varc.a.rm" %in% ls()){
    S.v.a.table.rm<-data.frame()
    S.v.a.table.rm<-cbind(x_axis.a.rm, varc.a.rm$betarand$estimate, s.a.rm, s.p.a$se.real[time.sel.a.rm], s.p.a$lcl[time.sel.a.rm], s.p.a$ucl[time.sel.a.rm], vcv.a.rm)
    colnames(S.v.a.table.rm)<-c("Year","shrunken_Sad","Sad","SE","lcl","ucl",rep("vcv",length(time.sel.a.rm)))
  } 

    # For adult transients
  if ("varc.a_trans.rm" %in% ls()){
    S.v.a_trans.table.rm<-data.frame()
    S.v.a_trans.table.rm<-cbind(x_axis.a_trans.rm, varc.a_trans.rm$betarand$estimate, s.a_trans.rm, s.p.a_trans$se.real[time.sel.a_trans.rm], s.p.a_trans$lcl[time.sel.a_trans.rm], s.p.a_trans$ucl[time.sel.a_trans.rm], vcv.a_trans.rm)
    colnames(S.v.a_trans.table.rm)<-c("Year","shrunken_Sad_trans","Sad_trans","SE","lcl","ucl",rep("vcv",length(time.sel.a_trans.rm)))
  } 

    # save tables as .csv files
  if ( "S.v.j.table.rm" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.j.rm<-paste(SPECIES_CODE, "Sjuv_VarComp_conv.yrs", "csv", sep='.')
    write.csv(S.v.j.table.rm, filename.j.rm)
  }

  if ( "S.v.a.table.rm" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.a.rm<-paste(SPECIES_CODE, "Sad_VarComp_conv.yrs", "csv", sep='.')
    write.csv(S.v.a.table.rm, filename.a.rm)
  }

  if ( "S.v.a_trans.table.rm" %in% ls()){
    setwd(DATA_DIRECTORY)
    filename.a_trans.rm<-paste(SPECIES_CODE, "Sad_trans_VarComp_conv.yrs", "csv", sep='.')
    write.csv(S.v.a_trans.table.rm, filename.a_trans.rm)
  }


    # Create Summary table for S
    # If variance component analysis with only converging years worked for all juveniles/adult residents/adult transients
  
  if ( ("varc.j.rm"%in% ls()) & ("varc.a.rm" %in% ls()) & ("varc.a_trans.rm" %in% ls()) ){
    # When variance component analysis with only converging years worked for all juveniles/adult residents/adult transients
  
  SSummary.rm<-data.frame()
  SSummary.rm[1,1]<-mean(s.j.rm)
  SSummary.rm[1,2]<-mean(varc.j.rm$betaran$estimate)
  SSummary.rm[1,3]<-sd(s.j.rm)
  SSummary.rm[1,4]<-var(s.j.rm)
  SSummary.rm[1,5]<-as.numeric(varc.j.rm$sigma)
  SSummary.rm[1,6]<-as.numeric(varc.j.rm$sigmasq[1])
  SSummary.rm[1,7]<-as.numeric(varc.j.rm$sigmasq[2])
  SSummary.rm[1,8]<-as.numeric(varc.j.rm$sigmasq[3])
  SSummary.rm[2,1]<-mean(s.a.rm)
  SSummary.rm[2,2]<-mean(varc.a.rm$betaran$estimate)
  SSummary.rm[2,3]<-sd(s.a.rm)
  SSummary.rm[2,4]<-var(s.a.rm)
  SSummary.rm[2,5]<-as.numeric(varc.a.rm$sigma)
  SSummary.rm[2,6]<-as.numeric(varc.a.rm$sigmasq[1])
  SSummary.rm[2,7]<-as.numeric(varc.a.rm$sigmasq[2])
  SSummary.rm[2,8]<-as.numeric(varc.a.rm$sigmasq[3])
  SSummary.rm[3,1]<-mean(s.a_trans.rm)
  SSummary.rm[3,2]<-mean(varc.a_trans.rm$betaran$estimate)
  SSummary.rm[3,3]<-sd(s.a_trans.rm)
  SSummary.rm[3,4]<-var(s.a_trans.rm)
  SSummary.rm[3,5]<-as.numeric(varc.a_trans.rm$sigma)
  SSummary.rm[3,6]<-as.numeric(varc.a_trans.rm$sigmasq[1])
  SSummary.rm[3,7]<-as.numeric(varc.a_trans.rm$sigmasq[2])
  SSummary.rm[3,8]<-as.numeric(varc.a_trans.rm$sigmasq[3])
  colnames(SSummary.rm)<-c("mean", "mean_shrunken","total_sd", "total_var", "proc_sd", "proc_var_estimate", "proc_var_lower", "proc_var_upper")
  row.names(SSummary.rm)<-c("J","A","A_trans")
  
  setwd(RESULTS_DIRECTORY)
  filename<-paste(SPECIES_CODE, "SSummary_conv.yrs.csv", sep='_')
  write.csv(SSummary.rm, filename)
  
  } else {
  
    if (("varc.a.rm" %in% ls()) & ("varc.a_trans.rm" %in% ls())) {
  # When variance component analysis with only converging years worked only for adult residents/adult transients
  
  SSummary.rm<-data.frame()
  SSummary.rm[1,1]<-mean(s.a.rm)
  SSummary.rm[1,2]<-mean(varc.a.rm$betaran$estimate)
  SSummary.rm[1,3]<-sd(s.a.rm)
  SSummary.rm[1,4]<-var(s.a.rm)
  SSummary.rm[1,5]<-as.numeric(varc.a.rm$sigma)
  SSummary.rm[1,6]<-as.numeric(varc.a.rm$sigmasq[1])
  SSummary.rm[1,7]<-as.numeric(varc.a.rm$sigmasq[2])
  SSummary.rm[1,8]<-as.numeric(varc.a.rm$sigmasq[3])
  SSummary.rm[2,1]<-mean(s.a_trans.rm)
  SSummary.rm[2,2]<-mean(varc.a_trans.rm$betaran$estimate)
  SSummary.rm[2,3]<-sd(s.a_trans.rm)
  SSummary.rm[2,4]<-var(s.a_trans.rm)
  SSummary.rm[2,5]<-as.numeric(varc.a_trans.rm$sigma)
  SSummary.rm[2,6]<-as.numeric(varc.a_trans.rm$sigmasq[1])
  SSummary.rm[2,7]<-as.numeric(varc.a_trans.rm$sigmasq[2])
  SSummary.rm[2,8]<-as.numeric(varc.a_trans.rm$sigmasq[3])
  colnames(SSummary.rm)<-c("mean", "mean_shrunken","total_sd", "total_var", "proc_sd", "proc_var_estimate", "proc_var_lower", "proc_var_upper")
  row.names(SSummary.rm)<-c("A", "A_trans")
  
    } else {
      
      SSummary.rm<-data.frame()
      SSummary.rm[1,1]<-mean(s.a.rm)
      SSummary.rm[1,2]<-mean(varc.a.rm$betaran$estimate)
      SSummary.rm[1,3]<-sd(s.a.rm)
      SSummary.rm[1,4]<-var(s.a.rm)
      SSummary.rm[1,5]<-as.numeric(varc.a.rm$sigma)
      SSummary.rm[1,6]<-as.numeric(varc.a.rm$sigmasq[1])
      SSummary.rm[1,7]<-as.numeric(varc.a.rm$sigmasq[2])
      SSummary.rm[1,8]<-as.numeric(varc.a.rm$sigmasq[3])

      colnames(SSummary.rm)<-c("mean", "mean_shrunken","total_sd", "total_var", "proc_sd", "proc_var_estimate", "proc_var_lower", "proc_var_upper")
      row.names(SSummary.rm)<-c("A")
            
    } 
  
  setwd(RESULTS_DIRECTORY)
  filename<-paste(SPECIES_CODE, "SSummary_conv.yrs.csv", sep='_')
  write.csv(SSummary.rm, filename)
  }

  return(SSummary.rm)

} # END OF FUNCTION 'VarianceComponent'


###################################################################################
# FUNCTION 'EstimateFecundity'
#
# RUN FECUNDITY ANALYSIS USING WINBUGS 1.4
#
# ARGUMENTS:
#     'p.table'   -- Results from function 'PCapResults'. Capture probabilities and abundances are used for fecundity analysis.
#     'MinAdults' -- The number of captured adults under which the relative density data will be discarded. 
#                 -- If the number of captured adults are too low, it is likely to have bias in the estimate of fecundity.
# 
# OUTPUTS:
#    lines in 'Debug' text file in DATA_DIRECTORY
#    lines in 'Intermediate Results' text file in RESULTS_DIRECTORY
#    SVG images of 2 MCMC chains separately for 'mean fecundity', 'temporal variance (SD)', and 'density effect' 
#    for corrected and uncorrect fecundity analyses stored in RESULTS_DIRECTORY  
###################################################################################

EstimateFecundity <- function (p.table=p.table, MinAdults = 4){
  
  p.table2<-p.table[,c("pop","year","Njuv","Nad","Nad_breed","corr.Njuv","corr.Nad","corr.Nad_breed","N","N_breed","corr.N","corr.N_breed",
                       "maps_rD","maps_prev.rD","maps_rD_ad","maps_prev.rD_ad")]
  
    # create a column of 1~19 for the years
  for(i in 1:nrow(p.table2)){
    p.table2$no.year[i]<-as.numeric(p.table2$year[i])
  }
  end<-ncol(p.table2)
  p.table2<-p.table2[,c(1,2,end,3:(end-1))] #move no.year next to year
   
    # leave only rows with values - one row for each year
  p.table2<-p.table2[!is.na(p.table2$Nad),]
   
    # set 'MinAdults' and leave only years with Nad_breed larger than 'MinAdults' 
    # if lower than MinAdults, juvenile:adult ratio does not have useful information for estimating fecundity
  p.table2<-p.table2[p.table2$Nad>MinAdults,]
  rownames(p.table2)<-NULL
  
  
      ######### Read in data for input in WinBUGS ############
	  
      # Bring in number of observed adults and juvenile captures
	  obsAdults<-p.table2$Nad
	  obsJuvs<-p.table2$Njuv
	   
	    # capture probability - time- and location- specific
    Juvp <- numeric(nrow(p.table2))
	  Juvplow <- numeric(nrow(p.table2))
	  Juvphigh <- numeric(nrow(p.table2))
	  Adultp <- numeric(nrow(p.table2))
	  Adultplow <- numeric(nrow(p.table2))
	  Adultphigh <- numeric(nrow(p.table2))

	  for(i in 1:nrow(p.table2)){
		ndx <- which((as.character(p.table$pop)==as.character(p.table2$pop[i]))&(p.table$year==p.table2$year[i]))
		tempj <- tapply(p.table$p.j[ndx],p.table$month[ndx],mean)   ##### KTS
		tempjlow <- tapply(p.table$p.j.lcl[ndx],p.table$month[ndx],mean)   ##### KTS
		tempjhigh <- tapply(p.table$p.j.ucl[ndx],p.table$month[ndx],mean)   ##### KTS
		tempa <- tapply(p.table$p.a[ndx],p.table$month[ndx],mean)   ##### KTS
		tempalow <- tapply(p.table$p.a.lcl[ndx],p.table$month[ndx],mean)   ##### KTS
		tempahigh <- tapply(p.table$p.a.ucl[ndx],p.table$month[ndx],mean)   ##### KTS
		
		Juvp[i]<-1-prod(1-tempj)  
    Juvplow[i]<-1-prod(1-tempjlow)
    Juvphigh[i]<-1-prod(1-tempjhigh)		
		Adultp[i]<-1-prod(1-tempa) 
    Adultplow[i]<-1-prod(1-tempalow)
    Adultphigh[i]<-1-prod(1-tempahigh)		
	  }
	  
  	  # MAPS density
	  density<-p.table2$maps_prev.rD
	  
	    # for year 1994, there is no maps_prev.rD
	    # imputate NAs with the mean
	  index<-which(is.na(p.table2$maps_prev.rD), arr.ind=TRUE)
	  density[index]<-mean(p.table2$maps_prev.rD, na.rm=T)

	    # mean of raw density
	  mean.raw.density<-mean(density)
	  sd.raw.density<-sd(density)
		  
	    # standardize the density (mean=0, sd=1)
	  standardize <- function(x){(x-mean(x))/sd(x)}
	  density<-standardize(density)  
	  
	    # years (from 1)
	  yrs<-p.table2$no.year
	  
	   
	  ######################################
    # 1) WinBUGS model for estimating fecundity with observation error 
    # corrected for capture probability separately for juveniles and adults)
 
    # Write a winbugs file from R
	  
    setwd(CODE_DIRECTORY)
	  sink("fecundity_MAPS_full.bug")
	  cat("
		 model {

  ## process model
for (i in 1:n) {
  Adultp[i] ~ dbeta(1,1)I(palow[i],pahigh[i])
  Juvp[i] ~ dbeta(1,1)I(pjlow[i],pjhigh[i])		  
  Adults[i] <- trunc(obsAdults[i]/Adultp[i])   #  estimated true number of adults in the population
  Juvs[i] <- exp(log(Adults[i]) + log.mean.fec + env.stoch.dev[yrs[i]] + beta.rD*rD[i])   # expected number of juveniles
}

# Observation model

for (i in 1:n) {
  obsJuvsexp[i] <- Juvs[i]*Juvp[i]
  obsJuvs[i] ~ dpois(obsJuvsexp[i])   ## DATA NODE
}

# Priors 

mean.fec ~ dunif(0.01, 10)        # mean fecundity [KTS: should we make these bounds a bit more biologically reasonable?]
env.stoch.tau ~ dgamma(0.1,0.1)
beta.rD ~ dnorm(0,0.01)            # precision=1/variance=0.001

# Derived terms

log.mean.fec <- log(mean.fec)
env.stoch.sd <- sqrt(1/env.stoch.tau)
env.stoch.var <- pow(env.stoch.sd,2)
env.stoch.sd.real <- sqrt((exp(env.stoch.var)-1)*exp(2*log.mean.fec + env.stoch.var))  # estimate standard deviation on real scale

# Estimated mean fecundity each year

for(t in 1:nt) {
  est.mean.fec[t] <- exp(log.mean.fec + env.stoch.dev[t])
}

# Random effects

for (t in 1:nt) {
  env.stoch.dev[t] ~ dnorm(0,env.stoch.tau)   # Deviates represent environmental stochasticity 
}  

} # end model
		  ", fill=TRUE)
	  sink()
	  
	  ##############################################
	  # RUN WINBUGS
	  
      # Load in data 
		  # WinBUGS brings in data in a list format
	  Data_full <- list(
		obsJuvs=obsJuvs,   # add one to uncorrect a bias introduced into the BUGS code (obsJuvs is never 0)
		obsAdults=obsAdults, 
		n=length(obsJuvs),
		rD=density,
		yrs=yrs,
		nt=max(unique(yrs)),
		palow=Adultplow,
		pjlow=Juvplow,
		pahigh=Adultphigh,
		#pa=Adultp,
		#pj=Juvp,
		pjhigh=Juvphigh
	  )
	  
	    # Set initial values  
    inits_full = function () list(
      #Adults=obsAdults/Adultp,
      #Juvs=obsJuvs/Juvp,
	  Adultp = Adultp,
	  Juvp = Juvp,
      mean.fec=rnorm(1,2,0.01),
      env.stoch.tau=rnorm(1, 25, 1), 
      beta.rD=-0.1
    )
   
	    #Send information to WinBUGS
	  Mod_full <- bugs(data=Data_full, inits=inits_full, 
				  parameters.to.save=c("mean.fec", "env.stoch.sd.real", "est.mean.fec", "beta.rD", "Adultp", "Juvp"), 
				  model.file="fecundity_MAPS_full.bug", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, 
          bugs.directory=BUGSdir, codaPkg=TRUE, over.relax=T, debug=TRUE)   
	  
      # read in results
	  FecResults_full = read.bugs(Mod_full)
      # To test for convergence, use the Gelman and Rubin's convergence diagnostic
      # Approximate convergence is diagnosed when the upper limit is close to 1.
      # Even if the chains are too short to allow the GR calculation, let the WinBUGS simulation run
    GR_full.W.E<-tryCatch.W.E(
      GR_full<-gelman.diag(FecResults_full, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
              multivariate=TRUE)
    )
  
      # output warning message if GR did not work
    if ( length( grep("Error", (paste(class(GR_full.W.E$value), collapse=" ")))) >0 ) {
      GR_full.W.E$warning <-"Computing Gelman and Rubin's convergence diagnostic for full model failed!"
    } else {GR_full.W.E$warning<-NULL}
  
      # print the GR convergence diagnostic in Results.txt
	  
	  # gelman.plot(FecResults_full)
    if ( length( grep("Error", (paste(class(GR_full.W.E$value), collapse=" ")))) >0 ) {
      ToResultsFile("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY CORRECTED FOR CAPTURE PROBABILITY

    Computing Gelman and Rubin's convergence diagnostic for full model failed!
    " , RESULTS_DIRECTORY, RESULTS_FILENAME)
      
      ToDebugFile("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY CORRECTED FOR CAPTURE PROBABILITY
    
    Computing Gelman and Rubin's convergence diagnostic for full model failed!
    " , DATA_DIRECTORY, DEBUG_FILENAME)
      ToDebugFile(paste(paste("Warning",GR_full.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
      
    } else {
      ToResultsFile(
        sprintf("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY CORRECTED FOR CAPTURE PROBABILITY
    
    Upper C.I. of Mean fecundity: %s \n
    Upper C.I. of Slope of DD relationship for fecundity: %s \n
    Upper C.I. of Temporal env variability (SD) in fecundity: %s \n
    *Approximate convergence is diagnosed when the upper limit is close to 1. 
    " , GR_full.W.E$value$psrf["mean.fec","Upper C.I."], GR_full.W.E$value$psrf["beta.rD","Upper C.I."], GR_full.W.E$value$psrf["env.stoch.sd.real","Upper C.I."]
        ),  RESULTS_DIRECTORY, RESULTS_FILENAME)
    }

      # read in values 	  
	  ModResults_full <- as.data.frame(FecResults_full[[1]])
	  if(nc>1){
		  for(c in 2:nc){
			ModResult_full <- rbind(ModResults_full,as.data.frame(FecResults_full[[c]]))
		  }
	  }
	  
  
    #######################################################################
    # 2) WinBUGS model for estimating fecundity without observation error 
    # not corrected for capture probability 
		# this is to show how crude calculation (simple Njuv/Nad) can result in biased estimate of fecundity
  
	    # Write a winbugs file from R
	  setwd(CODE_DIRECTORY)
	  sink("fecundity_MAPS_null.bug")
	  cat("
		 model {

  ## process model
for (i in 1:n) {		  
  #Adults[i] <- trunc(obsAdults[i]/Adultp[i])   #  estimated true number of adults in the population
  Juvsexp[i] <- exp(log(Adults[i]) + log.mean.fec + env.stoch.dev[yrs[i]] + beta.rD*rD[i])   # expected number of juveniles
}

# Observation model

for (i in 1:n) {
  Juvs[i] ~ dpois(Juvsexp[i])   ## DATA NODE
}

# Priors 

mean.fec ~ dunif(0.01, 10)        # mean fecundity [KTS: should we make these bounds a bit more biologically reasonable?]
env.stoch.tau ~ dgamma(0.1,0.1)
beta.rD ~ dnorm(0,0.01)            # precision=1/variance=0.001

# Derived terms

log.mean.fec <- log(mean.fec)
env.stoch.sd <- sqrt(1/env.stoch.tau)
env.stoch.var <- pow(env.stoch.sd,2)
env.stoch.sd.real <- sqrt((exp(env.stoch.var)-1)*exp(2*log.mean.fec + env.stoch.var))  # estimate standard deviation on real scale

# Estimated mean fecundity each year

for(t in 1:nt) {
  est.mean.fec[t] <- exp(log.mean.fec + env.stoch.dev[t])
}

# Random effects

for (t in 1:nt) {
  env.stoch.dev[t] ~ dnorm(0,env.stoch.tau)   # Deviates represent environmental stochasticity 
}  

} # end model
		  ", fill=TRUE)
	  sink()
	  
	  
	  ##############################################
	  # RUN WINBUGS
	  
	    # Load in data 
	    # WinBUGS brings in data in a list format
	  Data_null <- list(
		Juvs=obsJuvs,
		Adults=obsAdults,
		n=length(obsJuvs),
		rD=density,
		yrs=yrs,
		nt=max(unique(yrs))
	  )
	  
	    # Set initial values
	  inits_null = function () list(
	    mean.fec=rnorm(1,0.7,0.01),
	    env.stoch.tau=rnorm(1, 16, 5),
	    beta.rD=-0.1)
		  
	    #Send information to WinBUGS
	  Mod_null <- bugs(data=Data_null, inits=inits_null, 
				  parameters.to.save=c("mean.fec", "env.stoch.sd.real", "Juvs.pred","beta.rD", "est.mean.fec"), 
				  model.file="fecundity_MAPS_null.bug", n.thin=nt, n.chains=nc, n.burnin=nb,	n.iter=ni, 
          bugs.directory=BUGSdir, codaPkg=TRUE, over.relax=T, debug=TRUE)
	  
	    # Read the results back to R
	  FecResults_null = read.bugs(Mod_null)
	 
    # To test for convergence, use the Gelman and Rubin's convergence diagnostic
    # Approximate convergence is diagnosed when the upper limit is close to 1. 
    # Even if the chains are too short to allow the GR calculation, let the WinBUGS simulation run
    GR_null.W.E<-tryCatch.W.E(
      GR_null<-gelman.diag(FecResults_null, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
                         multivariate=TRUE)
    )
  
    # output warning message if GR did not work
    if ( length( grep("Error", (paste(class(GR_null.W.E$value), collapse=" ")))) >0 ) {
      GR_null.W.E$warning <-"Computing Gelman and Rubin's convergence diagnostic for null model failed!"
    } else {GR_null.W.E$warning<-NULL}
  
    # print the GR convergence diagnostic in Results.txt
  
    # gelman.plot(FecResults_null)
    if ( length( grep("Error", (paste(class(GR_null.W.E$value), collapse=" ")))) >0 ) {
      ToResultsFile("
      ----------------------------------------------------------------------------------------
      GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY NOT CORRECTED FOR CAPTURE PROBABILITY

      Computing Gelman and Rubin's convergence diagnostic for null model failed!
      " , RESULTS_DIRECTORY, RESULTS_FILENAME)
    
      ToDebugFile("
      ----------------------------------------------------------------------------------------
      GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY NOT CORRECTED FOR CAPTURE PROBABILITY
    
      Computing Gelman and Rubin's convergence diagnostic for null model failed!
      " , DATA_DIRECTORY, DEBUG_FILENAME)
      ToDebugFile(paste(paste("Warning",GR_null.W.E$warning, sep=": "), "\n", sep=""), DATA_DIRECTORY, DEBUG_FILENAME)
    
    } else {
      ToResultsFile(
        sprintf("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY NOT CORRECTED FOR CAPTURE PROBABILITY
            
    Upper C.I. of Mean fecundity: %s \n
    Upper C.I. of Slope of DD relationship for fecundity: %s \n
    Upper C.I. of Temporal env variability (SD) in fecundity: %s \n
    *Approximate convergence is diagnosed when the upper limit is close to 1. 
    " , GR_null.W.E$value$psrf["mean.fec","Upper C.I."], GR_null.W.E$value$psrf["beta.rD","Upper C.I."], GR_null.W.E$value$psrf["env.stoch.sd.real","Upper C.I."]
        ),  RESULTS_DIRECTORY, RESULTS_FILENAME)
    }
  
	    # read in values for nc chains
    ModResults_null <- as.data.frame(FecResults_null[[1]])
	  if(nc>1){
		  for(c in 2:nc){
			ModResults_null <- rbind(ModResults_null, as.data.frame(FecResults_null[[c]]))
		  }
	  }
	
	
    ###############################
	  # PLOT RESULTS 
	  setwd(RESULTS_DIRECTORY)
  
	    # plot multiple chains sepearately and compare visually
	
    filename <- paste(SPECIES_CODE,"BUGStrace_corrected.svg", sep="_")
    svg(file=filename,width=10,height=10, onefile=TRUE)
    par(mfrow=c(3,1))
    plot(as.data.frame(FecResults_full[[1]])$mean.fec,type="l",xlab="iterations",ylab="mean.fec")
    if(nc>1) lines(as.data.frame(FecResults_full[[2]])$mean.fec,col="red")
    plot(as.data.frame(FecResults_full[[1]])$beta.rD,type="l",xlab="iterations",ylab="beta.rD")
    if(nc>1)lines(as.data.frame(FecResults_full[[2]])$beta.rD,col="red")
    plot(as.data.frame(FecResults_full[[1]])$env.stoch.sd.real,type="l",xlab="iterations",ylab="env.stoch.sd")
    if(nc>1)lines(as.data.frame(FecResults_full[[2]])$env.stoch.sd.real,col="red")
    dev.off()

    filename <- paste(SPECIES_CODE,"BUGStrace_uncorrected.svg", sep="_")
    svg(file=filename,width=10,height=10, onefile=TRUE)
    par(mfrow=c(3,1))
    plot(as.data.frame(FecResults_null[[1]])$mean.fec,type="l",xlab="iterations",ylab="mean.fec")
    if(nc>1) lines(as.data.frame(FecResults_null[[2]])$mean.fec,col="red")
    plot(as.data.frame(FecResults_null[[1]])$beta.rD,type="l",xlab="iterations",ylab="beta.rD")
    if(nc>1)lines(as.data.frame(FecResults_null[[2]])$beta.rD,col="red")
    plot(as.data.frame(FecResults_null[[1]])$env.stoch.sd.real,type="l",xlab="iterations",ylab="env.stoch.sd")
    if(nc>1)lines(as.data.frame(FecResults_null[[2]])$env.stoch.sd.real,col="red")
    dev.off()
  

	    # MCMC
	  mean.fec.mcmc_full<-as.mcmc(ModResults_full$mean.fec)
	  beta.rD.mcmc_full<-as.mcmc(ModResults_full$beta.rD)
	  env.stoch.sd.mcmc_full<-as.mcmc(ModResults_full$env.stoch.sd.real)
    mean.fec.mcmc_null<-as.mcmc(ModResults_null$mean.fec)
    beta.rD.mcmc_null<-as.mcmc(ModResults_null$beta.rD)
    env.stoch.sd.mcmc_null<-as.mcmc(ModResults_null$env.stoch.sd.real)
  
    filename <- paste(SPECIES_CODE,"meanFec_BUGS_corrected.svg", sep="_")
	  svg(file=filename,width=5,height=5)
	  plot(mean.fec.mcmc_full, main="mean.fec")  
	  dev.off()
	  
    filename <- paste(SPECIES_CODE,"densityEffect_BUGS_corrected.svg", sep="_")
	  svg(file=filename,width=5,height=5)
	  plot(beta.rD.mcmc_full, main="beta.rD")
	  dev.off()
	  
    filename <- paste(SPECIES_CODE,"tempVar_BUGS_corrected.svg", sep="_")
	  svg(file=filename,width=5,height=5)
	  plot(env.stoch.sd.mcmc_full, main="env.stoch.sd")
	  dev.off()

    filename <- paste(SPECIES_CODE,"meanFec_BUGS_uncorrected.svg", sep="_")
    svg(file=filename,width=5,height=5)
    plot(mean.fec.mcmc_null, main="mean.fec")  
    dev.off()
  
    filename <- paste(SPECIES_CODE,"densityEffect_BUGS_uncorrected.svg", sep="_")
    svg(file=filename,width=5,height=5)
    plot(beta.rD.mcmc_null, main="beta.rD")
    dev.off()
  
    filename <- paste(SPECIES_CODE,"tempVar_BUGS_uncorrected.svg", sep="_")
    svg(file=filename,width=5,height=5)
    plot(env.stoch.sd.mcmc_null, main="env.stoch.sd")
    dev.off()
  
	  ###########################
	  #  STORE RESULTS IN TABLE
	  
	  # store 95% CI summary table
	  
	  res_mean.fec_full<-c(min(ModResults_full$mean.fec), mean(ModResults_full$mean.fec)-1.96*sd(ModResults_full$mean.fec), median(ModResults_full$mean.fec), mean(ModResults_full$mean.fec), mean(ModResults_full$mean.fec)+1.96*sd(ModResults_full$mean.fec), max(ModResults_full$mean.fec))
    res_beta.rD_full<-c(min(ModResults_full$beta.rD), mean(ModResults_full$beta.rD)-1.96*sd(ModResults_full$beta.rD), median(ModResults_full$beta.rD), mean(ModResults_full$beta.rD), mean(ModResults_full$beta.rD)+1.96*sd(ModResults_full$beta.rD), max(ModResults_full$beta.rD))
	  res_env.stoch.sd_full<-c(min(ModResults_full$env.stoch.sd.real), mean(ModResults_full$env.stoch.sd.real)-1.96*sd(ModResults_full$env.stoch.sd.real), median(ModResults_full$env.stoch.sd.real), mean(ModResults_full$env.stoch.sd.real), mean(ModResults_full$env.stoch.sd.real)+1.96*sd(ModResults_full$env.stoch.sd.real), max(ModResults_full$env.stoch.sd.real))
    FTable_full<-data.frame(res_mean.fec_full, res_beta.rD_full, res_env.stoch.sd_full)
	  row.names(FTable_full)<-c("min.","li(2.5% Qu.)","median","mean","ui(97.5% Qu.)","max.")
	  colnames(FTable_full)<-c("mean_fec","beta.rD","env.stoch.sd")
  
    res_mean.fec_null<-c(min(ModResults_null$mean.fec), mean(ModResults_null$mean.fec)-1.96*sd(ModResults_null$mean.fec), median(ModResults_null$mean.fec), mean(ModResults_null$mean.fec), mean(ModResults_null$mean.fec)+1.96*sd(ModResults_null$mean.fec), max(ModResults_null$mean.fec))
    res_beta.rD_null<-c(min(ModResults_null$beta.rD), mean(ModResults_null$beta.rD)-1.96*sd(ModResults_null$beta.rD), median(ModResults_null$beta.rD), mean(ModResults_null$beta.rD), mean(ModResults_null$beta.rD)+1.96*sd(ModResults_null$beta.rD), max(ModResults_null$beta.rD))
    res_env.stoch.sd_null<-c(min(ModResults_null$env.stoch.sd.real), mean(ModResults_null$env.stoch.sd.real)-1.96*sd(ModResults_null$env.stoch.sd.real), median(ModResults_null$env.stoch.sd.real), mean(ModResults_null$env.stoch.sd.real), mean(ModResults_null$env.stoch.sd.real)+1.96*sd(ModResults_null$env.stoch.sd.real), max(ModResults_null$env.stoch.sd.real))
    FTable_null<-data.frame(res_mean.fec_null, res_beta.rD_null, res_env.stoch.sd_null)
    row.names(FTable_null)<-c("min.","li(2.5% Qu.)","median","mean","ui(97.5% Qu.)","max.")
    colnames(FTable_null)<-c("mean_fec","beta.rD","env.stoch.sd")
	 	  
	  # save as .csv
	  setwd(RESULTS_DIRECTORY)
    filename<-paste(SPECIES_CODE, "FTable_full.csv", sep="_")
    write.csv(FTable_full, filename)
    filename<-paste(SPECIES_CODE, "FTable_null.csv", sep="_")
    write.csv(FTable_null, filename)
  
    FecundityResults<-list(
      Mean.rD=mean.raw.density,
      SD.rD=sd.raw.density,
      No.iter=ni,
      Burnin=nb,
      No.chains=nc,
      Thinningrate=nt,
      Data_full=Data_full,
      Data_null=Data_null,
      initial_full=inits_full,
      initial_null=inits_null,
      FResults_full=ModResults_full,
      FResults_null=ModResults_null,
      GR_full.W.E=GR_full.W.E,
      GR_null.W.E=GR_null.W.E,
      FTable_full=FTable_full,
      FTable_null=FTable_null
      )
  
      return(FecundityResults)
  
  } # END OF FUNCTION 'EstimateFecundity'


######################################################################################
# FUNCTION 'SummarizeForPopModel'
#
# SELECT OUT KEY RESULTS FOR CALCULATING DEMOGRAPHIC PARAMETERS USED IN POPULATION MODELING 
#
# ARGUMENTS:   
#    'RMarkData'   -- Results from function 'FormatForRMark'
#    'Apps'        -- Results from function 'ApparentS' to get the apparent survival estimates from time-constant model
#    'STempVar'    -- Results from function 'VarianceComponent' to get the temporal variability in S
#    'MarkResults' -- Final MARK fesults with density-dependence model 
#    'Fec'         -- Results from function 'EstimateFecundity'
#    'model.no'    -- Number of the density-dependent model in previous MARK results which is used to calculated density-dependence functions.
#
# OUPUT:
#    lines in 'Intermediate Results' text file in RESULTS_DIRECTORY
#
# NOTE:
#    If density relationship is negative for survival, the apparent survival estimates from density-dependent models are used,
#    whereas, if positive, the apparent survival estimates from time-constant models (stage model) are used.
######################################################################################

SummarizeForPopModel <- function(RMarkData, AppS, STempVar, MarkResults, Fec, model.no){ # writes text file output of all key population-level parameters, and also returns a list containing the same parameters

    # Apparent survival rates for juveniles and adults
    # TCM: from Time-constant model (Use when density relationship is positive)
    # DM: from Density model (Use when density relationship is negative) - see below
  Sjuv_TCM <- AppS$Sjuv
  Sad_TCM <- AppS$Sad
  
    # Temporal variability in survival rates for juveniles and adults
  if ("J" %in% row.names(STempVar)){
    Var_Sjuv <- list()
    Var_Sjuv$estimate <- STempVar["J","proc_var_estimate"]
    Var_Sjuv$lcl <- STempVar["J","proc_var_lower"]
    Var_Sjuv$ucl <- STempVar["J","proc_var_upper"]
    Var_Sad <- list()
    Var_Sad$estimate <- STempVar["A","proc_var_estimate"]
    Var_Sad$lcl <- STempVar["A","proc_var_lower"]
    Var_Sad$ucl <- STempVar["A","proc_var_upper"]
  } else {  
      Var_Sad <- list()
      Var_Sad$estimate <- STempVar["A","proc_var_estimate"]
      Var_Sad$lcl <- STempVar["A","proc_var_lower"]
      Var_Sad$ucl <- STempVar["A","proc_var_upper"]
  }
  
    # beta coefficients in model (S)
    # from model S(~st + maps_density + first_cap_bin:trans)p(~st + effort): model 2 in Density Mark Model
  S.betas<-MarkResults[[model.no]]$results$beta
  vcv<-MarkResults[[model.no]]$results$beta.vcv
  mean.maps_density<-mean(RMarkData$maps.ddl$S$maps_density)
    
  # function 'logitCI'
  # calculates CI (at 95% level)
  logitCI = function(val, se) {
    transSurv=logit(val)  # reconvert inv.logit(X) to X
    transSurvSE=deltamethod(list(~log(x1/(1-x1))), mean=val, cov=se^2)  #calculate SE for X
    transSurvLow=transSurv-1.96*transSurvSE  	#get X low/high value
    transSurvHigh=transSurv+1.96*transSurvSE
    survLow=inv.logit(transSurvLow)	#convert to inv.logit(X low), inv.logit(X high)
    survHigh=inv.logit(transSurvHigh)
    return(c(survLow, survHigh))
  }
  
  form1 = sprintf('~ exp(x1 + x2 + x3 * %f)/(1 + exp(x1 + x2 + x3 * %f))', mean.maps_density, mean.maps_density)  
  form2 = sprintf('~ exp(x1 + x3 * %f)/(1 + exp(x1 + x3 * %f))', mean.maps_density, mean.maps_density) 
  
    # Survival estimates at mean density
    # DM: from Density model (Use when density relationship is negative) - see below
    # Sjuv at mean density
  Sjuv_DM <- list()  
  Sjuv_DM$estimate<-inv.logit(S.betas$estimate[1] + S.betas$estimate[2] + S.betas$estimate[3]*mean.maps_density) # Juvenile (st=1): S(Intercept) + S(st) + S(maps_density)*mean(maps_density)
  Sjuv_DM$SE<-deltamethod(as.formula(form1), mean=S.betas$estimate, vcv, ses=TRUE) # deltamethod: approximating the standard error of a transformation g(X) of a random variable X
  Sjuv_logitCI<-logitCI(Sjuv_DM$estimate, Sjuv_DM$SE)
  Sjuv_DM$lcl<-Sjuv_logitCI[1]
  Sjuv_DM$ucl<-Sjuv_logitCI[2]
    
    # Sad at mean density
  Sad_DM <- list()  
  Sad_DM$estimate<-inv.logit(S.betas$estimate[1] + S.betas$estimate[3]*mean.maps_density) # Adult residents (st=1): S(Intercept) + S(maps_density)*mean(maps_density)
  Sad_DM$SE<-deltamethod(as.formula(form2), mean=S.betas$estimate, vcv, ses=TRUE) # deltamethod: approximating the standard error of a transformation g(X) of a random variable X
  Sad_logitCI<-logitCI(Sad_DM$estimate, Sad_DM$SE)
  Sad_DM$lcl<-Sad_logitCI[1]
  Sad_DM$ucl<-Sad_logitCI[2]
    
    # beta for intercept
  S_intcpt<-list()
  S_intcpt$estimate<-S.betas$estimate[1]
  S_intcpt$lcl<-S.betas$lcl[1]
  S_intcpt$ucl<-S.betas$ucl[1]
    
    # beta for stage
  S_st<-list()
  S_st$estimate<-S.betas$estimate[2]
  S_st$lcl<-S.betas$lcl[2]
  S_st$ucl<-S.betas$ucl[2]
  
    # beta for density
  S_dens<-list()
  S_dens$estimate<-S.betas$estimate[3]
  S_dens$lcl<-S.betas$lcl[3]
  S_dens$ucl<-S.betas$ucl[3]
  
    # Fecundity related parameters
  F_mean <- list()
  F_mean$estimate <- Fec$FTable_full$mean_fec[4]
  F_mean$lcl <- Fec$FTable_full$mean_fec[2]
  F_mean$ucl <- Fec$FTable_full$mean_fec[5]
  
  F_beta_rD <- list()
  F_beta_rD$estimate <- Fec$FTable_full$beta.rD[4]
  F_beta_rD$lcl <- Fec$FTable_full$beta.rD[2]
  F_beta_rD$ucl <- Fec$FTable_full$beta.rD[5]
  
  SD_F <- list()
  SD_F$estimate <- Fec$FTable_full$env.stoch.sd[4]
  SD_F$lcl <- Fec$FTable_full$env.stoch.sd[2]
  SD_F$ucl <- Fec$FTable_full$env.stoch.sd[5]
  
    # mean and SD of raw densities, and maximum rD above which ceiling-type density dependence will be used
  MeanDens<-FecundityResults$Mean.rD
  SD_Dens<-FecundityResults$SD.rD
  MaxPopDens <- RMarkData$max.rD

  if ("Var_Sjuv" %in% ls()){
  MPparameters<-list(
    Sjuv_TCM=Sjuv_TCM,
    Sad_TCM=Sad_TCM,
    Sjuv_DM=Sjuv_DM,
    Sad_DM=Sad_DM,
    Var_Sjuv=Var_Sjuv,
    Var_Sad=Var_Sad,
    S_intcpt=S_intcpt,
    S_st=S_st,
    S_dens=S_dens,
    F_mean=F_mean,
    SD_F=SD_F,
    F_beta_rD=F_beta_rD,
    MeanDens=MeanDens,
    SD_Dens=SD_Dens,
    MaxPopDens=MaxPopDens)
  } else {
    MPparameters<-list(
      Sjuv_TCM=Sjuv_TCM,
      Sad_TCM=Sad_TCM,
      Sjuv_DM=Sjuv_DM,
      Sad_DM=Sad_DM,
      Var_Sad=Var_Sad,
      S_intcpt=S_intcpt,
      S_st=S_st,
      S_dens=S_dens,
      F_mean=F_mean,
      SD_F=SD_F,
      F_beta_rD=F_beta_rD,
      MeanDens=MeanDens,
      SD_Dens=SD_Dens,
      MaxPopDens=MaxPopDens)
  }
 
    # output parameter values to RESULT.txt
  if ("Var_Sjuv" %in% ls()){
  ToResultsFile(
      paste("
      ----------------------------------------------------------------------------------------
      PARAMETER VALUES FOR POPULATION MODEL BEFORE CORRECTING FOR APPARENT SURVIVAL \n \n",
      paste("1. Apparent survival rate of juveniles from time-constant model:",Sjuv_TCM$estimate,"\n"),
      paste("2. Apparent survival rate of adults from time-constant model:",Sad_TCM$estimate,"\n"),
      paste("3. Apparent survival rate of juveniles from Density model:",Sjuv_DM$estimate,"\n"),
      paste("4. Apparent survival rate of adults from Density model:",Sad_DM$estimate,"\n"),
      paste("5. Temporal variability in survival of juveniles:",Var_Sjuv$estimate,"\n"),
      paste("6. Temporal variability in survival for adults:",Var_Sad$estimate,"\n"),
      paste("7. Intercept of density-dependence relationship for survival in logit scale:",S_intcpt$estimate,"\n"),
      paste("8. Juvenile effect in the density-dependence function for survival in logit scale:", S_st$estimate,"\n"),
      paste("9. Slope of density-dependence relationship for survival in logit scale:",S_dens$estimate,"\n"),
      paste("10. Mean fecundity:",F_mean$estimate,"\n"),
      paste("11. Temporal variability in fecundity:",SD_F$estimate^2,"\n"),
      paste("12. Slope of density-dependence relationship for fecundity in log scale:",F_beta_rD$estimate,"\n"),
      paste("13. Mean of MAPS density:",MeanDens,"\n"),
      paste("14. SD of MAPS density:",SD_Dens,"\n"),
      paste("15. Maximum MAPS density above which ceiling-type density-dependence will be assumed:",MaxPopDens,"\n")
      ), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }  else {
    ToResultsFile(
      paste("
      ----------------------------------------------------------------------------------------
      PARAMETER VALUES FOR POPULATION MODEL BEFORE CORRECTING FOR APPARENT SURVIVAL \n \n",
            paste("1. Apparent survival rate of juveniles from time-constant model:",Sjuv_TCM$estimate,"\n"),
            paste("2. Apparent survival rate of adults from time-constant model:",Sad_TCM$estimate,"\n"),
            paste("3. Apparent survival rate of juveniles from Density model:",Sjuv_DM$estimate,"\n"),
            paste("4. Apparent survival rate of adults from Density model:",Sad_DM$estimate,"\n"),
            paste("5. Temporal variability in survival of juveniles: NA \n"),
            paste("6. Temporal variability in survival for adults:",Var_Sad$estimate,"\n"),
            paste("7. Intercept of density-dependence relationship for survival in logit scale:",S_intcpt$estimate,"\n"),
            paste("8. Juvenile effect in the density-dependence function for survival in logit scale:", S_st$estimate,"\n"),
            paste("9. Slope of density-dependence relationship for survival in logit scale:",S_dens$estimate,"\n"),
            paste("10. Mean fecundity:",F_mean$estimate,"\n"),
            paste("11. Temporal variability in fecundity:",SD_F$estimate^2,"\n"),
            paste("12. Slope of density-dependence relationship for fecundity in log scale:",F_beta_rD$estimate,"\n"),
            paste("13. Mean of MAPS density:",MeanDens,"\n"),
            paste("14. SD of MAPS density:",SD_Dens,"\n"),
            paste("15. Maximum MAPS density above which ceiling-type density-dependence will be assumed:",MaxPopDens,"\n")
      ), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
    
  return(MPparameters)
  
} # END OF FUNCTION 'SummarizeForPopModel'


#########################################################################################################
# FUNCTION 'SummaryMP'
#
# CORRECT ALL DEMOGRAPHIC PARAMETERS FOR APPARENT SURVIVAL AND CREATE STAGE AND SD MATRIX
#
# ARGUMENTS:
#     'Data'      -- Results from function 'SummarizeForPopModel'
#     'TrendData' -- BBS trend used to correct for apparent survival
#
# OUTPUT: 
#     lines in 'Intermediate Results' text file in RESULTS_DIRECTORY
#     'Population Model Summary' text file in RESULTS_DIRECTORY
#########################################################################################################

SummaryMP <- function(Data, TrendData){
    
  if(!exists("MAPSTrend"))  cat("WARNING: MAPSTrend not defined! Please run ExtractTrend")
  
  # READ IN RESULTS FROM MAPS ANALYSIS
  
  #### test to see if the DD survival model is in effect
  
  DD_FLAG <- ifelse(Data$S_dens$estimate<0,TRUE,FALSE)
  
  Sjuv <- Data$Sjuv_TCM
  if(DD_FLAG) Sjuv <- Data$Sjuv_DM
  Sad <- Data$Sad_TCM
  if(DD_FLAG) Sad <- Data$Sad_DM
  Var_Sjuv <- Data$Var_Sjuv
  Var_Sad <- Data$Var_Sad
  S_intcpt <- Data$S_intcpt
  S_st <- Data$S_st
  S_dens <- Data$S_dens
  F_mean <- Data$F_mean
  SD_F <- Data$SD_F
  F_beta_rD <- Data$F_beta_rD
  MeanDens <- Data$MeanDens
  SD_Dens <- Data$SD_Dens
  MaxPopDens <- Data$MaxPopDens
    
  ToResultsFile("
                ---------------------------------------------------------
                DESCRIPTION OF VARIABLE NAMES
                F_mean            Mean fecundity
                F_beta_rD         Slope of DD relationship for fecundity, log space
                MaxPopDens        Maximum population density
                SD_F              Temporal env variability (SD) in fecundity
                MeanDens          Mean population density (for standardizign density covariate)
                SD_Dens           StDev of population density (for standardizing density covariate)
                Sad               Mean adult survival
                Sjuv              Mean juv survival
                Var_Sad           Temporal env variability (Variance) in adult survival
                Var_Sjuv          Temporal env variability (Variance) in juv survival
                S_intcpt          Intercept of DD relationship for survival, logit space
                S_dens            Slope of DD relationship for survival, logit space
                S_st              Juvenile effect in the DD function for survival          
                ", RESULTS_DIRECTORY, RESULTS_FILENAME)			
  

  ###  CALCULATE TRUE SURVIVAL (apparent to actual survival)
  
  ### use the upper and lower bounds of trends
  
  real.lambda <- list()
  real.lambda$estimate <- TrendData$estimate
  real.lambda$lcl <- TrendData$lcl
  real.lambda$ucl <- TrendData$ucl
 
  allcombs <- expand.grid(real.lambda=c(real.lambda$lcl,real.lambda$estimate,real.lambda$ucl),Sad=c(Sad$lcl,Sad$estimate,Sad$ucl),Sjuv=c(Sjuv$lcl,Sjuv$estimate,Sjuv$ucl),F_mean=c(F_mean$lcl,F_mean$estimate,F_mean$ucl))
  allcombs_ndx <- expand.grid(real.lambda.ndx=c(1,2,3),Sad.ndx=c(1,2,3),Sjuv.ndx=c(1,2,3),F_mean.ndx=c(1,2,3))
  allcombs$correction.factor <- allcombs$real.lambda/(allcombs$Sad + allcombs$Sjuv*allcombs$F_mean)
  allcombs$corrected.Sad <- allcombs$Sad * allcombs$correction.factor
  allcombs$corrected.Sjuv <- allcombs$Sjuv * allcombs$correction.factor
  allcombs <- cbind(allcombs,allcombs_ndx)
  
  corrected.Sad <- list()
  pointEstNdx <- with(allcombs,which((Sjuv.ndx==2)&(F_mean.ndx==2)&(Sad.ndx==2)&(real.lambda.ndx==2)))
  corrected.Sad$estimate <- with(allcombs,corrected.Sad[pointEstNdx])
  corrected.Sad$lcl <- with(allcombs,min(corrected.Sad))
  corrected.Sad$ucl <- with(allcombs,max(corrected.Sad))

  corrected.Sjuv <- list()
  corrected.Sjuv$estimate <- with(allcombs,corrected.Sjuv[pointEstNdx])
  corrected.Sjuv$lcl <- with(allcombs,min(corrected.Sjuv))
  corrected.Sjuv$ucl <- with(allcombs,max(corrected.Sjuv)) 
  
  correction.factor <- list()
  correction.factor$estimate <- with(allcombs,correction.factor[pointEstNdx])
  correction.factor$lcl <- with(allcombs,min(correction.factor))
  correction.factor$ucl <- with(allcombs,max(correction.factor))

  FSa <- list()
  FSa$estimate <- corrected.Sad$estimate *  F_mean$estimate 
  FSa$lcl <- corrected.Sad$lcl *  F_mean$lcl
  FSa$ucl <- corrected.Sad$ucl *  F_mean$ucl
  
  FSj <- list()
  FSj$estimate <- corrected.Sjuv$estimate *  F_mean$estimate 
  FSj$lcl <- corrected.Sjuv$lcl *  F_mean$lcl
  FSj$ucl <- corrected.Sjuv$ucl *  F_mean$ucl  
  
  ###   ASSEMBLE STAGE MATRIX
  
  ## stage.matrix [ row, column ]
  ## stage.matrix = ( ( (F * Sj), (F *Sad) ), ( Sj, Sad ) )
  
  stage.matrix <- list()
  
  stage.matrix$estimate <- matrix(c(0,0,0,0),nrow=2)
  stage.matrix$lcl <- matrix(c(0,0,0,0),nrow=2)
  stage.matrix$ucl <- matrix(c(0,0,0,0),nrow=2)

  stage.matrix$estimate[1,1] <- FSj$estimate  # corrected.Sjuv$estimate *  F_mean$estimate
  stage.matrix$estimate[1,2] <- FSa$estimate  # corrected.Sad$estimate *  F_mean$estimate
  stage.matrix$estimate[2,1] <- corrected.Sjuv$estimate 
  stage.matrix$estimate[2,2] <- corrected.Sad$estimate 
  
  stage.matrix$lcl[1,1] <- FSj$lcl  # corrected.Sjuv$lcl *  F_mean$lcl
  stage.matrix$lcl[1,2] <- FSa$lcl # corrected.Sad$lcl *  F_mean$lcl
  stage.matrix$lcl[2,1] <- corrected.Sjuv$lcl 
  stage.matrix$lcl[2,2] <- corrected.Sad$lcl 
  
  stage.matrix$ucl[1,1] <- FSj$ucl #corrected.Sjuv$ucl *  F_mean$ucl
  stage.matrix$ucl[1,2] <- FSa$ucl # corrected.Sad$ucl *  F_mean$ucl
  stage.matrix$ucl[2,1] <- corrected.Sjuv$ucl 
  stage.matrix$ucl[2,2] <- corrected.Sad$ucl
  
  ### ASSEMBLE STANDARD DEVIATION MATRIX (ENV. STOCHASTICITY)
  
  ## Fecundity process variance from sqr(env.stoch.sd)
  fec_variance <- list()
  fec_variance$estimate <- (SD_F$estimate)^2
  fec_variance$lcl <- (SD_F$lcl)^2
  fec_variance$ucl <- (SD_F$ucl)^2

  ## compute the CV for adult survival
  CV_Sad <- list()
  CV_Sad$estimate <- sqrt(Var_Sad$estimate)/Sad$estimate
  CV_Sad$lcl <- sqrt(Var_Sad$lcl)/Sad$estimate   # smallest CV is smallest sd divided by the point estimate for survival
  CV_Sad$ucl <- sqrt(Var_Sad$ucl)/Sad$estimate   # smallest CV is smallest sd divided by the point estimate for survival
  
  ## compute the CV for juvenile survival
  
  JuvSDFlag <- length(Var_Sjuv)>0
  if(JuvSDFlag) JuvSDFlag <- (Var_Sjuv$lcl>0)
  if(JuvSDFlag){
    CV_Sjuv <- list()
    CV_Sjuv$estimate <- sqrt(Var_Sjuv$estimate)/Sjuv$estimate
    CV_Sjuv$lcl <- sqrt(Var_Sjuv$lcl)/Sjuv$estimate   # smallest CV is smallest sd divided by the point estimate for survival
    CV_Sjuv$ucl <- sqrt(Var_Sjuv$ucl)/Sjuv$estimate   # smallest CV is smallest sd divided by the point estimate for survival
  }
  
  ## correct temporal variation in adult survival. Hold CV constant
  corrected.Var_Sad <- list()
  corrected.Var_Sad$estimate <- (corrected.Sad$estimate * CV_Sad$estimate)^2
  corrected.Var_Sad$lcl <- (corrected.Sad$estimate * CV_Sad$lcl)^2
  corrected.Var_Sad$ucl <- (corrected.Sad$estimate * CV_Sad$ucl)^2

  
  ## Sjuv variance cannot be calculated for most species, so it's based on CV of Sad
  corrected.Var_Sjuv <- list()  
  if(JuvSDFlag){             
    ## correct temporal variation in juvenile survival. Hold CV constant
	  corrected.Var_Sjuv$estimate <- (corrected.Sjuv$estimate * CV_Sjuv$estimate)^2
	  corrected.Var_Sjuv$lcl <- (corrected.Sjuv$estimate * CV_Sjuv$lcl)^2
	  corrected.Var_Sjuv$ucl <- (corrected.Sjuv$estimate * CV_Sjuv$ucl)^2   # if no issues, keep the results from MARK
  } else {
	  corrected.Var_Sjuv$estimate <- (corrected.Sjuv$estimate * CV_Sad$estimate)^2
	  corrected.Var_Sjuv$lcl <- (corrected.Sjuv$estimate * CV_Sad$lcl)^2
	  corrected.Var_Sjuv$ucl <- (corrected.Sjuv$estimate * CV_Sad$ucl)^2     # otherwise, use CV for adults
  }
  
  ## FSj and FSa terms (composite terms for the stage matrix...)
  
  ## Develop SD matrix- specifically, the stdev matrix elements combining fecundity and juvenile and adult survival rates
  
  FSj_variance <- list()
  FSa_variance <- list()
  if(CORRELATION == 0){    # exact variance when correlation is zero 
    ## Variance of (F*Sjuv)                                                  
    FSj_variance$estimate <- fec_variance$estimate * corrected.Sjuv$estimate^2 + 
	                         corrected.Var_Sjuv$estimate *  F_mean$estimate^2 + fec_variance$estimate * corrected.Var_Sjuv$estimate
							 
	FSj_variance$lcl <- fec_variance$lcl * corrected.Sjuv$lcl^2 + 
	                         corrected.Var_Sjuv$lcl *  F_mean$lcl^2 + fec_variance$lcl * corrected.Var_Sjuv$lcl
							 
	FSj_variance$ucl <- fec_variance$ucl * corrected.Sjuv$ucl^2 + 
	                         corrected.Var_Sjuv$ucl *  F_mean$ucl^2 + fec_variance$ucl * corrected.Var_Sjuv$ucl

							 ## Variance of (F*Sad)
    FSa_variance$estimate <- fec_variance$estimate * corrected.Sad$estimate^2 + 
	                         corrected.Var_Sad$estimate *  F_mean$estimate^2 + fec_variance$estimate * corrected.Var_Sad$estimate
	FSa_variance$lcl <- fec_variance$lcl * corrected.Sad$lcl^2 + 
	                         corrected.Var_Sad$lcl *  F_mean$lcl^2 + fec_variance$lcl * corrected.Var_Sad$lcl
	FSa_variance$ucl <- fec_variance$ucl * corrected.Sad$ucl^2 + 
	                         corrected.Var_Sad$ucl *  F_mean$ucl^2 + fec_variance$ucl * corrected.Var_Sad$ucl
  }else{
    ## Variance of (F*Sjuv)
    FSj_variance$estimate <- fec_variance$estimate * corrected.Sjuv$estimate^2 + 
	                         corrected.Var_Sjuv$estimate *  F_mean$estimate^2 + 2 *  F_mean$estimate * 
							 corrected.Sjuv$estimate * sqrt(fec_variance$estimate) * 
							 sqrt(corrected.Var_Sjuv$estimate) * CORRELATION
    FSj_variance$lcl <- fec_variance$lcl * corrected.Sjuv$lcl^2 + 
	                         corrected.Var_Sjuv$lcl *  F_mean$lcl^2 + 2 *  F_mean$lcl * 
							 corrected.Sjuv$lcl * sqrt(fec_variance$lcl) * 
							 sqrt(corrected.Var_Sjuv$lcl) * CORRELATION
    FSj_variance$ucl <- fec_variance$ucl * corrected.Sjuv$ucl^2 + 
	                         corrected.Var_Sjuv$ucl *  F_mean$ucl^2 + 2 *  F_mean$ucl * 
							 corrected.Sjuv$ucl * sqrt(fec_variance$ucl) * 
							 sqrt(corrected.Var_Sjuv$ucl) * CORRELATION
    ## Variance of (F*Sad)
    FSa_variance$estimate <- fec_variance$estimate * corrected.Sad$estimate^2 + 
	                         corrected.Var_Sad$estimate *  F_mean$estimate^2 + 2 *  F_mean$estimate * 
							 corrected.Sad$estimate * sqrt(fec_variance$estimate) * 
							 sqrt(corrected.Var_Sad$estimate) * CORRELATION
    FSa_variance$lcl <- fec_variance$lcl * corrected.Sad$lcl^2 + 
	                         corrected.Var_Sad$lcl *  F_mean$lcl^2 + 2 *  F_mean$lcl * 
							 corrected.Sad$lcl * sqrt(fec_variance$lcl) * 
							 sqrt(corrected.Var_Sad$lcl) * CORRELATION
    FSa_variance$ucl <- fec_variance$ucl * corrected.Sad$ucl^2 + 
	                         corrected.Var_Sad$ucl *  F_mean$ucl^2 + 2 *  F_mean$ucl * 
							 corrected.Sad$ucl * sqrt(fec_variance$ucl) * 
							 sqrt(corrected.Var_Sad$ucl) * CORRELATION
  }
  
  
    # output to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
    B. Vital Rates and Temporal Variability:
--------------------------------------------------------------------------
    Param         Mean (95%% conf int)          Std dev (95%% conf int)
--------------------------------------------------------------------------
    Sa            %.3f (%.3f - %.3f)        %.3f (%.3f - %.3f)
    Sj            %.3f (%.3f - %.3f)        %.3f (%.3f - %.3f)
    F             %.3f (%.3f - %.3f)        %.3f (%.3f - %.3f)
    F*Sj          %.3f (%.3f - %.3f)        %.3f (%.3f - %.3f)
    F*Sa          %.3f (%.3f - %.3f)        %.3f (%.3f - %.3f)
--------------------------------------------------------------------------

  Notes:
  The mean values are at average density and average environmental conditions.
  The standard deviations are used to model temporal environmental variability; they exclude variability due to sampling (or demographic stochasticity).
    "), round(corrected.Sad$estimate,3), round(corrected.Sad$lcl,3), round(corrected.Sad$ucl,3), round(sqrt(corrected.Var_Sad$estimate),3), round(sqrt(corrected.Var_Sad$lcl),3), round(sqrt(corrected.Var_Sad$ucl),3),
            round(corrected.Sjuv$estimate,3), round(Sjuv$lcl,3), round(Sjuv$ucl,3), round(sqrt(corrected.Var_Sjuv$estimate),3), round(sqrt(corrected.Var_Sjuv$lcl),3), round(sqrt(corrected.Var_Sjuv$ucl),3),
            round(F_mean$estimate,3), round(F_mean$lcl,3), round(F_mean$ucl,3), round(SD_F$estimate,3), round(SD_F$lcl,3), round(SD_F$ucl,3),
            round(FSj$estimate,3),round(FSj$lcl,3),round(FSj$ucl,3),round(sqrt(FSj_variance$estimate),3),round(sqrt(FSj_variance$lcl),3),round(sqrt(FSj_variance$ucl),3),
            round(FSa$estimate,3),round(FSa$lcl,3),round(FSa$ucl,3),round(sqrt(FSa_variance$estimate),3),round(sqrt(FSa_variance$lcl),3),round(sqrt(FSa_variance$ucl),3))      
    
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  if(CORRELATION==1){
    ToPopModelFile (
    sprintf(("
  Correlation between S and F = %s
  The standard deviations for F*Sj and F*Sa assume full correlation between survival and fecundity.
    "), CORRELATION), dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  } else{
    if (CORRELATION==0){
    ToPopModelFile (
      sprintf(("
  Correlation between S and F = %s
  The standard deviations for F*Sj and F*Sa assume zero correlation between survival and fecundity.
    "), CORRELATION), dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
    } else {
    ToPopModelFile (
      sprintf(("
  Correlation between S and F = %s
  The standard deviations for F*Sj and F*Sa assume a correlation of [%s] between survival and fecundity.
    "), CORRELATION, CORRELATION), dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
    }
  }
    
  ## Standard deviations matrix 
  ## SD.matrix [ row, column ]
  ## stage.matrix = ( ( (F * Sj), (F *Sad) ), ( Sj, Sad ) )
  
  SD.matrix <- list()
  SD.matrix$estimate <- matrix(c(0,0,0,0),nrow=2)
  SD.matrix$estimate[1,1] <- sqrt(FSj_variance$estimate)
  SD.matrix$estimate[1,2] <- sqrt(FSa_variance$estimate)
  SD.matrix$estimate[2,1] <- sqrt(corrected.Var_Sjuv$estimate) 
  SD.matrix$estimate[2,2] <- sqrt(corrected.Var_Sad$estimate)
  
  SD.matrix$lcl <- matrix(c(0,0,0,0),nrow=2)
  SD.matrix$lcl[1,1] <- sqrt(FSj_variance$lcl)
  SD.matrix$lcl[1,2] <- sqrt(FSa_variance$lcl)
  SD.matrix$lcl[2,1] <- sqrt(corrected.Var_Sjuv$lcl) 
  SD.matrix$lcl[2,2] <- sqrt(corrected.Var_Sad$lcl)
  
  SD.matrix$ucl <- matrix(c(0,0,0,0),nrow=2)
  SD.matrix$ucl[1,1] <- sqrt(FSj_variance$ucl)
  SD.matrix$ucl[1,2] <- sqrt(FSa_variance$ucl)
  SD.matrix$ucl[2,1] <- sqrt(corrected.Var_Sjuv$ucl) 
  SD.matrix$ucl[2,2] <- sqrt(corrected.Var_Sad$ucl)
  
  # output Stage Matrix to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
  C. The Stage Matrix:
|----------------------------------|-----------------------------------|
|  %.3f (%.3f - %.3f)           |   %.3f (%.3f - %.3f)           |
|----------------------------------|-----------------------------------|
|  %.3f (%.3f - %.3f)           |   %.3f (%.3f - %.3f)           |
|----------------------------------|-----------------------------------|
             "), 
			 round(stage.matrix$estimate[1,1],3),round(stage.matrix$lcl[1,1],3),round(stage.matrix$ucl[1,1],3), 
			 round(stage.matrix$estimate[1,2],3),round(stage.matrix$lcl[1,2],3),round(stage.matrix$ucl[1,2],3),
			 round(stage.matrix$estimate[2,1],3),round(stage.matrix$lcl[2,1],3),round(stage.matrix$ucl[2,1],3), 
			 round(stage.matrix$estimate[2,2],3),round(stage.matrix$lcl[2,2],3),round(stage.matrix$ucl[2,2],3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  
  # output to SD Matrix to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
  D. The Standard Deviation Matrix:
  
|----------------------------------|-----------------------------------|
|  %.3f (%.3f - %.3f)           |   %.3f (%.3f - %.3f)           |
|----------------------------------|-----------------------------------|
|  %.3f (%.3f - %.3f)           |   %.3f (%.3f - %.3f)           |
|----------------------------------|-----------------------------------|  
    "), 
			 round(SD.matrix$estimate[1,1],3),round(SD.matrix$lcl[1,1],3),round(SD.matrix$ucl[1,1],3), 
			 round(SD.matrix$estimate[1,2],3),round(SD.matrix$lcl[1,2],3),round(SD.matrix$ucl[1,2],3),
			 round(SD.matrix$estimate[2,1],3),round(SD.matrix$lcl[2,1],3),round(SD.matrix$ucl[2,1],3), 
			 round(SD.matrix$estimate[2,2],3),round(SD.matrix$lcl[2,2],3),round(SD.matrix$ucl[2,2],3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  # output Density-dependence functions to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
  E. Density Dependence:

  When (N/K) > [%.3f], the current population size is truncated at [%.3f]*K
  or the stage matrix and stage abundances are decreased such that the expected population size in the next time step is [%.3f]*K.
             "), round(MaxPopDens,3), round(MaxPopDens,3), round(MaxPopDens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  ToPopModelFile (
    sprintf(("
  When N/K < [%.3f], fecundity is calculated as the following function of density (N/K) at each time step:
  F  = F_mean * exp( F_beta_rD * ( (PopDens - MeanDens) / SD_Dens ) )
             "), round(MaxPopDens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  if (F_beta_rD$estimate<0){
    ToPopModelFile (
    sprintf(("  
  F  = %.3f * exp( (%.3f) * ( ( (N/K)  - %.3f  ) / %.3f ) )
  Negative density-dependence is detected for F, therefore, this function is used in the population model.
             "), round(F_mean$estimate,3), round(F_beta_rD$estimate,3), round(MeanDens,3), round(SD_Dens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  } else {
    ToPopModelFile (
      sprintf(("
  F  = %.3f * exp( (%.3f) * ( ( (N/K)  - %.3f  ) / %.3f ) )
  Positive density-dependence is detected for F, therefore, this function is not used in the population model.
             "), round(F_mean$estimate,3), round(F_beta_rD$estimate,3), round(MeanDens,3), round(SD_Dens,3))
      , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  }
  
  ToPopModelFile (
    sprintf(("
  When N/K < [%s], survival rates are calculated as the following functions of density (N/K) at each time step:
  Sj = [ exp(S_intcpt + S_st + S_dens*PopDens)/(1 + exp(S_intcpt + S_st + S_dens*PopDens)) ] * Corr_factor
  Sa = [ exp(S_intcpt        + S_dens*PopDens)/(1 + exp(S_intcpt        + S_dens*PopDens)) ] * Corr_factor
             "), round(MaxPopDens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  if (S_dens$estimate<0) {
    ToPopModelFile (
    sprintf(("     
  Sj = [ exp((%s) + (%s) * (N/K)) / (1 + exp((%s) + (%s) * (N/K))) ] * %s
  Sa = [ exp((%s) + (%s) * (N/K)) / (1 + exp((%s) + (%s) * (N/K))) ] * %s
  Negative density-dependence is detected for S, therefore, this functions are used in the population model.
             "), round(S_intcpt$estimate+S_st$estimate,3), round(S_dens$estimate,3), round(S_intcpt$estimate+S_st$estimate,3), round(S_dens$estimate,3), round(correction.factor$estimate,3),
                 round(S_intcpt$estimate,3), round(S_dens$estimate,3), round(S_intcpt$estimate,3), round(S_dens$estimate,3), round(correction.factor$estimate,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  } else {
    ToPopModelFile (
      sprintf(("     
  Sj = [ exp((%s) + (%s) * (N/K)) / (1 + exp((%s) + (%s) * (N/K))) ] * %s
  Sa = [ exp((%s) + (%s) * (N/K)) / (1 + exp((%s) + (%s) * (N/K))) ] * %s
  Positive density-dependence is detected for S, therefore, this functions are not used in the population model.
             "), round(S_intcpt$estimate+S_st$estimate,3), round(S_dens$estimate,3), round(S_intcpt$estimate+S_st$estimate,3), round(S_dens$estimate,3), round(correction.factor$estimate,3),
      round(S_intcpt$estimate,3), round(S_dens$estimate,3), round(S_intcpt$estimate,3), round(S_dens$estimate,3), round(correction.factor$estimate,3))
, dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  }
	
	  #######################################
    # output density dependence model to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
Density dependence function values:
--------------------------------------------------------------------------
    Param         Mean (95%% conf int)          
--------------------------------------------------------------------------
    S_intcpt        %.3f ((%.3f) - (%.3f))       
    S_st            %.3f ((%.3f) - (%.3f))       
    S_dens          %.3f ((%.3f) - (%.3f))        
    F_beta_rD       %.3f ((%.3f) - (%.3f))
    Corr_factor     %.3f ((%.3f) - (%.3f))	
    MeanDens        %.3f  
    SD_Dens         %.3f 
    MaxPopDens      %.3f 	
--------------------------------------------------------------------------

    "), round(S_intcpt$estimate,3), round(S_intcpt$lcl,3), round(S_intcpt$ucl,3), 
	    round(S_st$estimate,3), round(S_st$lcl,3), round(S_st$ucl,3),
		round(S_dens$estimate,3), round(S_dens$lcl,3), round(S_dens$ucl,3),
		round(F_beta_rD$estimate,3), round(F_beta_rD$lcl,3), round(F_beta_rD$ucl,3),
		round(correction.factor$estimate,3), round(correction.factor$lcl,3), round(correction.factor$ucl,3),
		round(MeanDens,3), round(SD_Dens,3), round(MaxPopDens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
	if ("CV_Sjuv" %in% ls()) {
	Result<-list(Data=Data, real.lambda=real.lambda, allcombs=allcombs, 
               corrected.Sad=corrected.Sad, corrected.Sjuv=corrected.Sjuv, correction.factor=correction.factor,
               FSa=FSa, FSj=FSj, stage.matrix=stage.matrix, fec_variance=fec_variance,
               CV_Sad=CV_Sad, CV_Sjuv=CV_Sjuv, corrected.Var_Sad=corrected.Var_Sad, corrected.Var_Sjuv=corrected.Var_Sjuv,
               FSj_variance=FSj_variance, FSa_variance=FSa_variance, SD.matrix=SD.matrix)
	} else {
	  Result<-list(Data=Data, real.lambda=real.lambda, allcombs=allcombs, 
	               corrected.Sad=corrected.Sad, corrected.Sjuv=corrected.Sjuv, correction.factor=correction.factor,
	               FSa=FSa, FSj=FSj, stage.matrix=stage.matrix, fec_variance=fec_variance,
	               CV_Sad=CV_Sad, corrected.Var_Sad=corrected.Var_Sad, corrected.Var_Sjuv=corrected.Var_Sjuv,
	               FSj_variance=FSj_variance, FSa_variance=FSa_variance, SD.matrix=SD.matrix)
	}

  return(Result)
  
} # END OF FUNCTION 'SummaryMP'
  

#########################################################################################################
# FUNCTION 'WriteMasterMPFile'
#
# BUILD RAMAS ".MP" FILE BASED ON INFORMATION FROM PREVIOUS ANALYSES
#
# ARGUMENT:
#     'Result'  -- Results from function 'SummaryMP': demographic parameters all corrected for apparent survival
#
# OUTPUT: 
#      MP files with best, lower, and upper bound estimates in POPMODELS_DIRECTORY
#########################################################################################################
  
  WriteMasterMPFile <- function(Result){
    
    # READ IN RESULTS FROM MAPS ANALYSIS  
  
    Data<-Result$Data
    
    #### test to see if the DD survival model is in effect
    DD_FLAG <- ifelse(Data$S_dens$estimate<0,TRUE,FALSE)
    
    Sjuv <- Data$Sjuv_TCM
    if(DD_FLAG) Sjuv <- Data$Sjuv_DM
    Sad <- Data$Sad_TCM
    if(DD_FLAG) Sad <- Data$Sad_DM
    Var_Sjuv <- Data$Var_Sjuv
    Var_Sad <- Data$Var_Sad
    S_intcpt <- Data$S_intcpt
    S_st <- Data$S_st
    S_dens <- Data$S_dens
    F_mean <- Data$F_mean
    SD_F <- Data$SD_F
    F_beta_rD <- Data$F_beta_rD
    MeanDens <- Data$MeanDens
    SD_Dens <- Data$SD_Dens
    MaxPopDens <- Data$MaxPopDens
    
    correction.factor<-Result$correction.factor
    stage.matrix<-Result$stage.matrix
    SD.matrix<-Result$SD.matrix
    
  ###  WRITE THE .MP FILE
  
  closeAllConnections()
  
  
  ## CONSTRUCT THE POPULATION LINE FOR THE .MP FILE
  
  ## Population string.  One line of text, constructed as string1 + string2
  ## The following line assumes N=1000 &  K=10000.  The user can change these in RAMAS.
  string1 <- paste("Pop1,0.000,0.000,",INITIAL_ABUNDANCE,",EX,0,",CARRYING_CAPACITY,",0,0,,0,1,0,TRUE,1,1,1,0,1,0,1,0,0,0,1.0,1,1,",sep="")
  
  ##  string2: make a string of comma-separated values out of the 9 varables listed below 
  ## S:(Intercept)    S:st    S:maps_density     F_mean    beta_rD	   Mean_Density   SD_Density    MaxDens
  ## such that it looks like this:
  ## '0.0256,-1.8453,0.0524,1.6960,-0.0828,1.5018,1.1340,3.14'
  string2 <- list()
  string2$estimate <- paste(S_intcpt$estimate, S_st$estimate, S_dens$estimate, F_mean$estimate, F_beta_rD$estimate, MeanDens, SD_Dens, MaxPopDens, correction.factor$estimate, sep=",") 
  string2$lcl <- paste(S_intcpt$lcl, S_st$lcl, S_dens$lcl, F_mean$lcl, F_beta_rD$lcl, MeanDens, SD_Dens, MaxPopDens, correction.factor$lcl, sep=",")
  string2$ucl <- paste(S_intcpt$ucl, S_st$ucl, S_dens$ucl, F_mean$ucl, F_beta_rD$ucl, MeanDens, SD_Dens, MaxPopDens, correction.factor$ucl, sep=",")
  
  Population.String <- list()
  Population.String$estimate <- paste(string1, string2$estimate)
  Population.String$lcl <- paste(string1, string2$lcl)
  Population.String$ucl <- paste(string1, string2$ucl)
  
  MP_initializeFiles <- function(){
    setwd(POPMODELS_DIRECTORY)
    MP_line1 <<- paste("Metapopulation input file (",METAPOP_VERSION,") map= ",sep="")
	MP_title <<- list()
    MP_title$estimate <<- paste("Population model for ",SPECIES_CODE,", parameterized using central point estimates from MAPS data.",sep="")
	MP_title$lcl <<- paste("Population model for ",SPECIES_CODE,", parameterized using lower bound estimates from MAPS data.",sep="")
	MP_title$ucl <<- paste("Population model for ",SPECIES_CODE,", parameterized using lower bound estimates from MAPS data.",sep="")
	MP_fileName <<- list()
    MP_fileName$estimate <<- paste(SPECIES_CODE,"_pointEstimates.mp",sep="_")
	MP_fileName$lcl <<- paste(SPECIES_CODE,"_lowerBound.mp",sep="_")
	MP_fileName$ucl <<- paste(SPECIES_CODE,"_upperBound.mp",sep="_")
    MP_dllLocation <<- paste(POPMODELS_DIRECTORY,"\\AvianDD.dll",sep="")
    
    ## open a blank file for writing
	MP_fileConnection <<- list()
    MP_fileConnection$estimate <<- file(MP_fileName$estimate,"w") 
    MP_fileConnection$lcl <<- file(MP_fileName$lcl,"w")
    MP_fileConnection$ucl <<- file(MP_fileName$ucl,"w")	
	
    flush(MP_fileConnection$estimate)  # remove existing contents
	flush(MP_fileConnection$lcl)
	flush(MP_fileConnection$ucl)
  }
  
  writeLines2<- function(text, conList){
     if(!is.list(text)){
       writeLines(text,MP_fileConnection$estimate)
       writeLines(text,MP_fileConnection$lcl)
       writeLines(text,MP_fileConnection$ucl)	 
	 }else{
       writeLines(text$estimate,MP_fileConnection$estimate)
       writeLines(text$lcl,MP_fileConnection$lcl)
       writeLines(text$ucl,MP_fileConnection$ucl)	 
	 }
  }
  
  close2 <- function(conList){
    close(conList$estimate)
    close(conList$lcl)
    close(conList$ucl)	
  }
  
  MP_writeBlock1 <- function(){
    writeLines2(MP_line1,MP_fileConnection)
    writeLines2(MP_title,MP_fileConnection)
    for(i in 1:4){
      writeLines2("",MP_fileConnection)
    }
    writeLines2(as.character(REPLICATES),MP_fileConnection)
    writeLines2(as.character(TIMESTEPS),MP_fileConnection)
    writeLines2("TRUE",MP_fileConnection)
    writeLines2("2 FALSE",MP_fileConnection)
    for(i in 1:2){
      writeLines2("",MP_fileConnection)
    }
    writeLines2("Local",MP_fileConnection)
    writeLines2("",MP_fileConnection)
    writeLines2("not spread",MP_fileConnection)
    writeLines2("0.0000",MP_fileConnection)
    writeLines2("0.0000,0.0000,0.0000,0.0000",MP_fileConnection)
    for(i in 1:2){
      writeLines2("",MP_fileConnection)
    }
    writeLines2("Local",MP_fileConnection)
    writeLines2("",MP_fileConnection)
    writeLines2("not spread",MP_fileConnection)
    writeLines2("0.0000",MP_fileConnection)
    writeLines2("0.0000,0.0000,0.0000,0.0000",MP_fileConnection)
    writeLines2("False,Zero",MP_fileConnection)
    writeLines2("all vital rates",MP_fileConnection)
    writeLines2("Lognormal,0",MP_fileConnection)
    writeLines2("0.000000",MP_fileConnection)
    writeLines2("count in total",MP_fileConnection)
    writeLines2("1 (F, S, K correlated)",MP_fileConnection)
    writeLines2("No",MP_fileConnection)
    writeLines2("AllStages",MP_fileConnection)
    writeLines2("No",MP_fileConnection)
    writeLines2("UD",MP_fileConnection)
    writeLines2(MP_dllLocation,MP_fileConnection)
    writeLines2("1",MP_fileConnection)
    writeLines2("years",MP_fileConnection)
    writeLines2("OnlyFemale",MP_fileConnection)
    writeLines2("1",MP_fileConnection)
    writeLines2("Monogamous",MP_fileConnection)
    writeLines2("2.0",MP_fileConnection)
    writeLines2("2.0",MP_fileConnection)
    writeLines2("0.0000",MP_fileConnection)
    writeLines2("0",MP_fileConnection)
  }
  
  MP_writeBlock2 <- function(){
    writeLines2("Migration",MP_fileConnection)
    writeLines2("FALSE",MP_fileConnection)
    writeLines2("0.000,0.00000,0.00000,0.00000",MP_fileConnection)
    writeLines2(" 0,",MP_fileConnection)
    writeLines2("Correlation",MP_fileConnection)
    writeLines2("FALSE",MP_fileConnection)
    writeLines2("0.000,0.00000,0.00000",MP_fileConnection)
    writeLines2(" 1,",MP_fileConnection)
    writeLines2("1 type(s) of stage matrix",MP_fileConnection)
    writeLines2("default",MP_fileConnection)
    writeLines2("1.000000",MP_fileConnection)
    writeLines2("1.000000",MP_fileConnection)
    writeLines2("0",MP_fileConnection)
  }
  
  MP_writeBlock3 <- function(){
    writeLines2("Constraints Matrix",MP_fileConnection)
    writeLines2("0.000000 0.000000",MP_fileConnection)
    for(i in 1:8){
      writeLines2("1.000000 1.000000",MP_fileConnection)
    }
    writeLines2("-1 -1",MP_fileConnection)
    writeLines2("Juvenile",MP_fileConnection)
    writeLines2("1.00000000",MP_fileConnection)
    writeLines2("FALSE",MP_fileConnection)
    writeLines2("TRUE",MP_fileConnection)
    writeLines2("         1",MP_fileConnection)
    writeLines2("Adult",MP_fileConnection)
    writeLines2("1.00000000",MP_fileConnection)
    writeLines2("FALSE",MP_fileConnection)
    writeLines2("TRUE",MP_fileConnection)
    writeLines2("         1",MP_fileConnection)
    writeLines2("0 (pop mgmnt)",MP_fileConnection)
    writeLines2("0.0",MP_fileConnection)
    writeLines2("0.0",MP_fileConnection)
    writeLines2("10",MP_fileConnection)
    writeLines2("-End of file-",MP_fileConnection)
  }
  
  MP_construct <- function(){
    MP_initializeFiles()
    MP_writeBlock1()
    writeLines2(Population.String,MP_fileConnection)
    MP_writeBlock2()
	
    ## WRITE STAGE MATRIX
	topRow <- list()
	topRow$estimate <- paste(stage.matrix$estimate[1,],collapse=" ")
	topRow$lcl <- paste(stage.matrix$lcl[1,],collapse=" ")
	topRow$ucl <- paste(stage.matrix$ucl[1,],collapse=" ")
	bottomRow <- list()
	bottomRow$estimate <- paste(stage.matrix$estimate[2,],collapse=" ")
	bottomRow$lcl <- paste(stage.matrix$lcl[2,],collapse=" ")
	bottomRow$ucl <- paste(stage.matrix$ucl[2,],collapse=" ")
    writeLines2(topRow,MP_fileConnection)
    writeLines2(bottomRow,MP_fileConnection)
    writeLines2("1 type(s) of st.dev. matrix",MP_fileConnection)
    writeLines2("default",MP_fileConnection)
	
    ## WRITE SD MATRIX
	topRow <- list()
	topRow$estimate <- paste(SD.matrix$estimate[1,],collapse=" ")
	topRow$lcl <- paste(SD.matrix$lcl[1,],collapse=" ")
	topRow$ucl <- paste(SD.matrix$ucl[1,],collapse=" ")
	bottomRow <- list()
	bottomRow$estimate <- paste(SD.matrix$estimate[2,],collapse=" ")
	bottomRow$lcl <- paste(SD.matrix$lcl[2,],collapse=" ")
	bottomRow$ucl <- paste(SD.matrix$ucl[2,],collapse=" ")
    writeLines2(topRow,MP_fileConnection)
    writeLines2(bottomRow,MP_fileConnection)
    MP_writeBlock3()
    close2(MP_fileConnection)
  }
  
  MP_construct()
  
} # END OF FUNCTION 'WriteMasterMPFile'
  

################################# END OF 'MAPS_AllFunctions.R' ###############################

