########################################################################################################################
####  R Functions used for accessing and processing MAPS data, building datafiles
####  for RMark, estimating demographic parameteters, and assembing a population model. 

####  [From Ryu et al., "Developing Population Models with Data from Marked Individuals", submitted to MEE, April 2015] 
########################################################################################################################

#################################################################################
######   FUNCTION FOR CATCHING AND STORING ERRORS (but allowing the program to keep running)
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
######  SET UP INFORMATIVE DEBUG FILE
################################################################################


InitializeDebugFile <- function(dir=DATA_DIRECTORY,filename=DEBUG_FILENAME){
	   # clear file
	setwd(dir)
	sink(filename)
	cat(" ")
             ### file header
	cat("
	----------------------------------------------------------------------
	DEBUG MESSAGES FOR MAPS TO MODELS ANALYSIS (warnings and errors) 

	Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" submitted to MEE, April 2015

	This document contains details about the \"MAPS to Models\" analysis- warning and error messages. Please read this
	document carefully before running any population models.

	The organization of the document reflects the order of operations in the workflow: 
		DATA EXTRACTION >> SURVIVAL ESTIMATION (Program MARK) >> FECUNDITY ESTIMATION (WinBUGS) >> POPULATION MODEL ASSEMBLY (Ramas Metapop)

    Please notify the authors to suggest improvements to this document! 		
	")
	sink()
}

	 ## call this function to append text to the debug file....
		 ## NOTE: use \" for quotes. Don't use commas!
ToDebugFile <- function(InText,dir=DATA_DIRECTORY,filename=DEBUG_FILENAME){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}


####################################################################################
######  SET UP INFORMATIVE RESULTS FILE
####################################################################################

InitializeResultsFile <- function(dir=RESULTS_DIRECTORY,filename=RESULTS_FILENAME){
	setwd(dir)
	sink(filename)
	cat(" ")
	      ## Set up the file header

	cat("
	----------------------------------------------------------------------
	RESULTS FOR MAPS TO MODELS ANALYSIS

	Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" submitted to MEE, April 2015

	This document contains details about the \"MAPS to Models\" analysis results. Please read this
	document carefully before running any population models derived from this analysis.

	The organization of the document reflects the order of operations in the workflow: 
		DATA EXTRACTION >> SURVIVAL ESTIMATION (Program MARK) >> FECUNDITY ESTIMATION (WinBUGS) >> POPULATION MODEL ASSEMBLY (Ramas Metapop)
		
	")
	sink()
}


     ## call this function to append text to the results file....
         ## NOTE: use \" for quotes. Don't use commas!
ToResultsFile <- function(InText,dir=RESULTS_DIRECTORY,filename=RESULTS_FILENAME){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}


####################################################################################
######  SET UP POPULATION MODEL SUMMARY FILE
####################################################################################

InitializePopModelFile <- function(dir=RESULTS_DIRECTORY,filename=POPMODELSUMMARY_FILENAME){
  setwd(dir)
  sink(filename)
  cat(" ")
  ## Set up the file header
  
  cat(
  sprintf("
	----------------------------------------------------------------------
	POPULATION MODEL SUMMARY

	Reference: Ryu et al: \"Developing Population Models with Data from Marked Individuals\" submitted to MEE, April 2015

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
  * Fecundity (F) is the number of daugthers per female.
  * The stage matrix has the following structure:

  F*Sj  F*Sa
  Sj	Sa

	", DEBUG_FILENAME, RESULTS_FILENAME, METATPOP_FILENAME, SPECIES_CODE)
  )
  sink()
}


## call this function to append text to the population model summary file....
## NOTE: use \" for quotes. Don't use commas!
ToPopModelFile <- function(InText,dir=RESULTS_DIRECTORY,filename=POPMODELSUMMARY_FILENAME){
  setwd(dir)
  sink(filename,append=T)
  cat(InText)
  sink()
}

###########################################################################

#########################################
# Function 'RetrieveData'
# SUMMARY: - Connects to MAPS database and extracts information for the target species necessary for estimating demographic parameters.   
# ARGUMENTS:
#			'dir' -- project data directory
#			'odbc.source' -- ODBC database connection that links to a database with raw MAPS data
#           'population_map' -- data frame that associates each MAPS location with a distinct biological population

# RETURNS:
#   'MAPS_ProcessedData', which is a list object that includes:
#       'band.data'        --       Data frame object with the following fields:  
#                                     band           -- unique band ID associated with the bird 
#                                     captureyear    -- year of capture 
#                                     month          -- month of capture
#                                     birthyear      -- estimated birth year 
#                                     actualage      -- estimated age at capture
#                                     agegroup       -- ??
#                                     loc            -- MAPS location where bird was captured
#                                     freq           -- ??
#                                     station        -- MAPS banding station where bird was captured. Listed separately for months 5,6,7,8
#       'band.data_allmon' -- data frame: same as 'band.data', but including all months, not just focal (breeding) months
#       'effort'           -- information of the trapping effort at each station for each month of interest
#       'sim.stations'     -- distinct stations (and its corresponding loc) for species in the focal set... [KTS: why is this needed??]
#			
###    NOTE: must have an ODBC database connection.
###                 - use function odbcDataSources() to see what data sources are locally available
###                 - to set up an ODBC data source, follow the instructions provided in the attached Word document (XXX)
###                 - on Windows machines you may need to use the 32-bit version of R 
###############################################################################################################

RetrieveData <- function(dir=DATA_DIRECTORY,odbc.source="maps",population_map=population_map){

	  # Connect to database	
	con = odbcConnect(odbc.source) # KTS: I can only run using 32-bit version of R

	  # Develop SQL query
	stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear, month, birthyear, actualage, agegroup, CP, BP, loc, station, 1 as freq ")
	stmt2=sprintf("FROM finalMAPSBAND")
	stmt3=sprintf("WHERE dupeSpec=0 AND spec=\'%s\' AND captureyear >= %s AND captureyear <= %s AND month in (5,6,7,8);", SPECIES_CODE, BEGINYEAR, ENDYEAR)
	stmt4=sprintf("%s %s %s", stmt1, stmt2, stmt3)
	band.data=sqlQuery(con, paste(stmt4), errors=FALSE)
	odbcClose(con)
		
	if (length(band.data)==1) {
		if (band.data[1]==-1) cat("Function create.banddata failed! \n")
		break
	}

	#############################################################
  # 1') Get Bird banding information from Access for all months 
	# this is for transients
	# previously, Function create.banddata   
	# output is 'band.data' - band, captureyear, month, birthyear, actualage, agegroup, loc, freq, station

	# Get connection to Access database
  # odbcDataSources()
	con = odbcConnect(odbc.source)   # KTS: I can only run using 32-bit version of R

	stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear, month, birthyear, actualage, agegroup, CP, BP, loc, station, 1 as freq ")
	stmt2=sprintf("FROM finalMAPSBAND")
	stmt3=sprintf("WHERE dupeSpec=0 AND spec=\'%s\' AND captureyear >= %s AND captureyear <= %s;", SPECIES_CODE, BEGINYEAR, ENDYEAR)
	stmt4=sprintf("%s %s %s", stmt1, stmt2, stmt3)
	band.data_allmon=sqlQuery(con, paste(stmt4), errors=FALSE)
	odbcClose(con)

	if (length(band.data_allmon)==1) {
	  if (band.data_allmon[1]==-1) cat("Function create.banddata failed! \n")
	  break
	}

  ##############################################################
	# 2) Get effort information from Access
	# previously, Function create.effort - get effort information from Access
	# output is 'effort' - loc, year, month, effort

	# Get connection to Access database
	# odbcDataSources()
	con = odbcConnect(odbc.source)  # KTS: I can only run using 32-bit version of R

	stmt11 = sprintf('SELECT loc, year(date) as year, month(date) as month, round(sum(cint(length)*((Cint(mid(end,1,2))*60+Cint(mid(end,3))*10) - (Cint(mid(start,1,2))*60+Cint(mid(start,3))*10) ))/1440,2) as effort')
	stmt22 = sprintf('FROM mapsef GROUP BY loc, year(date), month(date)')
	stmt33 = sprintf('%s %s', stmt11, stmt22)
	effort=sqlQuery(con, paste(stmt33), errors=FALSE)
	odbcClose(con)

	if (length(effort)==1) {
		if (effort[1]==-1) ToDebugFile("Function create.effort failed! \n")
		break
	}	

	############################################################
	# Get bird's station data from Access
	# previously, Function get.stationdata - get bird  information from Access
	# output is 'sim.stations' - station, loc
	# distinct stations (and its corresponding loc) for species in our list

  # Get connection to Access database
	# odbcDataSources()
	con = odbcConnect(odbc.source) 
		
	stmt111 = sprintf("SELECT distinct station, loc")
	stmt222 = sprintf("FROM finalmapsband")
	stmt333 = sprintf("where spec in ('GRCA','WOTH','WEVI','YBCH','HOWA','COYE','BCCH','CACH','NOCA')")
	stmt444 = sprintf("%s %s %s", stmt111, stmt222, stmt333)
	sim.stations=sqlQuery(con, paste(stmt444), errors=FALSE)
	odbcClose(con)
		
	if (length(sim.stations)==1) {
		if (sim.stations[1]==-1) ToDebugFile("Function get.stationdata failed! \n")
		break
	}

	###############################################################
  # make 'station.loc.pop' - station/loc/pop matrix
	# match population in population_map with loc in sim.stations
	# 'station.loc.pop' - station, loc, populations
	
  station.loc.pop<-cbind(sim.stations, NA)
	colnames(station.loc.pop)<-c("station","loc","pop")
	for (i in 1:nrow(station.loc.pop)) {
	  index<-which(as.character(population_map$loc)==as.character(station.loc.pop$loc)[i])
	  station.loc.pop$pop[i]<-population_map$pop[index]
	}

	###############################################################
	# Build structure for storing all key results...
	
	MAPS_ProcessedData = list()
	MAPS_ProcessedData$band.data <- band.data
	MAPS_ProcessedData$band.data_allmon <- band.data_allmon
	MAPS_ProcessedData$effort <- effort
	MAPS_ProcessedData$sim.stations <- sim.stations
	MAPS_ProcessedData$station.loc.pop <- station.loc.pop
	
    return(MAPS_ProcessedData)
} # end of function 'RetrieveData'


######################################################################################

######################################################################################
# Function 'FormatForCMR'
# SUMMARY: Reshapes the raw data extracted from the MAPS database into a 'capture history' format useful for performing capture-recapture analysis 
###    ARGUMENTS
#			'MAPSData' -- results from function 'RetrieveData'- data retrieved and processed from MAPS database
#			'dir' -- project data directory

###    RETURNS 'CMR_Data', which is a data frame containing all the information necessary to run a capture-mark-recapture analysis
#######################################################################################

FormatForCMR <- function(MAPSData=ProcessedData, dir=DATA_DIRECTORY){	

    # Extract the processed data from MAPS
	data=MAPSData$band.data
	data.allMon <- MAPSData$band.data_allMon
	effort=MAPSData$effort
	station.loc.pop <- MAPSData$station.loc.pop
	
	CMR_Data <- list()
	
		# Structuring input file for processing
	work=data[,c("band","captureyear","month","loc","birthyear","agegroup","actualage","freq","station","CP","BP")]
	work2=cbind(work[c(1)], year_month=paste(work[,c(2)],work[,c(3)],sep="_"),  work[c(4:11,2,3)])

    ###############################
	  ## MAKE "FAKE BIRDS" TO ENSURE THAT ALL LOCATIONS/TIMES ARE REPRESENTED IN THE CAPTURE HISTORY
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
		  
		  work2[ndx,3:ncol(work2)] <- work2[1,3:ncol(work2)] #use the below code for BCCH and YEWA
		  # For BCCH and YEWA, the fake bird is put into location that has no effort values for the corresponding year_month
		  # none of the locations have full effort values for all the year_month
		  #work2[ndx,3:ncol(work2)] <- work2[2,3:ncol(work2)]
		}
	  }
	}
	work2$year_month <- as.factor(work2$year_month)   # reconvert to factor

		# 'work3' - band, year_month, loc, birthyear, agegroup(A/J), actualage(from -1 to 7;0=juveniles,all other=adults), freq, station, CP, BP, captureyear, month
		# work3 puts the bands in order by year_month 
	work3=work2[order(work2$year_month), ]	
		
		# 'work_reshape' - band, year_month, loc, birthyear, agegroup, actualage, freq, CP, BP
	work_reshape=work3[,c("band", "year_month", "loc", "birthyear", "agegroup", "actualage", "freq", "CP", "BP")]			
		# 'work_density' - band, captureyear, month, station
	work_density=work3[,c("band","captureyear","month","station")]		

		#### make sure only a single obs for each unique individual and time period   [NOTE: takes a while to run!]
		# leave only one capture for each year-month for each individual
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
	
	  # write to Debug file
	ToDebugFile("
  ----------------------------------------------------------------------------
	The following individuals were captured more than once in a given month: \n 
	")
	ToDebugFile(
	as.character(flags)
	)

		# "explode" the dataset so that each survey event gets its own column 
		# 'test'- band, loc, birthyear, freq, agegroup1994_5, actualage1994_5, CP1994_5, BP1994_5, agegroup1994_6, actualage1994_6, etc..
	  # assume no movement and don't feed 'loc' as v.names
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
		# from the previous brought-in effort matrix (loc, year, month, effort), navigate for each individual where it was located for specific year_month and put that effort value
		# in the form of a new matrix with individual as rows and year_month as columns 

	occasion_names=work3$year_month
	ch_data=test

		#  generate matrices and summary variables for linking effort with the capture history matrix
	realbout <- as.character(unique(occasion_names))
	realyear <- as.numeric(substr(realbout,1,4))
	realmonth <- as.numeric(substr(realbout,6,6))
	realloc <- as.character(ch_data$loc)

	firstyear <- min(realyear)
	lastyear <- max(realyear)
	nbouts <- length(realyear)
	nyears <- length(unique(realyear))
	nind <- nrow(ch_data)

		# shape into matrices with same dimensions as capture history
	yearmat <- matrix(rep(realyear,times=nind),nrow=nind,ncol=nbouts,byrow=T)
	monmat <- matrix(rep(realmonth,times=nind),nrow=nind,ncol=nbouts,byrow=T)
	locmat <- matrix(rep(realloc,times=nbouts),nrow=nind,ncol=nbouts,byrow=F)

    # generate a new effort matrix
	effortmatrix <- matrix(0,nrow=nind,ncol=nbouts)   
	for(i in 1:nind){
	  for(j in 1:nbouts){
		ndx <- which((effort$loc==locmat[i,j])&(effort$year==yearmat[i,j])&(effort$month==monmat[i,j]))
		effortmatrix[i,j] <- ifelse(length(ndx)>0, effort$effort[ndx],NA)
	  }
	}
	effortmat<-effortmatrix
		
	  ######## IMPORTANT ########
		# Where NA in effort matrix(ind as rows, year_month as columns), which means there is no data, 
		# replace "1"s and "0"s in ch2 matrix (ind as rows, agegroupyear_month as colums) with "." (=NA) for survey occasions with no data....
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
    paste("To view the capture history, go to file location:", dir, "and \n look for file: ", filename))


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
		index<-which((ProcessedData$band.data_allmon$band==indvs[i])&(ProcessedData$band.data_allmon$captureyear==years[j]))
		recap_within_sameyear[i,j]<-ifelse(length(index)==1,1,0)
	  }
	}

	  # final 'pot_trans' 
	  # for each individual, whether potential transient in a given year
	pot_trans<-matrix(NA,nrow=nrow(test2),ncol=nyears) 
	for (i in 1:nrow(pot_trans)){
	  for (j in 1:ncol(pot_trans)){
		# if trans=1 and recap_within_sameyear=1, then it is a potential transient
		pot_trans[i,j]<-trans[i,j]*recap_within_sameyear[i,j]   # potential transient=1
	  }
	}

	pot_transcolnames=paste("trans", unique(yearlab),sep="")
	colnames(pot_trans)=pot_transcolnames
	head(pot_trans)
	

	  ##################### CMR_Data (structure for sending along) #####################################
		
	# only st and effort and potential trasient as temporarily varying individual covariate
	CMR_Data$MasterCapHist=data.frame(band=test2[c(1)], ch=ch, freq=test2[c(4)], loc=test2[c(2)], stratadf, effortdf, pot_trans)
	CMR_Data$MasterCapHist$ch=as.character(CMR_Data$MasterCapHist$ch)

	  # add population into CMR_Data
	CMR_Data$MasterCapHist[, "pop"] <- NA
	for (i in 1:nrow(CMR_Data$MasterCapHist)){
	  index<-which(station.loc.pop$loc==as.character(CMR_Data$MasterCapHist$loc[i]))
	  CMR_Data$MasterCapHist$pop[i]<-ifelse(length(index)>0, station.loc.pop$pop[index], NA)
	}
	CMR_Data$MasterCapHist$pop=as.factor(CMR_Data$MasterCapHist$pop)   # change pop as factor
	  # move pop after loc
	end<-ncol(CMR_Data$MasterCapHist)
	CMR_Data$MasterCapHist=CMR_Data$MasterCapHist[,c(1:4,end,5:(end-1))] 
	
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
  
} # end of function 'FormatForCMR'

######################################################################################


##########################################################################################
# Function 'FormatForRMark'
# ARGUMENTS
#			'CMRData' -- results from function 'FormatForCMR'- data from the MAPS database, formatted in capture history format
#			'dir' -- project data directory
#     'AddDensity'  -- Boolean flag (T or F) indicating whether to append density results. Default is FALSE
#   RETURNS 'CMR_Data', which is a data frame containing all the information necessary to run a capture-mark-recapture analysis
#############################################################################################	
 
  # Generate Design data for Survival estimation - based on Robust Design model

FormatForRMark <- function(CMRData=CMRData, MAPSData=ProcessedData, dir=DATA_DIRECTORY, AddDensity = FALSE){

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
		maps.process=process.data(file, model="RDHuggins", time.intervals=time.interval, begin.time=startyear, groups='loc') # groups by loc
    maps.ddl=make.design.data(maps.process,parameters=list(S=list(pim.type="all")))
			#Resulting S has index, group(which is loc), cohort, age, time, occ.cohort, cohort, Age, Time, loc
			#same for GammaDoublePrime, GammaPrime
			#p has index, group, time, session, Time, loc, c(all 0s)
			#same for c, but c with all 1s
			
		  ########### first capture year #############
			# identify first year of capture to model transients
			# 'age' in MARK is the year after first capture, not the real age
		  # Resulting S has index, group, cohort, age, time, occ.cohort, cohort, Age, Time, loc, first_cap
		maps.ddl=add.design.data(maps.process,maps.ddl,parameter="S",type="age",bins=c(0,0.5,lastyear-startyear+1),name="first_cap" )

			#make first_cap binary (0 or 1)
		maps.ddl$S$first_cap_bin=numeric(nrow(maps.ddl$S))
		for(i in 1:nrow(maps.ddl$S)){
			maps.ddl$S$first_cap_bin[i] = ifelse(maps.ddl$S$first_cap[i]=="[0,0.5]",1,0)
		} 

			######################ADD DUMMY VARIABLES FOR YEAR (SESSION)
			# add dummy variables for the "p" design data representing the "year" identity of each bout. 
			# resulting p and c has index, group, time, session, Time, loc, c(all 0s), and is1994, is 1995, ... to is2012
			
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
			# resulting p and c has index, group, time, session, Time, loc, c(all 0s), and is1994, is 1995, ... to is2012, and newIntercept

		maps.ddl$p$newIntercept=rep(1,times=nrow(maps.ddl$p))
		maps.ddl$c$newIntercept=rep(1,times=nrow(maps.ddl$c))

		  ##### Add effort to "p" and "c" design data: alternative method for modeling EFFORT
			## This works if the assumption of no movement between locations and stations is met. 
			# resulting p and c has index, group, time, session, Time, loc, c(all 0s), and year1994,..., newIntercept, and effort	
		maps.ddl$p$effort=numeric(nrow(maps.ddl$p))
		maps.ddl$c$effort=numeric(nrow(maps.ddl$c))
		for(i in 1:nrow(maps.ddl$p)){
			index = which((effort$year==maps.ddl$p$session[i])&
							 (as.character(effort$loc)==as.character(maps.ddl$p$loc)[i])&((effort$month-4)==maps.ddl$p$time[i]))
			maps.ddl$p$effort[i] = ifelse(length(index>0),log(effort$effort[index]),log(0.001)) 	
		} 	
		for(i in 1:nrow(maps.ddl$c)){
			index = which((effort$year==maps.ddl$c$session[i])&
							 (as.character(effort$loc)==as.character(maps.ddl$c$loc)[i])&((effort$month-4)==maps.ddl$c$time[i]))
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
	    
		setwd(DATA_DIRECTORY)      # load up the previous RMarkData
    filename<-paste(SPECIES_CODE, "RMarkData.RData", sep="_")    
    load(filename)
    
		maps.ddl = RMarkData$maps.ddl
		maps.process=RMarkData$maps.process

		  # create 'maps_density_frame'
		  # density with both J and A
		maps_density_frame<-p.table[,c("pop","year","maps_rD","maps_prev.rD")]
		# mean(maps_density_frame$maps_rD,na.rm=T)    # should be 1
		  
		  # leave only rows with values for maps_rD - one row for each year
		maps_density_frame2<-maps_density_frame[!is.na(maps_density_frame$maps_rD),] 

		  # determine max(MAPS previous density) above which ceiling-type density-dependence will be used
		  # max(MAPS previous density) = mean of max(maps_prev.rD) for each population
		unique.pop<-sort(unique(maps_density_frame2$pop))
		  # max for each location over time
		max.maps_prev.rD<-data.frame(pop=unique.pop, max.maps_prev.rD=NA)
		for (i in 1:nrow(max.maps_prev.rD)){
		  index<-which(maps_density_frame2$pop==max.maps_prev.rD$pop[i])
		  max.maps_prev.rD$max.maps_prev.rD[i]<-max(maps_density_frame2$maps_prev.rD[index],na.rm=T)
		}

	    # get the mean ( max over time) over all locations 
		mean_max.maps_prev.rD<-mean(max.maps_prev.rD$max.maps_prev.rD)

		  #leave only rows with values for maps_prev.rD - one row for each year
		  # BEGINYEAR removed
		maps_density_frame<-maps_density_frame[!is.na(maps_density_frame$maps_prev.rD),] # removes year 1994 since there is no prev.rD

		  # add real maps previous density for each location and time step into maps.ddl$S
		maps.ddl$S$maps_density=numeric(nrow(maps.ddl$S))
	
		maps_density_frame$year <- as.character(maps_density_frame$year)    # added because factors weren't aligning.
		maps.ddl$S$time <- as.character(maps.ddl$S$time)    # added because factors weren't aligning.
		
		for(i in 1:nrow(maps.ddl$S)){
		  index = which((maps_density_frame$year==maps.ddl$S$time[i])&
						  (as.character(maps_density_frame$pop)==as.character(maps.ddl$S$loc)[i])) 
		  maps.ddl$S$maps_density[i] = ifelse(length(index>0),maps_density_frame$maps_prev.rD[index],0.001)
		}

		## bundle data for return to main workspace
		RMarkData <- list()
		RMarkData$maps.ddl <- maps.ddl
		RMarkData$maps.process <- maps.process    
    RMarkData$max.rD <- mean_max.maps_prev.rD
    
	}
	
	  #[NOTE: could add weather information here!!!]

	return(RMarkData)
  
} # end of function 'FormatForRMark'

###########################################################################

###########################################################################
# Function 'Run.Models'
# SUMMARY: run several sets of models for parameter estimation - Robust design
# ARGUMENTS: 
# RMarkData  --  List object with the following components:
# maps.ddl    --    "design data layer" object. Needed for running MARK from RMark. see RMark documentation for details
# maps.process   --   "process" object (list). Needed for running MARK from RMark. see RMark documentation for details                        
# initial   --  set initial values for S 
# RETURNS:
# result    --       RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object)
# NOTES: Effort is used as a covariate 2 different ways. When the model includes "effort", it is used from the design matrix
#  	if model includes "eff" that refers to effort as a temporarily varying covariate which is more flexible and
#		does not need the assumption of no movement between locations / stations / populations
# 
############################################################################

Run.Models <- function(RMarkData, initial, DensityModel=FALSE) {

  process <- RMarkData$maps.process
  ddl <- RMarkData$maps.ddl
  
  ###################### TIME MODELS ############################
  # contains only Time related models
  # the resulting estimates of capture probabilities for juveniles/adults will later be used to calculated relative density
      
  if (!DensityModel) {
    
    #################### Survival ########################
    # effect of transients included
    # trans - a temporarily varying individual covariate
    # first_cap_bin - first capture year  
    
    # Stage model - estimates apparent survival for juveniles/adults
    S.st.plus.trans=list(formula=~st+first_cap_bin:trans)
    
    # Time model - estimates temporal variability in S for juveniles/adults
    S.st.plus.time.plus.trans=list(formula=~st+time+first_cap_bin:trans) 
    S.st.plus.time.plus.st.time.time.plus.trans=list(formula=~st+time+st:time+first_cap_bin:trans) # with st:time
                  
    #################### Capture probability ########################
    
    p.stage.plus.effort=list(formula=~st+effort,share=TRUE)

    #################### Gamma parameters ########################
    
    # Gamma prime - the probability of being off the study area, unavailable for capture during primary trapping session (i) 
    #      given that the animal was not present on the study area during primary trapping session (i ??? 1), and survives to trapping session (i).
    # Gamma double prime - the probability of being off the study area, unavailable for capture during the primary trapping session (i) 
    #      given that the animal was present during primary trapping session (i ??? 1), and survives to trapping session (i)
    # in a 'no movement' model, ??' is fixed to 1 and ??" is fixed to 0. 
    
    GammaPrime.fixed=list(formula=~1, fixed=1)
    GammaDoublePrime.fixed=list(formula=~1, fixed=0)

    cml=suppressMessages(create.model.list("RDHuggins"))
    results=suppressMessages(mark.wrapper(cml,data=process,ddl=ddl, use.initial=TRUE, silent=TRUE))  # takes a long time to run!   the values from the previous model used as initial values in the later models
  }
  
  ###################### DENSITY MODELS ############################
  # contains 'density' as covariate which is calculated based on capture probabilities from MODEL 1
  
  if (DensityModel) {   
    
    ###################  Survival #############################
    # effect of transients included
    # trans - a temporarily varying individual covariate
    # first_cap_bin - first capture year
    
    # Density model - measures density-dependence in S for juveniles/adults/adult transients
    S.st.plus.maps_density.plus.trans=list(formula=~st+maps_density+first_cap_bin:trans)
    S.st.plus.maps_density.plus.st.times.maps_density.plus.trans=list(formula=~st+maps_density+st:maps_density+first_cap_bin:trans)
    
    #################### Capture probability ########################
    
    p.stage.plus.effort=list(formula=~st+effort,share=TRUE)
    
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

  return(results)
} ### end of function 'Run.Models'


######################################################################################

###################################################################################
# Function 'PCapResults'
# SUMMARY: -estimate capture probabilities from Mark Result
#          -calculate abundances and relative densities based on the capture probabilities
# 
# ARGUMENTS: 
#          RMarkResults    --       RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object)
#          maps.ddl        --       "design data layer" object. Needed for running MARK from RMark. see RMark documentation for details   
#          band.data       --       Data frame object with the following fields:  
#                                   band           -- unique band ID associated with the bird 
#                                   captureyear    -- year of capture 
#                                   month          -- month of capture
#                                   birthyear      -- estimated birth year 
#                                   actualage      -- estimated age at capture
#                                   agegroup       -- ??
#                                   loc            -- MAPS location where bird was captured
#                                   freq           -- ??
#                                   station        -- MAPS banding station where bird was captured. Listed separately for months 5,6,7,8
###################################################################################

PCapResults <- function (RMarkResults=MarkResults, maps.ddl=RMarkData$maps.ddl, band.data=ProcessedData$band.data, model.no=3){
  
  # Extract population, year, month, log(effort) information from maps.ddl
  p.table <- maps.ddl$p[,c("loc","session","time","effort")]     
  colnames(p.table) <- c("pop","year","month","log_effort")
  
  # Calculate capture probability for each month/year at each population 
  # the effort values in maps.ddl$p are already log-transformed and the models were fitted to those 
  # use model S (~st + time + st:time + first_cap_bin:trans), p(~st + effort) - model 1
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
  # p.j: capture probability of juvenile at a given time and location
  # p.a: capture probability of adult at a given time and location
  for (i in 1:nrow(p.table)){
    # Juv: beta(intercept)+beta(st)+beta(effort)*log(effort)
    p.table$p.j[i]<-inv.logit(b.itcp + b.st + b.effort* p.table$log_effort[i])
	p.table$p.j.lcl[i]<-inv.logit(b.itcp.lcl + b.st.lcl + b.effort.lcl* p.table$log_effort[i])
	p.table$p.j.ucl[i]<-inv.logit(b.itcp.ucl + b.st.ucl + b.effort.ucl* p.table$log_effort[i])
    # Adults: beta(intercept)+beta(effort)*log(effort)
    p.table$p.a[i]<-inv.logit(b.itcp + b.effort* p.table$log_effort[i])
    p.table$p.a.lcl[i]<-inv.logit(b.itcp.lcl + b.effort.lcl* p.table$log_effort[i])
	p.table$p.a.ucl[i]<-inv.logit(b.itcp.ucl + b.effort.ucl* p.table$log_effort[i])
    
	
  }
  
  # Calculate capture probability for a given year
  # p.j_year: capture probability of juvenile at a given year and location  
  # p.a_year: capture probability of adult at a given year and location
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
  # processing 'band.data' to avoid double (or triple) counting of individuals that were captured multiple times within a year
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
    index = which((band.data2$captureyear==p.table$year[4*i])&(as.character(band.data2$loc)==as.character(p.table$pop)[4*i])
                  &(band.data2$agegroup=="J"))
    p.table$Njuv[4*i]<-ifelse(length(index>0),length(index),0) 
  }
  for (i in 1:(nrow(p.table)/4)){
    index = which((band.data2$captureyear==p.table$year[4*i])&(as.character(band.data2$loc)==as.character(p.table$pop)[4*i])
                  &(band.data2$agegroup=="A"))
    p.table$Nad[4*i]<-ifelse(length(index>0),length(index),0) 
  }
  for (i in 1:(nrow(p.table)/4)){
    index = which((band.data_A_breed2$captureyear==p.table$year[4*i])&(as.character(band.data_A_breed2$loc)==as.character(p.table$pop)[4*i])
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
  
  # Use MAPS previous year's density (maps_prev.rD) to model effect of density on the vital rates
  # put maps_prev.rD into corresponding year and location
  suppressWarnings(
    for (i in 1:(nrow(p.table)/4)){
      index = which((p.table$year==as.numeric(as.character(p.table$year[4*i]))-1)&(as.character(p.table$pop)==as.character(p.table$pop)[4*i]))
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
  
  # Use MAPS previous year's density (maps_prev.rD_ad) to model effect of density on the vital rates
  # put maps_prev.rD_ad into corresponding year and location
  suppressWarnings(
    for (i in 1:(nrow(p.table)/4)){
      index = which((p.table$year==as.numeric(as.character(p.table$year[4*i]))-1)&(as.character(p.table$pop)==as.character(p.table$pop)[4*i]))
      index = max(index)
      p.table$maps_prev.rD_ad[4*i] = ifelse(length(index>0),p.table$maps_rD_ad[index],NA)   
    } # warnings() are due to the first year (no prev.rD for the beginyear)
  )
  
  return (p.table)
} # end of function 'PCapResults'

######################################################################################


######################################################################################
# Function 'ApparentS'
# SUMMARY: Estimates apparent survival rates for juveniels/adults/transients
# ARGUMENTS:   
#    RMarkResults    --       RMark object with all model results from program MARK (see RMark documentation for details on 'marklist' object)
#                              								
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
  
} # end of function 'ApparentS'

#################################################################################################


##################################################################################
# Function 'VarianceComponent'
# to measue temporal variance in S for juveniels/adults/adult transients
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

} # end of function 'VarianceComponent'

#############################################################################

###################################################################################
# Function 'EstimateFecundity'
# SUMMARY: Run fecundity analysis using WinBUGS 1.4
# ARGUMENTS:
#         p.table   --
#         Corrected   --
#         MinAdults   --
# RETURNS:
# NOTE:
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

env.stoch.sd <- sqrt(1/env.stoch.tau)
log.mean.fec <- log(mean.fec)

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
   
             #  nt=5;nc=2;nb=10000;ni=20000
	    #Send information to WinBUGS
	  Mod_full <- bugs(data=Data_full, inits=inits_full, 
				  parameters.to.save=c("mean.fec", "env.stoch.sd", "est.mean.fec", "beta.rD", "Adultp", "Juvp"), 
				  model.file="fecundity_MAPS_full.bug", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, 
          bugs.directory=BUGSdir, codaPkg=TRUE, over.relax=T, debug=FALSE)   
	  
      # read in results
	  FecResults_full = read.bugs(Mod_full)
      # To test for convergence, use the Gelman and Rubin's convergence diagnostic
      # Approximate convergence is diagnosed when the upper limit is close to 1. 
    GR_full<-gelman.diag(FecResults_full, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
              multivariate=TRUE)
      # print the GR convergence diagnostic in Results.txt
	  
	  # gelman.plot(FecResults_full)
    ToResultsFile(
    sprintf("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY CORRECTED FOR CAPTURE PROBABILITY
    
    Mean fecundity: %s \n
    Slope of DD relationship for fecundity: %s \n
    Temporal env variability (SD) in fecundity: %s \n
    *Approximate convergence is diagnosed when the upper limit is close to 1. 
    " , GR_full$psrf["mean.fec","Upper C.I."], GR_full$psrf["beta.rD","Upper C.I."], GR_full$psrf["env.stoch.sd","Upper C.I."]
    ),  RESULTS_DIRECTORY, RESULTS_FILENAME)

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

env.stoch.sd <- sqrt(1/env.stoch.tau)
log.mean.fec <- log(mean.fec)

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
				  parameters.to.save=c("mean.fec", "env.stoch.sd", "Juvs.pred","beta.rD", "est.mean.fec"), 
				  model.file="fecundity_MAPS_null.bug", n.thin=nt, n.chains=nc, n.burnin=nb,	n.iter=ni, 
          bugs.directory=BUGSdir, codaPkg=TRUE, over.relax=T, debug=FALSE)
	  
	    # Read the results back to R
	  FecResults_null = read.bugs(Mod_null)
	 
    # To test for convergence, use the Gelman and Rubin's convergence diagnostic
    # Approximate convergence is diagnosed when the upper limit is close to 1. 
    GR_null<-gelman.diag(FecResults_null, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
                       multivariate=TRUE)
    # print the GR convergence diagnostic in Results.txt
  ToResultsFile(
    sprintf("
    ----------------------------------------------------------------------------------------
    GELMAN AND RUBIN'S CONVERGENCE DIAGNOSTIC - FECUNDITY NOT CORRECTED FOR CAPTURE PROBABILITY
            
    Mean fecundity: %s \n
    Slope of DD relationship for fecundity: %s \n
    Temporal env variability (SD) in fecundity: %s \n
    *Approximate convergence is diagnosed when the upper limit is close to 1. 
    " , GR_null$psrf["mean.fec","Upper C.I."], GR_null$psrf["beta.rD","Upper C.I."], GR_null$psrf["env.stoch.sd","Upper C.I."]
    ),  RESULTS_DIRECTORY, RESULTS_FILENAME)
  
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
    plot(as.data.frame(FecResults_full[[1]])$env.stoch.sd,type="l",xlab="iterations",ylab="env.stoch.sd")
    if(nc>1)lines(as.data.frame(FecResults_full[[2]])$env.stoch.sd,col="red")
    dev.off()

    filename <- paste(SPECIES_CODE,"BUGStrace_uncorrected.svg", sep="_")
    svg(file=filename,width=10,height=10, onefile=TRUE)
    par(mfrow=c(3,1))
    plot(as.data.frame(FecResults_null[[1]])$mean.fec,type="l",xlab="iterations",ylab="mean.fec")
    if(nc>1) lines(as.data.frame(FecResults_null[[2]])$mean.fec,col="red")
    plot(as.data.frame(FecResults_null[[1]])$beta.rD,type="l",xlab="iterations",ylab="beta.rD")
    if(nc>1)lines(as.data.frame(FecResults_null[[2]])$beta.rD,col="red")
    plot(as.data.frame(FecResults_null[[1]])$env.stoch.sd,type="l",xlab="iterations",ylab="env.stoch.sd")
    if(nc>1)lines(as.data.frame(FecResults_null[[2]])$env.stoch.sd,col="red")
    dev.off()
  

	    # MCMC
	  mean.fec.mcmc_full<-as.mcmc(ModResults_full$mean.fec)
	  beta.rD.mcmc_full<-as.mcmc(ModResults_full$beta.rD)
	  env.stoch.sd.mcmc_full<-as.mcmc(ModResults_full$env.stoch.sd)
    mean.fec.mcmc_null<-as.mcmc(ModResults_null$mean.fec)
    beta.rD.mcmc_null<-as.mcmc(ModResults_null$beta.rD)
    env.stoch.sd.mcmc_null<-as.mcmc(ModResults_null$env.stoch.sd)
  
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
	  res_env.stoch.sd_full<-c(min(ModResults_full$env.stoch.sd), mean(ModResults_full$env.stoch.sd)-1.96*sd(ModResults_full$env.stoch.sd), median(ModResults_full$env.stoch.sd), mean(ModResults_full$env.stoch.sd), mean(ModResults_full$env.stoch.sd)+1.96*sd(ModResults_full$env.stoch.sd), max(ModResults_full$env.stoch.sd))
    FTable_full<-data.frame(res_mean.fec_full, res_beta.rD_full, res_env.stoch.sd_full)
	  row.names(FTable_full)<-c("min.","li(2.5% Qu.)","median","mean","ui(97.5% Qu.)","max.")
	  colnames(FTable_full)<-c("mean_fec","beta.rD","env.stoch.sd")
  
    res_mean.fec_null<-c(min(ModResults_null$mean.fec), mean(ModResults_null$mean.fec)-1.96*sd(ModResults_null$mean.fec), median(ModResults_null$mean.fec), mean(ModResults_null$mean.fec), mean(ModResults_null$mean.fec)+1.96*sd(ModResults_null$mean.fec), max(ModResults_null$mean.fec))
    res_beta.rD_null<-c(min(ModResults_null$beta.rD), mean(ModResults_null$beta.rD)-1.96*sd(ModResults_null$beta.rD), median(ModResults_null$beta.rD), mean(ModResults_null$beta.rD), mean(ModResults_null$beta.rD)+1.96*sd(ModResults_null$beta.rD), max(ModResults_null$beta.rD))
    res_env.stoch.sd_null<-c(min(ModResults_null$env.stoch.sd), mean(ModResults_null$env.stoch.sd)-1.96*sd(ModResults_null$env.stoch.sd), median(ModResults_null$env.stoch.sd), mean(ModResults_null$env.stoch.sd), mean(ModResults_null$env.stoch.sd)+1.96*sd(ModResults_null$env.stoch.sd), max(ModResults_null$env.stoch.sd))
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
      GR_full=GR_full,
      GR_null=GR_null,
      FTable_full=FTable_full,
      FTable_null=FTable_null
      )
  
      return(FecundityResults)
  
  } # end of function 'EstimateFecundity'


######################################################################


######################################################################################
# Function 'SummarizeForPopModel'
# SUMMARY: Print out key results for populatio modeling (stage matrix, temporal variability, density dependence)
# ARGUMENTS:   
#    
#                                							
######################################################################################

SummarizeForPopModel <- function(RMarkData, AppS, STempVar, MarkResults, Fec, model.no){ # writes text file output of all key population-level parameters, and also returns a list containing the same parameters

    # Apparent survival rates for juveniles and adults
  Sjuv <- AppS$Sjuv
  Sad <- AppS$Sad
  
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
  
  S_intcpt<-list()
  S_intcpt$estimate<-S.betas$estimate[1]
  S_intcpt$lcl<-S.betas$lcl[1]
  S_intcpt$ucl<-S.betas$ucl[1]
  
  S_st<-list()
  S_st$estimate<-S.betas$estimate[2]
  S_st$lcl<-S.betas$lcl[2]
  S_st$ucl<-S.betas$ucl[2]
  
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
    Sjuv=Sjuv,
    Sad=Sad,   
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
      Sjuv=Sjuv,
      Sad=Sad,   
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
      paste("1. Apparent survival rate of juveniles:",Sjuv$estimate,"\n"),
      paste("2. Apparent survival rate of adults:",Sad$estimate,"\n"),
      paste("3. Temporal variability in survival of juveniles:",Var_Sjuv$estimate,"\n"),
      paste("4. Temporal variability in survival for adults:",Var_Sad$estimate,"\n"),
      paste("5. Intercept of density-dependence relationship for survival in logit scale:",S_intcpt$estimate,"\n"),
      paste("6. Juvenile effect in the density-dependence function for survival in logit scale:", S_st$estimate,"\n"),
      paste("7. Slope of density-dependence relationship for survival in logit scale:",S_dens$estimate,"\n"),
      paste("8. Mean fecundity:",F_mean$estimate,"\n"),
      paste("9. Temporal variability in fecundity:",SD_F$estimate^2,"\n"),
      paste("10. Slope of density-dependence relationship for fecundity in log scale:",F_beta_rD$estimate,"\n"),
      paste("11. Mean of MAPS density:",MeanDens,"\n"),
      paste("12. SD of MAPS density:",SD_Dens,"\n"),
      paste("13. Maximum MAPS density above which ceiling-type density-dependence will be assumed:",MaxPopDens,"\n")
      ), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }  else {
    ToResultsFile(
      paste("
      ----------------------------------------------------------------------------------------
      PARAMETER VALUES FOR POPULATION MODEL BEFORE CORRECTING FOR APPARENT SURVIVAL \n \n",
            paste("1. Apparent survival rate of juveniles:",Sjuv$estimate,"\n"),
            paste("2. Apparent survival rate of adults:",Sad$estimate,"\n"),
            paste("3. Temporal variability in survival of juveniles: NA \n"),
            paste("4. Temporal variability in survival for adults:",Var_Sad$estimate,"\n"),
            paste("5. Intercept of density-dependence relationship for survival in logit scale:",S_intcpt$estimate,"\n"),
            paste("6. Juvenile effect in the density-dependence function for survival in logit scale:", S_st$estimate,"\n"),
            paste("7. Slope of density-dependence relationship for survival in logit scale:",S_dens$estimate,"\n"),
            paste("8. Mean fecundity:",F_mean$estimate,"\n"),
            paste("9. Temporal variability in fecundity:",SD_F$estimate^2,"\n"),
            paste("10. Slope of density-dependence relationship for fecundity in log scale:",F_beta_rD$estimate,"\n"),
            paste("11. Mean of MAPS density:",MeanDens,"\n"),
            paste("12. SD of MAPS density:",SD_Dens,"\n"),
            paste("13. Maximum MAPS density above which ceiling-type density-dependence will be assumed:",MaxPopDens,"\n")
      ), RESULTS_DIRECTORY, RESULTS_FILENAME)
  }
    
  return(MPparameters)
  
} # end of function 'SummarizeForPopModel'


#########################################################################################################
# Function 'WriteMasterMPFile'
# Correct for apparent survival and temporal variability of juveniles using CV method
#  Build RAMAS ".MP" file on the basis of information from MAPS analysis(and BBS trend analysis)
#  OUTPUT 
#     -- writes MP file to POPMODELS_DIRECTORY
#     -- 'Population Model Summary' text file
#########################################################################################################

WriteMasterMPFile <- function(Data,TrendData){
  
  # READ IN RESULTS FROM MAPS ANALYSIS
  
  Sjuv <- Data$Sjuv
  Sad <- Data$Sad
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
                ")			
  

  
  ###  CALCULATE TRUE SURVIVAL (apparent to actual survival)
  
  ### use the upper and lower bounds of trends
  
  real.lambda <- list()
  real.lambda$estimate <- 1.0 + TrendData$estimate /100.0
  real.lambda$lcl <- 1.0 + TrendData$lcl /100.0
  real.lambda$ucl <- 1.0 + TrendData$ucl /100.0
  
 
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

  ## compute the CV for adult fecundity
  CV_Sad <- list()
  CV_Sad$estimate <- sqrt(Var_Sad$estimate)/Sad$estimate
  CV_Sad$lcl <- sqrt(Var_Sad$lcl)/Sad$estimate   # smallest CV is smallest sd divided by the point estimate for survival
  CV_Sad$ucl <- sqrt(Var_Sad$ucl)/Sad$estimate   # smallest CV is smallest sd divided by the point estimate for survival
  
  ## correct temporal variation in adult survival. Hold CV constant
  corrected.Var_Sad <- list()
  corrected.Var_Sad$estimate <- (corrected.Sad$estimate * CV_Sad$estimate)^2
  corrected.Var_Sad$lcl <- (corrected.Sad$estimate * CV_Sad$lcl)^2
  corrected.Var_Sad$ucl <- (corrected.Sad$estimate * CV_Sad$ucl)^2

  
  ## Sjuv variance cannot be calculated for most species, so it's based on CV of Sad
  
  corrected.Var_Sjuv <- list()  
  if((!is.na(Var_Sjuv$estimate))&(Var_Sjuv$lcl>0)){               # "Var_Sjuv" %in% Data
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
    B. Vital Rates (corrected for apparent survival) and Temporal Variability:
           Mean     LCL    UCL    Standard deviation    LCL    UCL
    Sa      %s      %s     %s            %s             %s     %s
    Sj      %s      %s     %s            %s             %s     %s
    F       %s      %s     %s            %s             %s     %s
    F?Sj    %s      %s     %s            %s             %s     %s
    F?Sa    %s      %s     %s            %s             %s     %s

  Notes:
  The mean values are at average density and average environmental conditions.
  The standard deviations are used to model temporal environmental variability; they exclude variability due to sampling (or demographic stochasticity).
  [if correlation.SF=1]
  The standard deviations for F?Sj and F?Sa assume full correlation between survival and fecundity.
  [if correlation.SF=0]
  The standard deviations for F?Sj and F?Sa assume zero correlation between survival and fecundity.
  [else]
  The standard deviations for F?Sj and F?Sa assume a correlation of [correlation.SF] between survival and fecundity.
    "), corrected.Sad$estimate, corrected.Sad$lcl, corrected.Sad$ucl, sqrt(corrected.Var_Sad$estimate), sqrt(corrected.Var_Sad$lcl), sqrt(corrected.Var_Sad$ucl),
            corrected.Sjuv$estimate, Sjuv$lcl, Sjuv$ucl, sqrt(corrected.Var_Sjuv$estimate), sqrt(corrected.Var_Sjuv$lcl), sqrt(corrected.Var_Sjuv$ucl),
            F_mean$estimate, F_mean$lcl, F_mean$ucl, SD_F$estimate, SD_F$lcl, SD_F$ucl,
            FSj$estimate,FSj$lcl,FSj$ucl,FSj_variance$estimate,FSj_variance$lcl,FSj_variance$ucl,
            FSa$estimate,FSa$lcl,FSa$ucl,FSa_variance$estimate,FSa_variance$lcl,FSa_variance$ucl)      
    
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  
  
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
-----------------------------------------------------------------------             
|  %s (%s - %s)           |   %s (%s - %s)         |
-----------------------------------------------------------------------
|  %s (%s - %s)           |   %s (%s - %s)         |
-----------------------------------------------------------------------  
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
  
 ----------------------------------------------------------------------             
|  %s (%s - %s)           |   %s (%s - %s)         |
-----------------------------------------------------------------------
|  %s (%s - %s)           |   %s (%s - %s)         |
-----------------------------------------------------------------------  
    "), 
			 round(SD.matrix$estimate[1,1],3),round(SD.matrix$lcl[1,1],3),round(SD.matrix$ucl[1,1],3), 
			 round(SD.matrix$estimate[1,2],3),round(SD.matrix$lcl[1,2],3),round(SD.matrix$ucl[1,2],3),
			 round(SD.matrix$estimate[2,1],3),round(SD.matrix$lcl[2,1],3),round(SD.matrix$ucl[2,1],3), 
			 round(SD.matrix$estimate[2,2],3),round(SD.matrix$lcl[2,2],3),round(SD.matrix$ucl[2,2],3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  # output Density-dependence functions to 'Population Model Summary' text file
  ToPopModelFile (
    sprintf(("
  D. Density Dependence:

  When (N/K) > [%s], the current population size is truncated at [%s]*K
  or the stage matrix and stage abundances are decreased such that the expected population size in the next time step is [%s]*K.

             "), round(MaxPopDens,3), round(MaxPopDens,3), round(MaxPopDens,3))
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  
  ToPopModelFile (
    sprintf(("
  [if F_beta_rD<0 then:]
  When N/K < [%s], fecundity is calculated as the following function of density (N/K) at each time step:
  F  = F_mean * exp(F_beta_rD * ( (PopDens - MeanDens) / SD_Dens ) )
  F  = %s * %s   * ( ( (N/K)  - %s  ) / %s ) )
             "), MaxPopDens, F_mean$estimate, F_beta_rD$estimate, MeanDens, SD_Dens )
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  ToPopModelFile (
    sprintf(("
  [if S_Dens<0 then:]
  When N/K < [%s], survival rates are calculated as the following functions of density (N/K) at each time step:
  Sj = exp(S_intcpt + S_st + S_dens*PopDens)/(1 + exp(S_intcpt + S_st + S_dens*PopDens)) 
  Sa = exp(S_intcpt        + S_dens*PopDens)/(1 + exp(S_intcpt        + S_dens*PopDens)) 
             "), MaxPopDens)
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  ToPopModelFile (
    sprintf(("
      
  Sj = exp(%s + %s - %s * (N/K)) / (1 + exp(%s + %s - %s * (N/K)))
  Sa = exp(%s - %s * (N/K)) / (1 + exp(%s - %s * (N/K)))
             "), S_intcpt$estimate, S_st$estimate, S_dens$estimate, S_intcpt, S_st$estimate, S_dens$estimate,
                 S_intcpt$estimate, S_dens$estimate, S_intcpt$estimate, S_dens$estimate)
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  
  ToPopModelFile (
    sprintf(("
  [else if S_Dens>=0 AND F_beta_rD>=0then:]
  Neither survival rate nor fecundity are density dependent below N/K of [%s]
             "), MaxPopDens)
    , dir=RESULTS_DIRECTORY, filename=POPMODELSUMMARY_FILENAME)
  ###  WRITE THE .MP FILE
  
  closeAllConnections()
  
   ############# START HERE
  
  ## CONSTRUCT THE POPULATION LINE FOR THE .MP FILE
  
  ## Population string.  One line of text, constructed as string1 + string2
  ## The following line assumes N=1000 &  K=10000.  The user can change these in RAMAS.
  string1 <- paste("Pop1,0.000,0.000,",INITIAL_ABUNDANCE,",EX,0,",CARRYING_CAPACITY,",0,0,,0,1,0,TRUE,1,1,1,0,1,0,1,0,0,0,1.0,1,1,",sep="")
  
  ##  string2: make a string of comma-separated values out of the 9 varables listed below 
  ## S:(Intercept)    S:st    S:maps_density     F_mean    beta_rD	   Mean_Density   SD_Density    MaxDens
  ## such that it looks like this:
  ## '0.0256,-1.8453,0.0524,1.6960,-0.0828,1.5018,1.1340,3.14'
  string2 <- paste(S_intcpt, S_st, S_dens, F_mean, F_beta_rD, MeanDens, SD_Dens, MaxPopDens, sep=",") 
  
  Population.String <- paste(string1, string2)
  
  MP_initializeFile <- function(){
    setwd(POPMODELS_DIRECTORY)
    MP_line1 <<- paste("Metapopulation input file (",METAPOP_VERSION,") map= ",sep="")
    MP_title <<- paste("Population model for ",SPECIES_CODE,", parameterized from MAPS data",sep="")
    MP_fileName <<- paste(SPECIES_CODE,"test.mp",sep="_")
    MP_dllLocation <<- paste(POPMODELS_DIRECTORY,"\\aviandd.dll",sep="")
    
    ## open a blank file for writing
    MP_fileConnection <<- file(MP_fileName,"w")   
    flush(MP_fileConnection)  # remove existing contents
  }
  
  MP_writeBlock1 <- function(){
    writeLines(MP_line1,MP_fileConnection)
    writeLines(MP_title,MP_fileConnection)
    for(i in 1:4){
      writeLines("",MP_fileConnection)
    }
    writeLines(as.character(REPLICATES),MP_fileConnection)
    writeLines(as.character(TIMESTEPS),MP_fileConnection)
    writeLines("TRUE",MP_fileConnection)
    writeLines("2 FALSE",MP_fileConnection)
    for(i in 1:2){
      writeLines("",MP_fileConnection)
    }
    writeLines("Local",MP_fileConnection)
    writeLines("",MP_fileConnection)
    writeLines("not spread",MP_fileConnection)
    writeLines("0.0000",MP_fileConnection)
    writeLines("0.0000,0.0000,0.0000,0.0000",MP_fileConnection)
    for(i in 1:2){
      writeLines("",MP_fileConnection)
    }
    writeLines("Local",MP_fileConnection)
    writeLines("",MP_fileConnection)
    writeLines("not spread",MP_fileConnection)
    writeLines("0.0000",MP_fileConnection)
    writeLines("0.0000,0.0000,0.0000,0.0000",MP_fileConnection)
    writeLines("False,Zero",MP_fileConnection)
    writeLines("all vital rates",MP_fileConnection)
    writeLines("Lognormal,0",MP_fileConnection)
    writeLines("0.000000",MP_fileConnection)
    writeLines("count in total",MP_fileConnection)
    writeLines("1 (F, S, K correlated)",MP_fileConnection)
    writeLines("No",MP_fileConnection)
    writeLines("AllStages",MP_fileConnection)
    writeLines("No",MP_fileConnection)
    writeLines("UD",MP_fileConnection)
    writeLines(MP_dllLocation,MP_fileConnection)
    writeLines("1",MP_fileConnection)
    writeLines("years",MP_fileConnection)
    writeLines("OnlyFemale",MP_fileConnection)
    writeLines("1",MP_fileConnection)
    writeLines("Monogamous",MP_fileConnection)
    writeLines("2.0",MP_fileConnection)
    writeLines("2.0",MP_fileConnection)
    writeLines("0.0000",MP_fileConnection)
    writeLines("0",MP_fileConnection)
  }
  
  MP_writeBlock2 <- function(){
    writeLines("Migration",MP_fileConnection)
    writeLines("FALSE",MP_fileConnection)
    writeLines("0.000,0.00000,0.00000,0.00000",MP_fileConnection)
    writeLines(" 0,",MP_fileConnection)
    writeLines("Correlation",MP_fileConnection)
    writeLines("FALSE",MP_fileConnection)
    writeLines("0.000,0.00000,0.00000",MP_fileConnection)
    writeLines(" 1,",MP_fileConnection)
    writeLines("1 type(s) of stage matrix",MP_fileConnection)
    writeLines("default",MP_fileConnection)
    writeLines("1.000000",MP_fileConnection)
    writeLines("1.000000",MP_fileConnection)
    writeLines("0",MP_fileConnection)
  }
  
  MP_writeBlock3 <- function(){
    writeLines("Constraints Matrix",MP_fileConnection)
    writeLines("0.000000 0.000000",MP_fileConnection)
    for(i in 1:8){
      writeLines("1.000000 1.000000",MP_fileConnection)
    }
    writeLines("-1 -1",MP_fileConnection)
    writeLines("Juvenile",MP_fileConnection)
    writeLines("1.00000000",MP_fileConnection)
    writeLines("FALSE",MP_fileConnection)
    writeLines("TRUE",MP_fileConnection)
    writeLines("         1",MP_fileConnection)
    writeLines("Adult",MP_fileConnection)
    writeLines("1.00000000",MP_fileConnection)
    writeLines("FALSE",MP_fileConnection)
    writeLines("TRUE",MP_fileConnection)
    writeLines("         1",MP_fileConnection)
    writeLines("0 (pop mgmnt)",MP_fileConnection)
    writeLines("0.0",MP_fileConnection)
    writeLines("0.0",MP_fileConnection)
    writeLines("10",MP_fileConnection)
    writeLines("-End of file-",MP_fileConnection)
  }
  
  MP_construct <- function(){
    MP_initializeFile()
    MP_writeBlock1()
    writeLines(Population.String,MP_fileConnection)
    MP_writeBlock2()
    ## WRITE STAGE MATRIX
    writeLines(paste(stage.matrix[1,],collapse=" "),MP_fileConnection)
    writeLines(paste(stage.matrix[2,],collapse=" "),MP_fileConnection)
    writeLines("1 type(s) of st.dev. matrix",MP_fileConnection)
    writeLines("default",MP_fileConnection)
    ## WRITE SD MATRIX
    writeLines(paste(SD.matrix[1,],collapse=" "),MP_fileConnection)
    writeLines(paste(SD.matrix[2,],collapse=" "),MP_fileConnection)
    MP_writeBlock3()
    close(MP_fileConnection)
  }
  
  MP_construct()
  
} # end of function 'WriteMasterMPFile'
  
###############################################################################
  
