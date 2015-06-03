#################################################################################################################
# BEGIN
#################################################################################################################

CHLOE = F
CHLOE_LAPTOP = T
KEVIN = F
EVA = F
DEBUG = F
POP_ANALYSIS = T
#What mode are we running the program? Development or production.
#If production then we do thing like store intermediary files for debugging, etc. T is production, F is development.
RUN_MODE = T

speciesname="WOTH"
beginyear=1994
endyear=2012

if (EVA) {
	data_dir="//psf/Home/Dropbox/MAPS Project/Data/"
	result_dir=paste("//psf/Home/Dropbox/MAPS Project/results/",speciesname,sep="")
}
if (KEVIN) {
	data_dir="C:\\Users\\Kevin\\Dropbox\\MAPS Project\\Data"
	result_dir=paste("C:\\Users\\Kevin\\Dropbox\\MAPS Project\\results\\",speciesname,sep="")
}
if (CHLOE) {
	data_dir="C:\\Users\\Chloe\\Dropbox\\MAPS Project\\Data"
	result_dir=paste("C:\\Users\\Chloe\\Dropbox\\MAPS Project\\results\\",speciesname,sep="")
}

if (CHLOE_LAPTOP) {
	data_dir="C:\\Users\\haeyeong86\\Dropbox\\MAPS Project\\Data"
	result_dir=paste("C:\\Users\\haeyeong86\\Dropbox\\MAPS Project\\results\\",speciesname,sep="")
}

setwd(data_dir)

	# look at begin/end year for each species and speficy
#beginendyear<-read.csv("begin_endyear.csv", header=T)

MAPSyears=c(beginyear:endyear)
BBSyears=MAPSyears

library(RMark)
library(gtools)
library(foreign)
library(RODBC)
library(doBy)

#################################################################################################################
# Processing input file from Access
#################################################################################################################

# 1) Get Bird banding information from Access
# previously, Function create.banddata 	
# output is 'band.data' - band, captureyear, month, birthyear, actualage, agegroup, loc, freq, station
# for months 5,6,7,8
#Get connection to Access database
	
odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS")    # KTS: I can only run using 32-bit version of R

stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear, month, birthyear, actualage, agegroup, CP, BP, loc, station, 1 as freq ")
				#EK??? stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear,  month, birthyear, actualage, agegroup, station, 1 as freq ")
stmt2=sprintf("FROM finalMAPSBAND")
stmt3=sprintf("WHERE dupeSpec=0 AND spec=\'%s\' AND captureyear >= %s AND captureyear <= %s AND month in (5,6,7,8);", speciesname, beginyear, endyear)
stmt4=sprintf("%s %s %s", stmt1, stmt2, stmt3)
band.data=sqlQuery(con, paste(stmt4), errors=FALSE)
odbcClose(con)
	
if (length(band.data)==1) {
	if (band.data[1]==-1) cat("Function create.banddata failed! \n")
	break
}

#for only one population (loc=SWIF) in WEVI
#Get connection to Access database
	
#odbcDataSources()
#if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
#if(any(c(KEVIN))) con = odbcConnect("MAPS")    # KTS: I can only run using 32-bit version of R

#stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear,  month, birthyear, actualage, agegroup, loc, 1 as freq, station ")
				#EK??? stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear,  month, birthyear, actualage, agegroup, station, 1 as freq ")
#stmt2=sprintf("FROM finalMAPSBAND")
#stmt3=sprintf("WHERE dupeSpec=0 AND spec=\'%s\' AND loc='SWIF' AND captureyear >= %s;", speciesname, beginyear)
#stmt4=sprintf("%s %s %s", stmt1, stmt2, stmt3)
#band.data=sqlQuery(con, paste(stmt4), errors=FALSE)
#odbcClose(con)
	
#if (length(band.data)==1) {
#	if (band.data[1]==-1) cat("Function create.banddata failed! \n")
#	break
#}

#############################################################
# 1') Get Bird banding information from Access for all months 
# this is for transients
# previously, Function create.banddata   
# output is 'band.data' - band, captureyear, month, birthyear, actualage, agegroup, loc, freq, station

#Get connection to Access database

odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS")    # KTS: I can only run using 32-bit version of R

stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear, month, birthyear, actualage, agegroup, CP, BP, loc, station, 1 as freq ")
#EK??? stmt1=sprintf("SELECT DISTINCT finalMAPSBAND.band, captureyear,  month, birthyear, actualage, agegroup, station, 1 as freq ")
stmt2=sprintf("FROM finalMAPSBAND")
stmt3=sprintf("WHERE dupeSpec=0 AND spec=\'%s\' AND captureyear >= %s AND captureyear <= %s;", speciesname, beginyear, endyear)
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

#Get connection to Access database
odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS")    # KTS: I can only run using 32-bit version of R

stmt11 = sprintf('SELECT loc, year(date) as year, month(date) as month, round(sum(cint(length)*((Cint(mid(end,1,2))*60+Cint(mid(end,3))*10) - (Cint(mid(start,1,2))*60+Cint(mid(start,3))*10) ))/1440,2) as effort')
stmt22 = sprintf('FROM mapsef GROUP BY loc, year(date), month(date)')
stmt33 = sprintf('%s %s', stmt11, stmt22)
effort=sqlQuery(con, paste(stmt33), errors=FALSE)
odbcClose(con)

if (length(effort)==1) {
	if (effort[1]==-1) cat("Function create.effort failed! \n")
	break
}	

############################################################

# 3) Get bird's station data from Access
# previously, Function get.stationdata - get bird  information from Access
# output is 'sim_stations' - station, loc
# distinct stations (and its corresponding loc) for species in our list
#Get connection to Access database
odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS") 
	
stmt111 = sprintf("SELECT distinct station, loc")
stmt222 = sprintf("FROM finalmapsband")
stmt333 = sprintf("where spec in ('GRCA','WOTH','WEVI','KEWA','YBCH','HOWA','YEWA','YTWA','COYE','BCCH','BHNU','CACH','NOCA')")
stmt444 = sprintf("%s %s %s", stmt111, stmt222, stmt333)
sim_stations=sqlQuery(con, paste(stmt444), errors=FALSE)
odbcClose(con)
	
if (length(sim_stations)==1) {
	if (sim_stations[1]==-1) cat("Function get.stationdata failed! \n")
	break
}

############################################################
# SKIP - BBS density is not used anymore

# 4) Get abundance from Brooke by station
# from year 1990 to 2012
# previously, Function get.abundance 
# output is 'abundance_map' - station, year, abundance

#Get connection to Access database
odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS") 

stmt1111 = sprintf("SELECT station, year, PredAbund as abundance")
stmt2222 = sprintf("FROM all%s", speciesname)
stmt3333 = sprintf("WHERE year>=1990")
stmt4444 = sprintf("%s %s %s", stmt1111, stmt2222, stmt3333)
abundance_map=sqlQuery(con, paste(stmt4444), errors=FALSE)
odbcClose(con)
	
if (length(abundance_map)==1) {
	if (abundance_map[1]==-1) cat("Function get.abundance failed! \n")
	break
}

###############################################################

# 5) Get weather data from Brooke by station
# from year 1990 to 2011
# output is 'weather_map' 

#Get connection to Access database
odbcDataSources()
if(any(c(EVA,CHLOE,CHLOE_LAPTOP))) con = odbcConnect("maps") 
if(any(c(KEVIN))) con = odbcConnect("MAPS") 

stmt11111 = sprintf("SELECT station, Month, Year, Suitability")
stmt22222 = sprintf("FROM weather_allspecies")
stmt33333 = sprintf("WHERE Year>=1990 AND species=\'%s\'", speciesname)
stmt44444 = sprintf("%s %s %s", stmt11111, stmt22222, stmt33333)
weather_map=sqlQuery(con, paste(stmt44444), errors=FALSE)
odbcClose(con)

if (length(weather_map)==1) {
  if (weather_map[1]==-1) cat("Function get.weather failed! \n")
  break
}

###############################################################
# 6) get population info for each station

# In case we want to read data from file
		#filename = sprintf('%s/Abund/all%s.csv', getwd(), speciesname)  
		#densities = read.csv(filename,header=T)
		#colnames(densities)=c("year","station","lat","long","abundance")

# Read in data for population analysis. User defines which stations belong to which populations.
# output 'population_map' - station, population
### USER DEFINES FOR EACH SPECIES, for now: loc = population
# get rid of '1660' in loc "CONG" since this only exist once for band 99999 in NOCA and complicates density values for other species      

  # 1) if population grouped by location
population_map = read.csv("loc_pop.csv",header=T) 

  # 2) if everything is considered as one population
#population_map = read.csv("station_pop_one.csv",header=T)

  # 3) if groupings at the intermediate scale (by bird conservation regions)
#population_map = read.csv("station_pop_int.csv",header=T)

##############################################################
  # make 'station.loc.pop' - station/loc/pop matrix
  # match population in population_map with loc in sim_stations
  # 'station.loc.pop' - station, loc, populations
station.loc.pop<-cbind(sim_stations, NA)
colnames(station.loc.pop)<-c("station","loc","pop")
for (i in 1:nrow(station.loc.pop)) {
  index<-which(as.character(population_map$loc)==as.character(station.loc.pop$loc)[i])
  station.loc.pop$pop[i]<-population_map$pop[index]
}


###############################################################
# 7) Create inputfile for RMARK
	# [previously, Function create.inputFile] 

data=band.data
#abund_map=abundance_map
#pop_map=population_map


if (DEBUG) browser()

	# Structuring input file for processing
	# 'work2' - band, year_month, loc, birthyear, agegroup, actualage, freq, station, CP, BP, captureyear, month
work=data[,c("band","captureyear","month","loc","birthyear","agegroup","actualage","freq","station","CP","BP")]
work2=cbind(work[c(1)], year_month=paste(work[,c(2)],work[,c(3)],sep="_"),  work[c(4:11,2,3)])

# Making fake birds to create uniform secondary occasions for each primary occasion (year)
	# previously, function 'make.fakebirds'
  # make sure to have all combinations of year-month
	# make sure at least one individual is in the dataset for each year-month period
	# if missing, add a "fake bird" with ID "99999"

  ###############################
  ## ATTENTION, for BCCH and YEWA

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

nrow(work2[which(work2$band==99999),])
if (DEBUG) tail(work2$band)

	# 'work3' - band, year_month, loc, birthyear, agegroup(A/J), actualage(from -1 to 7;0=juveniles,all other=adults), freq, station, CP, BP, captureyear, month
	# work3 puts the bands in order by year_month 
work3=work2[order(work2$year_month), ]	
	
	# 'work_reshape' - band, year_month, loc, birthyear, agegroup, actualage, freq, CP, BP
work_reshape=work3[,c("band", "year_month", "loc", "birthyear", "agegroup", "actualage", "freq", "CP", "BP")]			
	# 'work_density' - band, captureyear, month, station
work_density=work3[,c("band","captureyear","month","station")]		
if (DEBUG) {
  	colnames(work3)
  	nrow(work3)
}


      #### make sure only a single obs for each unique individual and time period
      # leave only one capture for each year-month for each individual
nrow(work_reshape)  #compare with later

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
nrow(work_reshape)

  #check which individuals were caught more than once in each year_month time period
temp <-apply(flag,1,function(t) ifelse(length(which(t>1))>0,1,0))  
ndx <- which(temp==1) 
flags <- indivs[ndx] ##individuals that was caught more than once in each year_month -> if everything is fine, should be 0
flags # these were removed from work_reshape in the previous loop
length(flags)

      #### end debug

	# "explode" the dataset so that each survey event gets its own column 
	# 'test'- band, loc, birthyear, freq, agegroup1994_5, actualage1994_5, CP1994_5, BP1994_5, agegroup1994_6, actualage1994_6, etc..

test=reshape(work_reshape, timevar='year_month', direction='wide', idvar='band', v.names=c('agegroup','actualage','CP','BP'), sep='')  # 'effort'

if (DEBUG) {
  write.csv(work3$band,'//psf/Home/Dropbox/MAPS Project/code/work3 band.csv')
}

if (DEBUG) {
	head(test)
	nrow(test)
}

	# determine the columns that will make up the capture history
	# which are columns with 'agegroup' 
indx=grep("agegroup",x=names(test)) 	#selects columns that contain 'agegroup' in them

	#Take out the year and month for every agegroup for generating primary and secondary trapping occasions for robust design
rdyear=as.numeric(substr(names(test[,indx]),9,12))	#Take out year (9th to 12th position) from ex) agegroup1994_5
rdmonth=as.numeric(substr(names(test[,indx]),14,14))	#Take out month (14th position) from ex) agegroup1994_5
rdn_year=length(unique(rdyear))
rdn_secondaryoccasion=as.numeric(table(rdyear))		#number of each year appearing in agegroup1994_5, etc.. (for each year 4 because May, June, July, August)

	# make a preliminary capture history (not collapsed)
	# 'ch2' - agegroup1994_5, agegroupd1994_5, ..., agegroup2012_8
ch2=apply(test[,indx],c(1,2),function(t) ifelse(is.na(t),0,1))	#for columns in test with agegoup in them, replace NA with 0
nrow(ch2)
if (DEBUG) nrow(ch2)



		#Create effort matrix to the same structure as capture history
		# previously, function reshape.effort
		#from the previous brought-in effort matrix (loc, year, month, effort), navigate for each individual where it was located for specific year_month and put that effort value
		#in the form of a new matrix with individual as rows and year_month as columns 

occasion_names=work3$year_month
ch_data=test
debugflag=DEBUG

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
 	#if (debugflag)
#cat(head(yearmat))
monmat <- matrix(rep(realmonth,times=nind),nrow=nind,ncol=nbouts,byrow=T)
locmat <- matrix(rep(realloc,times=nbouts),nrow=nind,ncol=nbouts,byrow=F)
if (debugflag){
	head(locmat)
	nrow(locmat)
}

      # generate a new effort matrix
	# 'effortmatrix' - band, year_month (nrow=number of bands, ncol=number of year_month)
effortmatrix <- matrix(0,nrow=nind,ncol=nbouts)   
for(i in 1:nind){
  for(j in 1:nbouts){
    ndx <- which((effort$loc==locmat[i,j])&(effort$year==yearmat[i,j])&(effort$month==monmat[i,j]))
    effortmatrix[i,j] <- ifelse(length(ndx)>0, effort$effort[ndx],NA)
  }
}

effortmat<-effortmatrix
if (DEBUG) nrow(effortmat)


	# look for errors: "1" in ch and no effort 
  # where captured (1 in ch), effort values should be present
errors=numeric(nind)
for(i in 1:nind){
	errors[i] <- length(which(is.na(effortmat[i,])&(ch2[i,]==1))) #find out how many cells in effortmat is NA, when there is 1 in ch2 for each individual
}
unique(errors)	# should be 0. If not, ch of 1 will be replaced to NA (see below).

  #if (DEBUG)
if(sum(errors) > 0) {
	cat("Effort is 0 but a bird was captured in nind! \n")
	break
} # could be due to fake individual

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
nrow(ch3)
ndx<-which(apply(ch2,1,function(t) sum(t,na.rm=TRUE))==0)
ndx
  # if any ndx, then,
ch3 <- ch2[-ndx,]
nrow(ch3)
  # check
unique(c(ch2[2006,]))

  # remove individuals in 'effortmat' as well
nrow(effortmat)
effortmat <- effortmat[-ndx,]
nrow(effortmat)

  # remove individuals in 'test' as well
test2<-test
nrow(test2)
test2 <- test[-ndx,]
nrow(test2)

	# make final capture history matrix (with dots for no data)
	# ch2 consists of ind as rows and agegroupyear_months as colums with values of 0,1,NA
	# put those values in a sequences while converting NA to "."
	# 'ch' - real capture history with 0,1, and '.' for all individuals
ch=apply(ch3,1,function(t) paste(ifelse(is.na(t),".",t),collapse=""))
length(ch)
if (DEBUG) {
	head(ch)
	tail(ch)
	length(ch)
}

  

	# generate input data frames for RMARK

	############# eff #######################	
	# this setup allows to use effort as a temporarily varying individual covariate

effortcolnames=paste("eff", realyear,rep(c(1,2,3,4),times=nyears),sep="")          
			# realyear,rep(c(1,2,3,4),times=nyears),sep="")
			#seq(1,nbouts,1),sep="")     
			# realyear, # rep(c(1,2,3,4),times=nyears),
                  # log transform the effort for use in the MARK model...
			#browser()

	#effortmat is in a format of ind as rows, year_month as colums with effort and NA for no data
	#effortdf takes natural logarithms of the values from effortmat (for NA, take log(0.001))
	# WHY??????????????????????????
	# 'effortdf' - ln(effortmat) with nrow=nind, ncol=number of year_month

effortdf=as.data.frame(apply(effortmat,c(1,2),function (t) ifelse(is.na(t), log(0.001), log(t))))
names(effortdf)=effortcolnames
nrow(effortdf)

if (DEBUG) {
  nrow(effortdf)
  head(effortdf)
}
	########## st ############################
	###### agegroup is either A or J #########
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
head(stratadf)
nrow(stratadf)

  ########## trans ############################
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
}   # if warnings(), this is due to all NA resulting in -Inf for their maximum
unique(c(CP_df2))
unique(c(BP_df2))
nrow(CP_df2)
nrow(BP_df2)

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
unique(c(trans))
nrow(trans)

  # among individuals that trans=1, leave out the individuals that were caught more than once within the same year

  # first create matrix which indicate whether the individual was recaptured again within the same year
  # for the same individuals in 'test'
  # 0 if caught more than once within the same year or when caught none
  # 1 if only caught once within the same year
recap_within_sameyear<-matrix(0,nrow=nrow(test2),ncol=nyears) 
colnames(recap_within_sameyear)<-c(beginyear:endyear)
years<-c(beginyear:endyear)
indvs<-test2$band

for (i in 1:length(indvs)){
  for (j in 1:nyears){
    index<-which((band.data_allmon$band==indvs[i])&(band.data_allmon$captureyear==years[j]))
    recap_within_sameyear[i,j]<-ifelse(length(index)==1,1,0)
  }
}
unique(c(recap_within_sameyear))
nrow(recap_within_sameyear)

  # final 'pot_trans' 
  # for each individual, whether potential transient in a given year
pot_trans<-matrix(NA,nrow=nrow(test2),ncol=nyears) 
for (i in 1:nrow(pot_trans)){
  for (j in 1:ncol(pot_trans)){
    # if trans=1 and recap_within_sameyear=1, then it is a potential transient
    pot_trans[i,j]<-trans[i,j]*recap_within_sameyear[i,j]   # potential transient=1
  }
}
unique(c(pot_trans))
nrow(pot_trans)

pot_transcolnames=paste("trans", unique(yearlab),sep="")
colnames(pot_trans)=pot_transcolnames
head(pot_trans)
if (DEBUG) {
  head(pot_trans)
}

  ################ SKIP ####################
  ################### BBS density ######################################
  ############### change to MAPS density ! #############################
	# Create density matrix, rather temporarily varying individual covariates for density
		# previoulsy, function 'create.popsANDdensities' - Density by year and population to the same structure as capture history
		# density changes by year (not by suboccassion/months) and is pooled by population (not by station)
		# stations in the same population have same abundance data for all years
		# density or abundance data is generated by Brooke who extrapolated it from BBS data
		# some values are NA as some BBS routes are not surveyed every year
		# In addition: create population matrix for temp varying individual population covariate to allow individuals
		#    to move between populations between years

cat("BEFORE_rD\n\n")
	#work_density: a matrix of band, captureyear, month, station
	#abundance_map: a matrix of station, year, abundance
	#pop_map: a matrix of station, population
	
bands=work_density
abund_map=abundance_map
pop_map=population_map

	#browser()

debugflag=FALSE
if (debugflag) write.csv(pop_map,'C:\\Users\\vica\\Documents\\Stony Brook\\RA\\Debug\\pop_map loc.csv')

actualbands=unique(bands$band)
nband=length(unique(bands$band))
actualyears=unique(bands$captureyear)
minyear=min(unique(bands$captureyear))
	#from abundance data
#nyears=length(unique(abund_map$year))
	#from CMR data
nyears2=length(unique(bands$captureyear))
	#dd_df: temp (all 0s for now), band, year, pop (all NAs for now)
dd_df=data.frame(temp=rep(0,times=nband*nyears2), band=rep(unique(bands$band),each=nyears2),year=rep(unique(bands$captureyear),times=nband), pop=NA)

	# Create a df that associates each bird with a population based on station
	# bands: band, captureyear, month, station
	# pop_map: station, population
	# band_map: station, band, captureyear, month, population
band_map=merge(bands,pop_map,by="station")
	# order by captureyear and month
band_map=band_map[with(band_map,order(captureyear,month)),]
	# rename column 'captureyear' as 'year'
	# 'band_map' - station, band, year, month, population
colnames(band_map)=c("station","band","year","month","population")

	#Calculate Carrying capacity and relative density for each population and year combination
	# abund_map: station, year, abundance
	# pop_map: station, population
	# mergedMap: station, year, abundance, population
mergedMap=merge(abund_map, pop_map, by.x="station")
	# order by station and year
orderedMap=mergedMap[with(mergedMap,order(station,year)),]
	# 'orderedMap' - population, station, year, abundance
orderedMap=orderedMap[,c(4,1:3)]

	# if grouped by population
  # Get Carrying capacity for each population

	# band_map (band, year, population) -> density_frame (population, year, absDens, K, rD) => for each individual for each year, K, absDens, rD calculated
	# some populations have multiple stations, but those stations seems to have same abundance. probably, because abundance data based on location (by Brook)
	# K = mean(abundance of stations in one population) over time

EK_K_capacity=summaryBy(abundance~population,orderedMap, FUN=function(t) mean(t))
	
	# Get average density (absDensity) per population per year (if multiple stations, it will average the stations' abund for that year and loc) 
	# absDens = mean(abundance of stations in one population) at each time step

EK_absDens = summaryBy(abundance~population+year,orderedMap,FUN=function(t)mean(t))
temp=merge(EK_absDens, EK_K_capacity ,by="population")
colnames(temp)=c("population","year", "absDens", "K")
	
	# rD (relative density) = absDens/K
  # which is, average abudance of stations in each population for each time step / average abundance of stations in each population
  # this ratio is equivalent to the sum(abundance of stations in each population for each time step) / sum(abundance of stations in one population)
	# 'density_frame'= population, year, absDens, K, rD
	
density_frame=data.frame(temp, rD=0)
density_frame$rD=density_frame$absDens / density_frame$K
  # add loc in density_frame
for (i in 1:nrow(density_frame)){
  index<-which(station.loc.pop$pop==density_frame$population[i])
  density_frame$loc[i]<-unique(station.loc.pop$loc[index])
}
colnames(density_frame)=c("population","year","absDens","K", "rD", "loc")

if (debugflag) write.csv(density_frame,'C:\\Users\\vica\\Documents\\Stony Brook\\RA\\Debug\\density_frame loc.csv')
	
	# 'dd_merged' - year, population, station, band, month, absDens, K, rD
dd_merged=merge(band_map,density_frame,by=c("year","population"))
	
	# 'dd' - band, year, rD
dd=dd_merged[,c(4,1,8)]
colnames(dd)=c("band","year","rD")
udd=unique(dd)

	#DEBUG code , original value 1993
	#udd[16,2]=1993
	
	####fill in the pop column in dd_df matrix
	# 'dd_df' - temp, band, year, population
counter=1
for (i in 1:nband){
  pop=NA
  for (j in 1:nyears2){
    ndx <- which( (band_map$band==dd_df$band[counter]) & (band_map$year==dd_df$year[counter]) )  
    if (length(ndx) != 0) pop=band_map$population[ndx[1]]      # change pop only if it is known to change, otherwise assume it's still in the same population
    dd_df$pop[counter]= pop
    counter=counter+1
  }	
}
	
colnames(dd_df)=c("temp","band","year","population")		#in dd_df, there are individuals with NA for population
	# 'merged_df' - population, year, temp, band, absDens, K, rD
merged_df=merge(dd_df,density_frame, by=c("population","year"), all.x=TRUE )  
	# 'merged_df' - population, year, band, rD
merged_df=merged_df[,c("population","year","band","rD")]

	# 'create temporaly varying variable 'relative density (rD)' by year
	# 'dd_reshaped' - band, rd1997,... (rd1996,1995,1994 shows up at the end for NOCA)
dd_reshaped=reshape(merged_df[,c(2,3,4)], timevar='year', direction='wide', idvar='band', v.names=c('rD'), sep='')  

	# 'create temporaly varying variable 'population' by year
	# 'pop_reshaped' - band, population1997,... (population1996,1995,1994 shows up at the end for NOCA)
pop_reshaped=reshape(merged_df[,c(1,2,3)], timevar='year', direction='wide', idvar='band', v.names=c('population'), sep='')  


	#OLD EK code
	#	for (i in 1:nrow(dd_df)){
          	#dupes=nrow(udd[which(udd$band==dd_df$band[i] & udd$year==dd_df$year[i]),])
	    	#if (dupes > 1 )
		#	cat ("Dupes found for band: and year: dupes: ", dd_df$band[i],dd_df$year[i], dupes , "\n")
	#		ndx <- which(udd$band==dd_df$band[i] & udd$year==dd_df$year[i])
	#		if (length(ndx) != 0)
	#	          dd_df$rD[i]=udd$rD[min(ndx)]     
	#	}

results=list(dd_rs=dd_reshaped, pop_rs=pop_reshaped)
	
cat("AFTER_rD\n\n")
	
result_dd=results$dd_rs
result_pop=results$pop_rs

	# Create abundance matrix
	# 'abundancedf2' - for each band, rD1994, ...
matched_ddresult=match(test$band,result_dd$band)
abundancedf=result_dd[matched_ddresult,] 		
col_ndx=grep("rD",names(abundancedf))
abundancedf=abundancedf[,col_ndx]
abundancedf2=apply(abundancedf,c(1,2),function(t) ifelse(is.na(t),0,t))		#change NA to 0.

	# Create population matrix
	# 'popdf2' - for each band, population1994, ...
matched_popresult=match(test$band,result_pop$band)
popdf=result_pop[matched_popresult,]		
col_ndx=grep("population",names(popdf))
popdf=popdf[,col_ndx]
popdf2=apply(popdf,c(1,2),function(t) ifelse(is.na(t),0,t)) # if pop=NA, put 0


  ##################### inputFile #####################################
	#	inputFile=data.frame(band=test[c(1)],ch=ch,freq=test[c(4)],loc=test[c(2)], stratadf, effortdf)
	# 'inputFile' - band, ch, freq, loc, st1994, ..., eff19941,..., rd1994, ..., population1994,..
#inputFile=data.frame(band=test[c(1)],ch=ch,freq=test[c(4)],loc=test[c(2)], stratadf, effortdf, trans, abundancedf2, popdf2)
# only st and effort and potential trasient as temporally varying individual covariate
inputFile=data.frame(band=test2[c(1)],ch=ch,freq=test2[c(4)],loc=test2[c(2)], stratadf, effortdf, pot_trans)
inputFile$ch=as.character(inputFile$ch)

head(inputFile)
nrow(inputFile)
if (DEBUG) {
	nrow(inputFile)
	head(effort)
	head(inputFile)
}

  # add population into inputFile
inputFile[, "pop"] <- NA
for (i in 1:nrow(inputFile)){
  index<-which(station.loc.pop$loc==as.character(inputFile$loc[i]))
  inputFile$pop[i]<-ifelse(length(index)>0, station.loc.pop$pop[index], NA)
}
inputFile$pop=as.factor(inputFile$pop)   # change pop as factor
# move pop after loc
end<-ncol(inputFile)
inputFile=inputFile[,c(1:4,end,5:(end-1))] 
head(inputFile) #check if placed right

  # remove "fake bird" :)
nrow(inputFile[which(inputFile$band==99999),])
if (nrow(inputFile[which(inputFile$band==99999),])!= 0 ) inputFile=inputFile[-which(inputFile$band==99999),]

if (DEBUG) {
	head(inputFile)
	nrow(inputFile)
}

	# make fake covariate for tricking RMark
inputFile$random=as.numeric(paste(seq(1,nrow(inputFile),1),rep(9999,times=nrow(inputFile)),sep=""))  #rnorm(nrow(inputFile),5,1)

if (DEBUG) {
	nrow(inputFile)
	head(inputFile)
}

if (DEBUG ) write.csv(inputFile, 'routput.csv')

result=list(first=inputFile, second=rdn_year, third=realyear, fourth=realbout)
	
inputFile=result$first
rdn_year=result$second
realyear=result$third
realbout=result$fourth

 #pop-up window when done
system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE) #pop-up notification when done

#######################################################################################################
# Change working directory for output
#######################################################################################################

setwd(result_dir)
  # 1) if population is grouped by location
write.csv(inputFile, sprintf("%s_rinput.csv", speciesname))
write.csv(inputFile, sprintf("%s_rinput_trans.csv", speciesname))
  # 2) if everything is considered as one population
#write.csv(inputFile, sprintf("%s_rinput_one.csv", speciesname))
  # 3) if groupings at intermediate scale by conservation region
#write.csv(inputFile, sprintf("%s_rinput_int.csv", speciesname))
	# for one population
#write.csv(inputFile, sprintf("%s_SWIF_rinput.csv", speciesname))

  # save 'result' as .RData
filename<-paste(speciesname, "result", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "result_trans", Sys.Date(), "RData", sep=".")
setwd(result_dir)
#save(result, band.data, effort, sim_stations_rev,sim_stations, station.loc.pop, abundance_map, population_map, effortmatrix, density_frame, file=filename)
save(result, band.data, band.data_allmon, effort, weather_map, sim_stations, station.loc.pop, population_map, work2, work3, work_reshape, work_density, 
     test, test2, ch, ch2, ch3, effortmatrix, effortmat, effortdf, stratadf, trans, recap_within_sameyear, pot_trans, file=filename)

#with weather_map
save(result, band.data, effort, sim_stations, sim_stations_rev, station.loc.pop, abundance_map, weather_map, population_map, effortmatrix, density_frame, file=filename)

#######################################################################################################
# Generate Design data for Survival estimation - based on Robust Design model
#######################################################################################################
# when bringing in 'result.RData'
setwd(result_dir)
load("NOCA.result.2014-11-26.RData")
load("COYE.result_trans.2014-12-10.RData")

inputFile=result$first
rdn_year=result$second
realyear=result$third
realbout=result$fourth


# previoulsy, function 'create.ddl'

file=inputFile
startyear=beginyear
lastyear=endyear

	#browser()

time.interval=rep(c(0,0,0,1), times=rdn_year)[-(4*rdn_year)]  	#last 1 omitted
  # groups by location (this is the same as grouping by population when pop=loc)
  # later when weather is incorporated, this will be more useful
maps.process=process.data(file, model="RDHuggins", time.intervals=time.interval, begin.time=startyear, groups='loc') # groups by loc
#maps.process=process.data(file, model="RDHuggins", time.intervals=time.interval, begin.time=startyear, groups='pop') # groups by population
      #maps.ddl=make.design.data(maps.process, parameters=list(S=list(pim.type="time"), p=list(pim.type="time"),c=list(pim.type="time") GammaPrime=list(pim.type="constant"), GammaDoublePrime=list(pim.type="constant")))
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

if (DEBUG) nrow(maps.process$data)

if (DEBUG){ 
  names(maps.ddl)
  maps.ddl$S
  maps.ddl$p
  maps.ddl$c
}

	######################ADD DUMMY VARIABLES FOR YEAR (SESSION)
      # add dummy variables for the "p" design data representing the "year" identity of each bout. 
	#resulting p and c has index, group, time, session, Time, loc, c(all 0s), and is1994, is 1995, ... to is2012
	
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
	#resulting p and c has index, group, time, session, Time, loc, c(all 0s), and is1994, is 1995, ... to is2012, and newIntercept

maps.ddl$p$newIntercept=rep(1,times=nrow(maps.ddl$p))
maps.ddl$c$newIntercept=rep(1,times=nrow(maps.ddl$c))

  ##### Add effort to "p" and "c" design data: alternative method for modeling EFFORT
	## This works if the assumption of no movement between locations and stations is met. 
	#resulting p and c has index, group, time, session, Time, loc, c(all 0s), and year1994,..., newIntercept, and effort	
maps.ddl$p$effort=numeric(nrow(maps.ddl$p))
maps.ddl$c$effort=numeric(nrow(maps.ddl$c))
for(i in 1:nrow(maps.ddl$p)){
	index = which((effort$year==maps.ddl$p$session[i])&
                     (as.character(effort$loc)==as.character(maps.ddl$p$loc)[i])&((effort$month-4)==maps.ddl$p$time[i]))
	maps.ddl$p$effort[i] = ifelse(length(index>0),log(effort$effort[index]),log(0.001)) 	# why log(effort)?????
} 	
for(i in 1:nrow(maps.ddl$c)){
	index = which((effort$year==maps.ddl$c$session[i])&
                     (as.character(effort$loc)==as.character(maps.ddl$c$loc)[i])&((effort$month-4)==maps.ddl$c$time[i]))
	maps.ddl$c$effort[i] = ifelse(length(index>0),log(effort$effort[index]),log(0.001))		# why log(effort)?????
} 

	

  ################ SKIP IF REAL WEATHER SUITABILITY DATA IS USED #####################
  # make fake weather suitability data  
  # simulate fake weather suitability data before Brooke's actual data
loc<-as.character(unique(sim_stations$loc))
nloc<-length(loc)
nyears<-length(BBSyears)

# a table of random numbers for each location at each time step (year)
f.weather <- array(0,dim=c(nloc,nyears))
for(s in 1:nloc){
  for(t in 1:nyears){
    f.weather[s,t] = runif(1,0,1)
  }
}
rownames(f.weather)<-loc
colnames(f.weather)<-BBSyears

# reshape f.weather with specific loc&time for each row

w_df<-data.frame(f.weather,rownames(f.weather))
colnames(w_df)<-c(as.character(beginyear:endyear),"loc")
ncol.w.df<-ncol(w_df) #make it vary by "loc"
f.weather_rshp<-reshape(w_df, varying=names(w_df)[-ncol.w.df], v.name="weather", idvar=c("loc"), timevar="Time", direction="long")
  #put back real years instead of 1,2,3.. for Time
for (i in 1:length(f.weather_rshp$Time)){
  f.weather_rshp$Time[i]<-f.weather_rshp$Time[i]+beginyear-1
}


	################ SKIP IF REAL DENSITY DATA IS USED #####################
  # make fake densities
	# simulate fake densities before Brooke's actual data
	# previously, function 'sim.abundance' - Generate density data randomly to test the code before using real abundance data from Brooke.

data=sim_stations
years=BBSyears

uniqueid <- paste(data$loc,data$station,sep="_")
nstations <- nrow(data)
nyears=length(BBSyears)
abund <- data
 	#names <- paste("y",c(1:nyears),sep="")
  	#names <- paste("y",BBSyears,sep="")
abund2 <- array(0,dim=c(nstations,nyears))
for(s in 1:nstations){
  for(t in 1:nyears){
    abund2[s,t] = runif(1,50,200)
  }
}
temp <- cbind(abund,uniqueid,as.matrix(abund2))
colnames(temp)=c("loc","station","uniqueid",as.character(BBSyears))

	# 'densities' - loc, station, uniqueid (loc_station), beginyear...endyear (filled with randomly generated values)
densities=temp

	# 'long.densities' - loc, station, uniqueid(loc_station), time, density 
long.densities = reshape(densities, idvar = "uniqueid", ids = as.character(densities$uniqueid),
               times = as.character(BBSyears), #timevar = "Characteristic",
               varying = list(as.character(BBSyears)), direction = "long")

colnames(long.densities)=c("loc","station","uniqueid","time", "density")
	
	# This is a test with randomly generated density data to make sure the code works before we apply real abundance data from Brooke
	# If working correctly, it should generate a straight line for a graph of S vs. density
	# Get average density for each location
TK_capacity = summaryBy(density~loc, long.densities,FUN=function(t) mean(t))
	# Get average density per location per year (if multiple stations, it will average the stations' abund for that year and loc) 
loc_y_density = summaryBy(density~loc+time,long.densities,FUN=function(t)mean(t))
	# Relative abundance
	# 'density_frame_fake' (with fake data) - loc, year, absDens, K, prop_abund
temp=merge(loc_y_density, TK_capacity ,by="loc")
colnames(temp)=c("loc","year", "absDens", "K")
  
  #first set of fake density
density_frame_fake=data.frame(temp, prop_abund=0)
density_frame_fake$prop_abund=density_frame_fake$absDens / density_frame_fake$K
     
  #for multiple fake density sets
density_frame_fake10=data.frame(temp, prop_abund=0)
density_frame_fake10$prop_abund=density_frame_fake10$absDens / density_frame_fake10$K


	##################### maps.ddl #####################################
	########## DENSITY ########################
	# Add density to the design matrix 

	#1) NO- forget location - group by populations - all stations 1 population, or several populations
		#group densities by location - Brooke - does she create it for years of MAPS data or preferrably for longer timeseries
		#what's right here? group by population or location??????????
			#you can see that in the create.inputFile function, in density_frame, that absDens,K,and rD were pulled by populations
	#2) figure out K for each location+station - mean abundance for all years (40- from 1972 to 2012), maybe 5 years moving window
	#3) use 2 to calculate relative abundance/proporition (abundance/K)
	#4) follow steps below

	#density_frame from Function 'create.popsANDdensities' should be used: not the density_frame from the above fake density data
	#density_frame has popluation, year, absDens, K, rD from Function 'create.popsANDdensities'
	#density_frame_fake from fake data has loc,year,absDens,K,prop_abund 
	
	# to put original density data in
#	maps.ddl$S$density=numeric(nrow(maps.ddl$S))
#	for(i in 1:nrow(maps.ddl$S)){
#		index = which((density_frame$year==maps.ddl$S$time[i])&
#                    (as.character(density_frame$loc)==as.character(maps.ddl$S$loc)[i])) 
#		maps.ddl$S$density[i] = ifelse(length(index>0),density_frame$prop_abund[index],0.001)
#	} 

	# to put fake density data in
maps.ddl$S$density=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
	index = which((density_frame_fake$year==maps.ddl$S$time[i])&
                    (as.character(density_frame_fake$loc)==as.character(maps.ddl$S$loc)[i])) 
	maps.ddl$S$density[i] = ifelse(length(index>0),density_frame_fake$prop_abund[index],0.001)
} 

      ## add another fake density data, just for kicks!
#maps.ddl$S$density2 <- runif(nrow(maps.ddl$S),0,2)
maps.ddl$S$density10=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  index = which((density_frame_fake10$year==maps.ddl$S$time[i])&
                  (as.character(density_frame_fake10$loc)==as.character(maps.ddl$S$loc)[i])) 
  maps.ddl$S$density10[i] = ifelse(length(index>0),density_frame_fake10$prop_abund[index],0.001)
} 

head(maps.ddl$S)
min(maps.ddl$S$density2)
max(maps.ddl$S$density2)

	## to put real BBS density data in

  # if grouped by location
  # add real density for each location and time step into maps.ddl$S
  # use previous year's density!
maps.ddl$S$density=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
	index = which((density_frame$year==maps.ddl$S$time[i])&
                  (as.character(density_frame$loc)==as.character(maps.ddl$S$loc)[i])) 
	maps.ddl$S$density[i] = ifelse(length(index>0),density_frame$rD[index],0.001)
}

  # if grouped by population
  # add real density for each population and time step into maps.ddl$S
#maps.ddl$S$density=numeric(nrow(maps.ddl$S))
#for(i in 1:nrow(maps.ddl$S)){
#  index = which((density_frame$year==maps.ddl$S$time[i])&
#                  (as.character(density_frame$population)==as.character(maps.ddl$S$pop)[i])) 
#  maps.ddl$S$density[i] = ifelse(length(index>0),density_frame$rD[index],0.001)
#} 

  ######### density from MAPS ##############

setwd(result_dir)
p.table<-read.csv("YBCH.F_table.2014-12-16.csv")
  # density with only Adults
p.table<-read.csv("COYE.F_table.2015-04-01.csv") # p.table with density of only adults (maps_prev.rD_ad)
head(p.table)

  # create 'maps_density_frame'
  # density with both J and A
maps_density_frame<-p.table[,c("pop","year","maps_rD","maps_prev.rD")]
mean(maps_density_frame$maps_rD,na.rm=T)    # should be 1
  # density with only Adults
maps_density_frame_ad<-p.table[,c("pop","year","maps_rD_ad","maps_prev.rD_ad")]
head(maps_density_frame_ad)
mean(maps_density_frame_ad$maps_rD_ad, na.rm=T)    # should be 1


  #### MAPS density check! ###########

  # for making MAPS density histogram
  #leave only rows with values for maps_rD - one row for each year
  #contains all years
maps_density_frame2<-maps_density_frame[!is.na(maps_density_frame$maps_rD),] 
head(maps_density_frame2)
sort(unique(maps_density_frame2$year))  # all years
  # densith with only Adults
maps_density_frame2_ad<-maps_density_frame_ad[!is.na(maps_density_frame_ad$maps_rD_ad),] 
head(maps_density_frame2_ad)
sort(unique(maps_density_frame2_ad$year))  # all years

  #check MAPS density
mean(maps_density_frame2$maps_rD)    # should be 1
sd(maps_density_frame2$maps_rD)
min(maps_density_frame2$maps_rD)
max(maps_density_frame2$maps_rD)
hist(maps_density_frame2$maps_rD, main=paste(speciesname,"MAPS density",sep=" "), xlab="MAPS density")

mean(maps_density_frame2$maps_prev.rD, na.rm=T)    # should be different from 1
sd(maps_density_frame2$maps_prev.rD, na.rm=T)
min(maps_density_frame2$maps_prev.rD, na.rm=T)
max(maps_density_frame2$maps_prev.rD, na.rm=T)

  #check MAPS density
  # density with only Adults
mean(maps_density_frame2_ad$maps_rD_ad)    # should be 1
sd(maps_density_frame2_ad$maps_rD_ad)
min(maps_density_frame2_ad$maps_rD_ad)
max(maps_density_frame2_ad$maps_rD_ad)
hist(maps_density_frame2_ad$maps_rD_ad, main=paste(speciesname,"MAPS density",sep=" "), xlab="MAPS density")

mean(maps_density_frame2_ad$maps_prev.rD_ad, na.rm=T)    # should be different from 1
sd(maps_density_frame2_ad$maps_prev.rD_ad, na.rm=T)
min(maps_density_frame2_ad$maps_prev.rD_ad, na.rm=T)
max(maps_density_frame2_ad$maps_prev.rD_ad, na.rm=T)

  # save histogram as .pdf
setwd(result_dir)
pdffilename=paste(speciesname, "S_MAPS density", Sys.Date(), "pdf", sep=".")
pdf(pdffilename, width=10, height=10, onefile=TRUE)
par(mfrow=c(1,1))
hist(maps_density_frame2$maps_rD, main=paste(speciesname,"MAPS density for S",sep=" "), xlab="MAPS density", col=rgb(0.8,0.8,0.8,0.5))
hist(maps_density_frame2$maps_prev.rD, col=rgb(0.1,0.1,0.1,0.5), add=T)
legend("topright", c("MAPS density", "MAPS previous density"), fill=c(rgb(0.8,0.8,0.8,0.5), rgb(0.1,0.1,0.1,0.5)))
#abline(v=mean(maps_density_frame2$maps_rD),col="red")
dev.off()

  # save maps_density_frame2 - containing all years
setwd(result_dir)
filename888<-paste(speciesname, "maps.rD", "csv", sep=".")
write.csv(maps_density_frame2,filename888)

  # save maps_density_frame2_ad - containing all years
setwd(result_dir)
filename888<-paste(speciesname, "maps.rD_ad", "csv", sep=".")
write.csv(maps_density_frame2_ad,filename888)

  # determine max(MAPS previous density) for Ceiling
  # for each population, get max(maps_prev.rD) over time
head(maps_density_frame2)
unique.pop<-sort(unique(maps_density_frame2$pop))
unique.pop
  # max for each location over time
max.maps_prev.rD<-data.frame(pop=unique.pop, max.maps_prev.rD=NA)
head(max.maps_prev.rD)

for (i in 1:nrow(max.maps_prev.rD)){
  index<-which(maps_density_frame2$pop==max.maps_prev.rD$pop[i])
  max.maps_prev.rD$max.maps_prev.rD[i]<-max(maps_density_frame2$maps_prev.rD[index],na.rm=T)
}
max.maps_prev.rD

  # save as .csv
setwd(result_dir)
filename<-paste(speciesname,"S_max.maps_prev.rD","csv",sep=".")
write.csv(max.maps_prev.rD,filename)

  # get the mean ( max over time) over all locations 
mean_max.maps_prev.rD<-mean(max.maps_prev.rD$max.maps_prev.rD)
mean_max.maps_prev.rD


  #### end of check ########


  #leave only rows with values for maps_prev.rD - one row for each year
  # removes year 1994 since there is no prev.rD
maps_density_frame<-maps_density_frame[!is.na(maps_density_frame$maps_prev.rD),] # removes year 1994 since there is no prev.rD
head(maps_density_frame)
sort(unique(maps_density_frame$year))  # beginyear removed

mean(maps_density_frame$maps_rD)  # slightly different from 1 because year 1994 is removed
mean(maps_density_frame$maps_prev.rD)

  # density with only Adults
  #leave only rows with values for maps_prev.rD - one row for each year
  # removes year 1994 since there is no prev.rD
maps_density_frame_ad<-maps_density_frame_ad[!is.na(maps_density_frame_ad$maps_prev.rD_ad),] # removes year 1994 since there is no prev.rD
head(maps_density_frame_ad)
sort(unique(maps_density_frame_ad$year))  # beginyear removed

mean(maps_density_frame_ad$maps_rD_ad)  # slightly different from 1 because year 1994 is removed
mean(maps_density_frame_ad$maps_prev.rD_ad)

  # add real maps previous density for each location and time step into maps.ddl$S
maps.ddl$S$maps_density=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  index = which((maps_density_frame$year==maps.ddl$S$time[i])&
                  (as.character(maps_density_frame$pop)==as.character(maps.ddl$S$loc)[i])) 
  maps.ddl$S$maps_density[i] = ifelse(length(index>0),maps_density_frame$maps_prev.rD[index],0.001)
}

head(maps.ddl$S)
maps.ddl$S$maps_density

  # density with only Adults
  # add real maps previous density for each location and time step into maps.ddl$S
maps.ddl$S$maps_density_ad=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  index = which((maps_density_frame_ad$year==maps.ddl$S$time[i])&
                  (as.character(maps_density_frame_ad$pop)==as.character(maps.ddl$S$loc)[i])) 
  maps.ddl$S$maps_density_ad[i] = ifelse(length(index>0),maps_density_frame_ad$maps_prev.rD_ad[index],0.001)
}

head(maps.ddl$S)
maps.ddl$S$maps_density_ad
sort(unique(maps.ddl$S$maps_density_ad))

  ########## WEATHER #######################
  # Add weather suitability to the design matrix 

  # add fake weather data
#maps.ddl$S$density=numeric(nrow(maps.ddl$S))
#for(i in 1:nrow(maps.ddl$S)){
#  index = which((density_frame_fake$year==maps.ddl$S$time[i])&
#                    (as.character(density_frame_fake$loc)==as.character(maps.ddl$S$loc)[i])) 
#	maps.ddl$S$density[i] = ifelse(length(index>0),density_frame_fake$prop_abund[index],0.001)
#} 

  # add fake weather data in
  # check before using!
maps.ddl$S$weather=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  maps.ddl$S$weather[i]=f.weather[c(as.character(maps.ddl$S$loc[i])),c(as.character(maps.ddl$S$time[i]))]
} 

  # add another set of fake weather data
maps.ddl$S$weather10=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  maps.ddl$S$weather10[i]=f.weather[c(as.character(maps.ddl$S$loc[i])),c(as.character(maps.ddl$S$time[i]))]
} 
head(maps.ddl$S)

  ############### REAL WEATHER #################
# add real weather data into maps.ddl$S

# make weather data.frame 'weather_map2'
# loc, Year, Month, Suitability

# weather_map: weather suitability from months 4,5,6,7
# what is station in weather_map is actually station
# add 'loc'
weather_map[, "loc"] <- 0
weather_map<-weather_map[,c("station","loc","Year","Month","Suitability")]

# place the corresponding 'loc' for station

for (i in 1:nrow(weather_map)){
  index<-which(station.loc.pop$station==as.character(weather_map$station[i]))
  weather_map$loc[i]<-as.character(station.loc.pop$loc[index])
}
head(weather_map)

# for locations which have multiple stations
# first, average by location - for each year and month, calculate average for all the stations in a location
# create matrix 'weather_map2' - each unique location for each year_month
u.w.loc<-sort(unique(weather_map$loc))
u.yrs<-sort(unique(weather_map$Year))
u.mon<-sort(unique(weather_map$Month))

w.nyrs<-length(unique(weather_map$Year))
w.nmon<-length(unique(weather_map$Month))
w.nyrsmon<-w.nyrs*w.nmon
w.nyrsmon

weather_map2<-data.frame(loc=rep(u.w.loc, each=w.nyrsmon), Year=rep(u.yrs, each=4), Month=rep(u.mon, w.nyrs), Suitability=NA)
weather_map2

colnames(weather_map2)<-c("loc","Year","Month","Suitability")
head(weather_map2)

for (i in 1:nrow(weather_map2)){
  ndx<-which((weather_map$loc==as.character(weather_map2$loc[i]))&(weather_map$Year==as.numeric(weather_map2$Year[i]))&(weather_map$Month==as.numeric(weather_map2$Month[i])))
  weather_map2$Suitability[i]<-mean(weather_map$Suitability[ndx])
}
head(weather_map2)
weather_map2$Suitability

# Now, take average of 4 months for all locations for each year

weather<-matrix(nrow=length(u.w.loc),ncol=length(u.yrs))
for (i in 1:length(u.w.loc)){
  for (j in 1:length(u.yrs)){
    ind<-which((weather_map2$loc==as.character(u.w.loc[i]))&(weather_map2$Year==u.yrs[j]))
    weather[i,j]<-mean(weather_map2$Suitability[ind])
  }
}
colnames(weather)<-u.yrs
rownames(weather)<-u.w.loc
head(weather)

  # reshape by 'loc', 'Time', 'weather'
real.w_df<-data.frame(weather,rownames(weather))
colnames(real.w_df)<-c(u.yrs,"loc")
ncol.real.w.df<-ncol(real.w_df) #make it vary by "loc"
weather_rshp<-reshape(real.w_df, varying=names(real.w_df)[-ncol.real.w.df], v.name="weather", idvar=c("loc"), timevar="Time", direction="long")
  #put back real years instead of 1,2,3.. for Time
for (i in 1:length(weather_rshp$Time)){
  weather_rshp$Time[i]<-weather_rshp$Time[i]+u.yrs[1]-1
}
head(weather_rshp)


  # now, add real weather into maps.ddl$S
maps.ddl$S$weather=numeric(nrow(maps.ddl$S))
for(i in 1:nrow(maps.ddl$S)){
  ndx<-which((weather_rshp$loc==as.character(maps.ddl$S$loc[i]))&(weather_rshp$Time==maps.ddl$S$time[i]))
  maps.ddl$S$weather[i]=weather_rshp$weather[ndx]
}
head(maps.ddl$S)

  # test if it worked 
maps.ddl$S$weather
i=6
ndx<-which((weather_rshp$loc==as.character(maps.ddl$S$loc[i]))&(weather_rshp$Time==maps.ddl$S$time[i]))
weather_rshp[ndx,]

#maps.ddl$S$weather=numeric(nrow(maps.ddl$S))
#for(i in 1:nrow(maps.ddl$S)){
#  maps.ddl$S$weather[i]=weather[c(as.character(maps.ddl$S$loc[i])),c(as.character(maps.ddl$S$time[i]))]
#} 
  #####################################################################
  ## 'isSpring' - ignore
	## Add another column to p to designate spring for juvenile analysis. 
	## Part of the season juveniles are not around so they cannot be captured.
maps.ddl$p$isSpring=rep(0,times=nrow(maps.ddl$p))
maps.ddl$c$isSpring=rep(0,times=nrow(maps.ddl$c))
for (i in 1:nrow(maps.ddl$p)) {
	if ( (maps.ddl$p$time[i] == 1) || (maps.ddl$p$time[i] == 2) )
		maps.ddl$p$isSpring[i]=1
}	#If time is 1 or 2, isSpring is 1.

for (i in 1:nrow(maps.ddl$c)) {
	if ( (maps.ddl$c$time[i] == 1) || (maps.ddl$c$time[i] == 2) )
			maps.ddl$c$isSpring[i]=1
}

	## change the "session" variable and add a year variable to the design data for "p" and "c"
maps.ddl$p$year=maps.ddl$p$session
maps.ddl$c$year=maps.ddl$c$session

	# add a "first capture year" component to ddl 
maps.ddl$GammaDoublePrime$firstYear <- as.factor(ifelse(maps.ddl$GammaDoublePrime$age==1,1,0)) 
result.maps.ddl=list(ddl=maps.ddl, process=maps.process)
maps.ddl=result.maps.ddl$ddl
maps.process=result.maps.ddl$process




#########################################################################
#Save result and maps.ddl for the species in Dropbox
setwd(result_dir)
  
  # 1) if population = location
filename<-paste(speciesname, "markinput", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_maps_density_real_weather", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_m_fake_weather_tests", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_m_fake_density_tests", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_trans", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_trans_maps.rD_weather", Sys.Date(), "RData", sep=".")
filename<-paste(speciesname, "markinput_trans_maps.rD_ad", Sys.Date(), "RData", sep=".")  # density with only Adults

  # 2) if all is one population
#filename<-paste(speciesname, "markinput_one", Sys.Date(), "RData", sep=".")
  # 3) if grouping at intermediate scale by conservation regions
#filename<-paste(speciesname, "markinput_int", Sys.Date(), "RData", sep=".")
#filename<-paste(speciesname, "markinput_SWIF", Sys.Date(), "RData", sep=".")

save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, effortmatrix, density_frame, station.loc.pop, f.weather, f.weather_rshp, file=filename)

# with trans - no density nor weather
save(result, maps.ddl, maps.process, band.data, band.data_allmon, effort, sim_stations, station.loc.pop, population_map, work2, work3, work_reshape, work_density, 
     test, test2, ch, ch2, ch3, effortmatrix, effortmat, effortdf, stratadf, trans, recap_within_sameyear, pot_trans, file=filename)

# with trans - with MAPS prev. density and weather
save(result, maps.ddl, maps.process, band.data, band.data_allmon, effort, sim_stations, weather_map, station.loc.pop, population_map, work2, work3, work_reshape, work_density, 
     test, test2, ch, ch2, ch3, effortmatrix, effortmat, effortdf, stratadf, trans, recap_within_sameyear, pot_trans, 
     p.table, maps_density_frame, weather_map2, weather, weather_rshp, file=filename)
  # without work2, work3, ch, ch2, ch3
save(result, maps.ddl, maps.process, band.data, band.data_allmon, effort, sim_stations, weather_map, station.loc.pop, population_map, work_reshape, work_density, 
     test, effortmatrix, effortdf, stratadf, trans, recap_within_sameyear, pot_trans, 
     p.table, maps_density_frame, weather_map2, weather, weather_rshp, file=filename)

# with trans - with MAPS prev. density of only Adults
save(result, maps.ddl, maps.process, p.table, maps_density_frame_ad, file=filename)


# with real BBS density and real weather
save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, weather_map, effortmatrix, density_frame, station.loc.pop, weather, weather_rshp, file=filename)

# with real MAPS density and real weather
save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, weather_map, effortmatrix, density_frame, maps_density_frame, station.loc.pop, weather, weather_rshp, file=filename)

# for 10 fake weather data tests
save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, effortmatrix, density_frame, station.loc.pop, f.weather, 
     f.weather_rshp, f.weather_rshp2, f.weather_rshp3, f.weather_rshp4, f.weather_rshp5, 
     f.weather_rshp6, f.weather_rshp7, f.weather_rshp8, f.weather_rshp9, f.weather_rshp10, file=filename)

# for 10 fake density data tests with weather_map
save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, weather_map, effortmatrix, 
     density_frame_fake, density_frame_fake2, density_frame_fake3, density_frame_fake4, density_frame_fake5, density_frame_fake6, 
     density_frame_fake7, density_frame_fake8, density_frame_fake9, density_frame_fake10,
     station.loc.pop, f.weather, f.weather_rshp, file=filename)
# for 10 fake density data tests without weather_map
save(result, maps.ddl, maps.process, band.data, effort, sim_stations, abundance_map, population_map, effortmatrix, 
     density_frame_fake, density_frame_fake2, density_frame_fake3, density_frame_fake4, density_frame_fake5, density_frame_fake6, 
     density_frame_fake7, density_frame_fake8, density_frame_fake9, density_frame_fake10,
     station.loc.pop, f.weather, f.weather_rshp, file=filename)
