rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions.R")
#source("lib/Islandwide Mean&Variance Functions.R")

# THIS SHOULD BE MOVED TO fish_team_Functions .. it calculates richness only for species with count>0
# The old Calc_Site_Species_Richness still has value for some circumstances - this should not replace that function
Modified_Site_Species_Richness<-function(x){  
  # Modification fos tandard Calc_Site_Species_Richness to not count species with zero counts (as they can be left in data file to ensure that the site has data records at all) 
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  y[y$x>1,]$x<-1																	#convert any non-zero count to 1, so we can sum those to get total number of species with count>0 
  z<-aggregate(y$x,by=y[,c("SITEVISITID", "METHOD", "REP")], sum)  		            # count number of species with non-zero counts this REP	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)
  
}
# end Modified_Site_Species_Richness


#LOAD THE CLEAN wd 
load("data/TMPwd.Rdata")

## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
wd[!wd$OBS_TYPE %in% c("U", "I", "N"), ]$COUNT<-0
wd<-subset(wd, wd$METHOD %in% c("nSPC"))
wd<-droplevels(wd)

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)

OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "ComplexityValue", "MEAN_SH", "SD_SH_DIFF", "MAX_HEIGHT")

# Generate a data frame with all benthic and site level information for each survey
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
save(surveys, file="data/TMPsurveys.Rdata")

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
save(species_table, file="data/TMPspecies.Rdata")

#### removing sharks and jacks altogether

test<-subset(wd, !wd$COMMONFAMILYALL %in% c("Jack", "Nurse", "Requiem", "Hammerhead",
"Turtle", "Dolphin", "UNKNOWN", "Monk Seal", "Tuna", "UNKNOWN", "", "Unspecified Fish"))
test<-droplevels(test)
wd<-test

# GENERATE SUMMARY METRICS --------------------------------------------------
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-names(r1[3:dim(r1)[2]])
r2<-Calc_Site_Bio(wd, "COMMONFAMILYALL"); family.cols<-names(r2[3:dim(r2)[2]])
r2<-r2[, c("SITEVISITID", "METHOD","Parrotfish")]; family.cols<-c("Parrotfish")
r2a<-Calc_Site_Abund(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
r2b<-Calc_Site_Bio(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)

##All biomass within each of the six log2 body-mass classes (32–64 g, 64–128 g, 128–256 g, 256–512 g, 512–1024 g, 1024–2048 g) was

2-4, 4-8, 8-16, 16-32, 
 
names(r2a)<-ifelse(names(r2a) %in% species.cols,
                   paste(names(r2a),"abund", sep = "_"),
                   names(r2a))
 ####
                   
# names(r2b)<-ifelse(names(r2b) %in% species.cols,
 #paste(names(r2b),"bio", sep = "_"),
 #names(r2b))
 
 
 ## tidy up species cols names for bio and abund
 
 species.cols.bio<-names(r2b)[3:dim(r2b)[2]]
 species.cols.abund<-names(r2a)[3:dim(r2a)[2]]
 
 ####

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsd<-merge(surveys,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
wsd<-merge(wsd,r2,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r2a,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r2b,by=UNIQUE_SURVEY)

data.cols<-c(trophic.cols, "TotFish", "Parrotfish", SURVEY_SITE_DATA, species.cols, species.cols.abund)
#wsd<-merge(wsd, r4b, by=UNIQUE_SURVEY)
#data.cols<-c(data.cols, "0_20", "20_50", "50_plus")
#names(wsd)[match(c("(0,20]", "(20,50]","(50,Inf]" ),names(wsd))] <- c("0_20", "20_50", "50_plus")
#wsd$BSR<-(wsd$HARD_CORAL+wsd$CCA)/(wsd$MA + wsd$TA)
#data.cols<-c(data.cols, "BSR")

## filter to just the PRIAs dataset

wsd<-subset(wsd, wsd$REGION == "PRIAs")
wsd<-droplevels(wsd)


## merge with the island level wave exposure data
waves<-read.csv("data/fish_waves_26_5_16.csv")
tmp<-merge(wsd, waves, all.x=T, by="SITEVISITID")

tmp$SITE<-tmp$SITE.x

drops<-c("SITE.x", "SITE.y", "Latitude", "Longitude")
wsd<-tmp[,!(names(tmp) %in% drops)]

# OUTPUT working_site_data  -----------------------------------
save(wsd, file="data/TMPwsd.Rdata")
save(data.cols, file="data/TMPdata.cols.Rdata")


# CAPPING OF DATA (e.g. for Monitoring report) # WE WILL not normally do this!
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start

#display quantile ranges of the data.cols
for(i in 1:(length(data.cols)))
{
	cat(data.cols[i]); 	cat(" ")
	cat(round(quantile(wsd[,data.cols[i]], c(0.9,0.95,0.975, 0.99), na.rm = T),1)); 	cat("      ")
}

#CAP FISH DATA VALUES TO SOMETHING CLOSE To 97.5% quantile
wsd[wsd$TotFish>450,]$TotFish<-450
wsd[wsd$PISCIVORE>300,]$PISCIVORE<-300
wsd[wsd$PLANKTIVORE>100,]$PLANKTIVORE<-100 # KM changed from 50
wsd[wsd$PRIMARY>80,]$PRIMARY<-80 # km changed from 85
wsd[wsd$SECONDARY>45,]$SECONDARY<-45 # KM changed from 40

#cap BSR too ... can be infinte in situations where divers record TA and MA as 0 (rare, but does happen)
#wsd[wsd$BSR>25 & !is.na(wsd$BSR),]$BSR<-25


# OUTPUT (not in Adel's script that I can see, but I think we should include) -----------------------------------
save(wsd, file="TMPwsdCAPPED.rdata")




