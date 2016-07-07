rm(list=ls())

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(splitstackshape)

setwd("/Users/adel.heenan/Documents/GitHub/reef-indicators-master")

source("data_pre-processing/lib/fish_team_functions.r")
source("data_pre-processing/lib/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################
## FIRST FISH, BENTHIC IS BELOW ... BE AWARE THAT THESE ARE MUCH ROUGHER AND GLITCHY THAN THE FISH REA SCRIPTS .. FAR LESS WORK HAS BEEN DONE ON THESE THAN FOR REA
#################################################################### TOW WORKUP ###########################################################################

load(file="data_pre-processing/data_raw/ALL_TOW_FISH_RAW.rdata")

# FISH TOW WORKINGS -------------------------------------------------------
wd<-df

## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]
miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

summary(wd)
unique(wd[wd$PROJECTEDLENGTH<50,]$ISLAND)

#remove segments with distance less than 50m
###wd<-wd[wd$PROJECTEDLENGTH>50,]

#wd<-subset(wd, wd$REGION=="SAMOA", drop=TRUE)
#wd<-subset(wd, wd$ISLAND != "South Bank", drop=TRUE)
#wd<-droplevels(wd)
wd[is.na(wd$COUNT),]$COUNT<-0
wd[is.na(wd$DEPTH),]$DEPTH<-0	
wd[is.na(wd$SIZE_),]$SIZE_<-0	
wd[is.na(wd$CENTROIDLAT),]$CENTROIDLAT<-0	
wd[is.na(wd$CENTROIDLON),]$CENTROIDLON<-0	
length(unique(wd$DIVEID))

tmp.lev<-levels(wd$REEF_ZONE); head(tmp.lev)
levels(wd$REEF_ZONE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$FAMILY); head(tmp.lev)
levels(wd$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TAXONNAME); head(tmp.lev)
levels(wd$TAXONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TROPHIC_MONREP); head(tmp.lev)
levels(wd$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

wd[is.na(wd$REEF_ZONE),"REEF_ZONE"]<-"UNKNOWN"
wd[is.na(wd$TAXONNAME),"TAXONNAME"]<-"UNKNOWN"
wd[is.na(wd$FAMILY),"FAMILY"]<-"UNKNOWN"
wd[is.na(wd$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

length(unique(wd$DIVEID))

wd$biomass_g<-wd$LW_A*wd$COUNT*((wd$SIZE_*wd$LENGTH_CONVERSION_FACTOR)^wd$LW_B)
 

#Fix errors in the database
wd[wd$SIZE_ <50,]
wd[wd$SIZE_ < 50,]$COUNT<-0    # These are really presence records ... NOT a QUAN cont record

# fix island codes
# set ISLAND of Lehua to Niihau (Lehua is a rock slightly offshore of Niihau)
wd[wd$ISLAND=="Lehua",]$ISLAND<-"Niihau"

levels(wd$ISLAND)<-c(levels(wd$ISLAND), "AGS")
wd[wd$ISLAND=="Alamagan",]$ISLAND<-"AGS"
wd[wd$ISLAND=="Guguan",]$ISLAND<-"AGS"
wd[wd$ISLAND=="Sarigan",]$ISLAND<-"AGS"
wd<-droplevels(wd)

## tidy up data - remove tuna, eels

a<-which(wd$COMMONFAMILYALL %in% c("Tuna", "Conger or Garden Eel", 
"Unspecified Fish","Worm or Snake Eel", "Moray"))
wd<-wd[-a,]

wd<-droplevels(wd)

#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "CENTROIDLAT", "CENTROIDLON", "DATE_", "DEPTH", "STARTLOCALTIME", "STRATA", "PROJECTEDLENGTH", "DIVEID")  

SEGMENT_ID2<-c( "DIVEID", "SEGMENT")
SEGMENT_INFO<-c("REGION", "ISLAND", "DATE_", "OBS_YEAR")
SEGMENT_INFO_TO_SUM<-c("PROJECTEDLENGTH")
SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH")
SEGMENT_INFO_TO_MIN<-c("STARTLOCALTIME")
SEGMENT_INFO_TO_MODE<-c("REEF_ZONE")

SEGMENT_FIELDS<-c(SEGMENT_INFO, SEGMENT_INFO_TO_SUM, SEGMENT_INFO_TO_AVE, SEGMENT_INFO_TO_MODE, SEGMENT_ID2)
DIVE_INFO<-c("DIVEID", SEGMENT_INFO)

#clean up the data file ## adel comment: this creates 14 warnings.... ### return to this, extract numeric only columns
##- invalid for factors with NA entries
wd[is.na(wd$BIOMASS),]$BIOMASS<-0
wd[is.na(wd$BIOMASS_G_M2),]$BIOMASS_G_M2<-0


segment.info<-aggregate(wd$COUNT, by=wd[,SEGMENT_FIELDS], sum)## aggregate sums total count of all fishes per record, using field_list 
segment.info<-segment.info[,SEGMENT_FIELDS] # drop the count - was just using that to generate a summary table

length(unique(segment.info$DIVEID))
	
#sum up to total length etc.. for the dive ID
#set depth, and centroid lat-long field to NaN if zero ... 
segment.info[segment.info$DEPTH==0,"DEPTH"]<-NaN
segment.info[segment.info$CENTROIDLAT==0,"CENTROIDLAT"]<-NaN
segment.info[segment.info$CENTROIDLON==0,"CENTROIDLON"]<-NaN

sum.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_SUM],by=segment.info[,DIVE_INFO], sum, na.rm=TRUE);
dimnames(sum.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_SUM)
ave.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], mean, na.rm=TRUE)  
med.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], median, na.rm=TRUE)  
mode.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_MODE],by=segment.info[,DIVE_INFO], Mode)
dimnames(mode.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_MODE)

tt<-merge(ave.segments, mode.segments[,c("DIVEID",SEGMENT_INFO_TO_MODE)], by="DIVEID"); dim(tt)
dive.info<-merge(tt, sum.segments[,c("DIVEID",SEGMENT_INFO_TO_SUM)], by="DIVEID"); dim(dive.info)

dive.info[is.na(dive.info$DEPTH),]
###FILLING IN SOME MISSING DEPTH VALUES FOR TOWS FROM THE CRUISE REPORT METADATAS
dive.info[dive.info$DIVEID == 200202261,]$DEPTH<-23
dive.info[dive.info$DIVEID == 200202262,]$DEPTH<-23.5
dive.info[dive.info$DIVEID == 200202263,]$DEPTH<-17.5
dive.info[dive.info$DIVEID == 200602192,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602193,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602194,]$DEPTH<-21.5  
dive.info[dive.info$DIVEID == 200602195,]$DEPTH<-16.5  
dive.info[dive.info$DIVEID == 200602196,]$DEPTH<-16.5  

dive.info[dive.info$DIVEID == 200602235,]$DEPTH<-15.83333333
dive.info[dive.info$DIVEID == 200803111,]$DEPTH<-15.60606061
dive.info[dive.info$DIVEID == 200803112,]$DEPTH<-14.39393939
dive.info[dive.info$DIVEID == 200803113,]$DEPTH<-18.18181818
dive.info[dive.info$DIVEID == 200803114,]$DEPTH<-17.72727273
dive.info[dive.info$DIVEID == 200803115,]$DEPTH<-17.12121212
dive.info[dive.info$DIVEID == 200803116,]$DEPTH<-15.15151515

dive.info[dive.info$DIVEID == 200202264,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200202184,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200402115,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 200402116,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 201503131,]$DEPTH<-18.9  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503132,]$DEPTH<-15.3  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503133,]$DEPTH<-17.4  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503134,]$DEPTH<-16.7  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503135,]$DEPTH<-16.9  #Average manually lifted from the SeaBird file

dive.info[is.na(dive.info$DEPTH),]$DIVEID

write.csv(dive.info, file="data_pre-processing/data_raw/tmp Tows.csv")
############################################################
### Now sum abundance and biomass data per species per dive,
### and convert to gm2 and abund m2
############################################################


## couple of data entry errors - where I think people have sized one fish, but not entered the count as 1

wd[which(wd$SPECIES != "NONE" & wd$COUNT == "0"),]$COUNT<-1


#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

sum.abund.bio<-aggregate(wd[,c("COUNT", "biomass_g")],by=wd[,c("DIVEID", "SPECIES", "SIZE_")], sum, na.rm=TRUE);
tfd<-merge(sum.abund.bio, dive.info[,c("DIVEID","PROJECTEDLENGTH")], by="DIVEID")
tfd$BIOGM2<-tfd$biomass_g / (10*tfd$PROJECTEDLENGTH)
tfd$ABUNM2<-tfd$COUNT / (10*tfd$PROJECTEDLENGTH)
length(unique(tfd$DIVEID))

## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE
tfd<-merge(tfd, t.species.table[, FISH_SPECIES_FIELDS], by="SPECIES")
# add data about the tow (island, zone, year, depth)
tfd<-merge(tfd, dive.info[, c("DIVEID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH")], by="DIVEID")
length(unique(tfd$DIVEID))

write.csv(tfd, file="data_pre-processing/data_raw/TMPtowData.csv")

#filter out forereef tows only...!!!!! Ie drop Rose Backreef and Lagoon tows
tfd<-subset(tfd, tfd$REEF_ZONE %in% c("Forereef", "Unspecified"))
#set the one tow from Tutuuila with undpecified REEF_ZONE to Forereef .. depths are ~10m, so probably must be forereef (never mind that every other tow ever is forereef)
tfd[tfd$REEF_ZONE=="Unspecified",]$REEF_ZONE<-"Forereef"
length(unique(tfd$DIVEID))

summary(tfd$DEPTH)

#Filter out tows less than 10m OR greater than 20m
tfd<-subset(tfd, tfd$DEPTH>10 & tfd$DEPTH<20)
tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

### CAN FILTER OUT BELOW CERTAIN SIZES HERE...
SIZE_CUT_OFF<-1
tfd[tfd$SIZE_<SIZE_CUT_OFF, c("BIOGM2", "ABUNM2")]<-0

#### DROPPING BARRACUA - TOO VARIABLE
#tfd[tfd$FAMILY=="Sphyraenidae",]$BIOGM2<-0
#tfd[tfd$FAMILY=="Sphyraenidae",]$ABUNM2<-0

#use these as generic data and pooling levels
tfd$DATA_COL<-tfd$BIOGM2
tfd$GROUPING<-tfd$SPECIES


### data cleaning
#tfd<-tfd[tfd$OBS_YEAR > 2009,]
tfd<-droplevels(tfd)

### need to remove the outliers

## cap outlier observations @ 95%

specieslevel.cols<-levels(tfd$GROUPING)
tmp<-tfd[, c("GROUPING", "DATA_COL")]

for(i in 1:length(specieslevel.cols)){
	### i<-1
	
	one_col<-tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]
	a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]<-one_col
	print((specieslevel.cols)[i])
}

tfd$DATA_COL<-tmp$DATA_COL

## capped data
xx<-aggregate(tfd$DATA_COL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "GROUPING")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "GROUPING", "DATA_COL")


#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA ~ GROUPING, value="DATA_COL", fill=0)
data.cols<-levels(tfd$GROUPING)
wtd$TotFish<-rowSums(wtd[,data.cols])

length(unique(wtd$DIVEID))

tld<-merge(wtd, dive.info, by="DIVEID")
head(tld)

tld$REGION<-tld$REGION.x
tld$ISLAND<-tld$ISLAND.x

drops<-c("REGION.y", "ISLAND.y", "REGION.x", "ISLAND.x")
tld<-tld[,!(names(tld) %in% drops)]

write.csv(tld, file="data_pre-processing/data_raw/TMP FishTow_Mean_family_tow_level_BIO.csv")


#aggregate - average per island/strata/year
tfi.mean<-aggregate(wtd[,c("TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(wtd[,c("TotFish")],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], length)
tfi.var<-aggregate(wtd[,c( "TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], var, na.rm=TRUE)
tfi.se<-tfi.mean
tfi.se[,c("TotFish", data.cols)]<-sqrt(tfi.var[,c("TotFish", data.cols)])/sqrt(tfi.n$x)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$x

write.csv(tfi.mean, file="data_pre-processing/data_raw/TMP FishTow_Mean_species_island_year_BIO.csv")
write.csv(tfi.se, file="data_pre-processing/data_raw/TMP FishTow_SE_species_island_year_BIO.csv")


###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$STRATA=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
!!!! dodgy quick index of data cols make sure col numbers are correct
idw<-aggregate(island.data[,5:148],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,154:297],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)

names(idw.se)[3:dim(idw.se)[2]]<-c("TotFish", data.cols)

island.data.sp.bio<-list(idw, idw.se)
save(island.data.sp.bio, file="data/tow_2010_on_forereef_1_bio.Rdata")

#################################################################################
## ISLAND level abundance

#use these as generic data and pooling levels
tfd$DATA_COL<-tfd$ABUNM2
tfd$GROUPING<-tfd$SPECIES


### need to remove the outliers

## cap outlier observations @ 95%

specieslevel.cols<-levels(tfd$GROUPING)
tmp<-tfd[, c("GROUPING", "DATA_COL")]

for(i in 1:length(specieslevel.cols)){
	### i<-1
	
	one_col<-tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]
	a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]<-one_col
	print((specieslevel.cols)[i])
}

tfd$DATA_COL<-tmp$DATA_COL

## capped data
xx<-aggregate(tfd$DATA_COL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "GROUPING")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "GROUPING", "DATA_COL")


#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA ~ GROUPING, value="DATA_COL", fill=0)
data.cols<-levels(tfd$GROUPING)
wtd$TotFish<-rowSums(wtd[,data.cols])

length(unique(wtd$DIVEID))

tld<-merge(wtd, dive.info, by="DIVEID")
head(tld)

tld$REGION<-tld$REGION.x
tld$ISLAND<-tld$ISLAND.x

drops<-c("REGION.y", "ISLAND.y", "REGION.x", "ISLAND.x")
tld<-tld[,!(names(tld) %in% drops)]

write.csv(tld, file="data_pre-processing/data_raw/TMP FishTow_Mean_family_tow_level_ABUND.csv")



#aggregate - average per island/strata/year
tfi.mean<-aggregate(wtd[,c("TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(wtd[,c("TotFish")],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], length)
tfi.var<-aggregate(wtd[,c( "TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], var, na.rm=TRUE)
tfi.se<-tfi.mean
tfi.se[,c("TotFish", data.cols)]<-sqrt(tfi.var[,c("TotFish", data.cols)])/sqrt(tfi.n$x)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$x

write.csv(tfi.mean, file="data_pre-processing/data_raw/TMP FishTow_Mean_species_island_year_ABUND.csv")
write.csv(tfi.se, file="data_pre-processing/data_raw/TMP FishTow_SE_species_island_year_ABUND.csv")


###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$STRATA=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,5:148],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,154:297],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)

names(idw.se)[3:dim(idw.se)[2]]<-c("TotFish", data.cols)

island.data.sp.abund<-list(idw, idw.se)
save(island.data.sp.abund, file="data/tow_2010_on_forereef_2_abund.Rdata")

####################################################################

#####  mean length, mass of all tow fish and biomass of fish > 1 kg
## tfd = working data

## mean length - take the mean size per species tow and then across islands and years

#use these as generic data and pooling levels
tfd$DATA_COL<-tfd$SIZE_
tfd$GROUPING<-tfd$SPECIES

### data cleaning
#tfd<-tfd[tfd$COUNT > 0,]
#tfd<-droplevels(tfd)

## need to create a new row for each count of 
test<-tfd[,c("DIVEID", "SPECIES", "SIZE_", "COUNT")]
##
tester<-expandRows(test, "COUNT", drop = F)
# put the zero NONEs back in 
a<-test[which(test$SPECIES == "NONE"),]
a<-droplevels(a)

test<-rbind(tester, a)

tester2<-aggregate(test$SIZE_, by=test[,c("DIVEID", "SPECIES")], mean, na.rm=T)
a<-tapply(tester2$x, tester2$DIVEID, mean, na.rm=T)

a<-data.frame(a)
a$DIVEID<-rownames(a)
a$SIZE_<-a$a

tld<-merge(a, dive.info, by="DIVEID", all.x=T)
head(tld)

write.csv(tld, file="data_pre-processing/data_raw/TMP FishTow_Mean_family_tow_level_mean_size.csv")

#aggregate - average per island/strata/year
tfi.mean<-aggregate(tld[,c("SIZE_")],by=tld[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], mean, na.rm=TRUE)
names(tfi.mean)[5]<-"SIZE_"
tfi.n<-aggregate(tld[,c("SIZE_")],by= tld[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], length)
tfi.var<-aggregate(tld[,c("SIZE_")],by= tld[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], var, na.rm=TRUE)
names(tfi.var)[5]<-"SIZE_"
tfi.se<-tfi.mean
tfi.se[,c("SIZE_")]<-sqrt(tfi.var[,c("SIZE_")])/sqrt(tfi.n$x)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$x

write.csv(tfi.mean, file="data_pre-processing/data_raw/TMP FishTow_Mean_size_island_year.csv")
write.csv(tfi.se, file="data_pre-processing/data_raw/TMP FishTow_SE_size_island_year.csv")

###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$REEF_ZONE=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$OBS_YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,"SIZE_"],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,"SIZE_.1"],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)

names(idw)[3]<-c("SIZE_")
names(idw.se)[3]<-c("SIZE_")

island.data.size<-list(idw, idw.se)
save(island.data.size, file="data/tow_2010_on_forereef_3_mean_length.Rdata")

#############################
## mean mass - use the total fish biomass
## biomass of fishes > 1 kg
## use these as generic data and pooling levels

tfd$biomass_g_per_fish<-tfd$biomass_g / tfd$COUNT

## select fishes > 1 kg

tmp<-tfd[which(tfd$biomass_g_per_fish > 1000),]
tmp2<-tfd[which(tfd$SPECIES == "NONE"),]
tmp2[is.na(tmp2$biomass_g_per_fish),]$biomass_g_per_fish<-0
tmp3<-rbind(tmp, tmp2) ## large fish but still including zero counts - no fish obsed


tfd_bf<-tmp3

tfd_bf$BIOGM2_BIG_FISH<-tfd_bf$biomass_g_per_fish / (10*tfd_bf$PROJECTEDLENGTH) ## bio of big fish

tfd_bf$DATA_COL<-tfd_bf$BIOGM2_BIG_FISH ## get it in kgs
tfd_bf$GROUPING<-tfd_bf$SPECIES

### data cleaning
tfd_bf<-tfd_bf[tfd_bf$OBS_YEAR > 2009,]
tfd_bf<-droplevels(tfd_bf)

### need to remove the outliers

## cap outlier observations @ 95%

specieslevel.cols<-levels(tfd$GROUPING)
tmp<-tfd_bf[, c("GROUPING", "DATA_COL")]

for(i in 1:length(specieslevel.cols)){
	### i<-1
	
	one_col<-tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]
	a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	tmp[which(tmp$GROUPING == specieslevel.cols[i]),2]<-one_col
	print((specieslevel.cols)[i])
}

tfd_bf$DATA_COL<-tmp$DATA_COL

## capped data
xx<-aggregate(tfd_bf$DATA_COL, by=tfd_bf[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "GROUPING")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "GROUPING", "DATA_COL")


#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA ~ GROUPING, value="DATA_COL", fill=0)
data.cols<-names(wtd)[6:50]
wtd$TotFish<-rowSums(wtd[,data.cols])

length(unique(wtd$DIVEID))

tld<-merge(wtd, dive.info, by="DIVEID")
head(tld)

write.csv(tld, file="data_pre-processing/data_raw/TMP FishTow_Mean_species_tow_level_BIO_LARGE_FISH_1KG.csv")

#aggregate - average per island/strata/year

tfi.mean<-aggregate(wtd[,c("TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(wtd[,c("TotFish")],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], length)
tfi.var<-aggregate(wtd[,c( "TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], var, na.rm=TRUE)
tfi.se<-tfi.mean
tfi.se[,c("TotFish", data.cols)]<-sqrt(tfi.var[,c("TotFish", data.cols)])/sqrt(tfi.n$x)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$x

write.csv(tfi.mean, file="data_pre-processing/data_raw/TMP FishTow_Mean_species_island_year_BIO_LARGE_FISH_1KG.csv")
write.csv(tfi.se, file="data_pre-processing/data_raw/TMP FishTow_SE_species_island_year_BIO_LARGE_FISH_1KG.csv")

###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$STRATA=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,c("TotFish", data.cols)],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,c("TotFish", data.cols)],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)

names(idw.se)[3:dim(idw.se)[2]]<-c("TotFish", data.cols)

island.data.sp.bio_large_fish<-list(idw, idw.se)
save(island.data.sp.bio_large_fish, file="data/tow_2010_on_forereef_4_big_fish.Rdata")

###################################################################
## creating the mass bins ## 
## need to go through the same step of getting mass per fish, including the "NONE"s
###################################################################

## at the moment biomass is total per species, irrespective of number of indivs - need to put back to biomass per to then put in size mass bins

## create a row per fish  
#tfd$biomass_g_per_fish<-tfd$biomass_g / tfd$COUNT

## create a new row for individual counts per biomass_g
test<-tfd[,c("DIVEID", "SPECIES", "COUNT", "biomass_g")]
test$biomass_g_per_fish<-test$biomass_g / test$COUNT

##
tester<-expandRows(test, "COUNT", drop = F)
# put the zero NONEs back in 
a<-test[which(test$SPECIES == "NONE"),]
a<-droplevels(a)
a$biomass_g_per_fish<-0

test<-rbind(tester, a)

## create a the mass bins are a factor level

size_classes = c(0,20,40,80,160,320,640, 12800, 25600, 51200, 1024000, 2048000)
 
  	test$sizeclass<-cut(test$biomass_g_per_fish, breaks = size_classes, include.lowest=TRUE)
  	
  	levels(test$sizeclass)<-c("0_20", "20_40", "40_80", "80_160", "160_320", "320_640","640_12800", "12800_25600", "25600_51200", "51200_1024000", "1024000_2048000")

tester2<-aggregate(test$biomass_g_per_fish, by=test[,c("DIVEID", "sizeclass")], mean)

tld<-merge(tester2, dive.info, by="DIVEID", all.x=T)
tld$x<-tld$x/tld$PROJECTEDLENGTH
head(tld)

### cap 95% outliers

sizeclass<-levels(tld$sizeclass)
tmp<-tld[, c("sizeclass", "x")]

for(i in 1:length(sizeclass)){
	### i<-1
	
	one_col<-tmp[which(tmp$sizeclass == sizeclass[i]),2]
	a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	tmp[which(tmp$sizeclass == sizeclass[i]),2]<-one_col
	print((sizeclass)[i])
}

tld$x<-tmp$x

#################

## make a cross2 tab
tt<-cast(tld, DIVEID + REGION + ISLAND + OBS_YEAR + REEF_ZONE ~ sizeclass, value="x", fill=0)

write.csv(tt, file="data_pre-processing/data_raw/TMP FishTow_Mean_biomass_per_mass_class.csv")

DATA_COL<-names(tt)[6:15]

#aggregate - average per island/strata/year
tfi.mean<-aggregate(tt[,c(DATA_COL)],by=tt[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(tt[,c(DATA_COL)],by= tt[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], length)
tfi.var<-aggregate(tt[,c(DATA_COL)],by= tt[,c("REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR")], var, na.rm=TRUE)

tfi.se<-tfi.mean
tfi.se[,c(DATA_COL)]<-sqrt(tfi.var[,c(DATA_COL)])/sqrt(tfi.n[,c(DATA_COL)])

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.n[,5]

write.csv(tfi.mean, file="data_pre-processing/data_raw/TMP FishTow_Mean_biomass_per_mass_class_year.csv")
write.csv(tfi.se, file="data_pre-processing/data_raw/TMP FishTow_Mean_biomass_per_mass_class_year.csv")

###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$REEF_ZONE=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$OBS_YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,5:14],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,20:29],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)

names(idw)[3:12]<-DATA_COL
names(idw.se)[3:12]<-c(DATA_COL)

island.data.mass.class<-list(idw, idw.se)
save(island.data.mass.class, file="data/tow_2010_on_forereef_5_mass_size_class.Rdata")

###################################################################
#################################################################### TOW WORKUP END #######################################################################

