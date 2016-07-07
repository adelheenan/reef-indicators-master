#Use a grid to define reef scale FD

#Read in study sites
#Load in points
sites.dir = "/nfs/coralreef-data/Original_Datasets/CORAL_FishSites_Datasets"
extract.points=file.path(sites.dir, "Sites/mergedSites_no_parefico.csv")
merged.sites <- read.csv(extract.points)

#Read in site list
cred.data <- as.data.frame(read.csv('/nfs/coralreef-data/FxnDiv/Functional-Diversity-Metrics/Rarefaction/Data_for_cluster/cred_remote_2010_2015.csv'));
cred.data$unique.site <- paste(cred.data$SITE, cred.data$DATE_, cred.data$METHOD)
cred.sites <- merged.sites[as.character(merged.sites$site.id) %in% cred.data$unique.site,]

#Recenter data on the Pacific
cred.sites$lon[which(cred.sites$lon <0)] <- cred.sites$lon[which(cred.sites$lon <0)] + 360

cred.sites <- SpatialPointsDataFrame(cred.sites[,3:2], cred.sites[,c(1,4)])
proj <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
proj4string(cred.sites) <- proj


plot(cred.sites)



#Create a 1 km grid
#Use i km based on Kramer and Chapman 1999 Envir Biol Fish
#one degree latitute based on 15 degrees lat
#107550.46 m = 1 degree

107550.46/1000

1/107.5505

#0.009297958 degree grid size for box

library(raster)

test <-raster(xmn=cred.sites@bbox[1,1], xmx=cred.sites@bbox[1,2], ymn=cred.sites@bbox[2,1], ymx=cred.sites@bbox[2,2], 
              crs = proj,  resolution = 0.009297958, vals=NULL)
length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:30358944,4662,6512)
#image(xy)

# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
extent(rast) <- extent(cred.sites)
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

test2<-extract(rast,cred.sites)
cred.data2 <- cbind(as.data.frame(cred.sites), test2)

#say what point are in the same grid cell
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(203, 206), ylim = c(19,21))

sum.grid <- aggregate(site.id ~ test2, data = cred.data2, FUN = length)
sum(sum.grid$site.id >2)/nrow(sum.grid)
hist(sum.grid$site.id)
sum(is.na(cred.data2$test2))
#Try bigger home range size

#relationship between home range and fork length from Kramer and Chapman
#log10y = -3.75+2.35log10x

# Load in trait data - 2010-2014 only
fg <- read.csv('/nfs/coralreef-data/FxnDiv/Functional-Diversity-Metrics/Rarefaction/Data_for_cluster/traits.10.14.csv')
rownames(fg) <- fg[,1]
fg <- fg[2:5]
hist(fg$LMAX, breaks = 20)
sum(fg$LMAX > 200)/nrow(fg)

#94% of spp less than 100cm TL


#Need to fix fork to total but....
10^(-3.75+2.35*log10(1000))

#About 2 km for a fish of 1 m

hist(cred.data$converted_length, breaks = 30)
sum(cred.data$converted_length > 50)/nrow(cred.data)
sum(sum.grid$site.id[sum.grid$site.id>2])/sum(sum.grid$site.id)


#99% of observed individuals less than 50 cm


#Try to redo on a 2 km grid

#Figure out the number of cells

test <-raster(xmn=cred.sites@bbox[1,1], xmx=cred.sites@bbox[1,2], ymn=cred.sites@bbox[2,1], ymx=cred.sites@bbox[2,2], 
              crs = proj,  resolution = 0.009297958*2, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,2331,3256)
#image(xy)

# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1], cred.sites@bbox[1,2],cred.sites@bbox[2,1],cred.sites@bbox[2,2])
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

test2<-extract(rast,cred.sites)
cred.data2 <- cbind(as.data.frame(cred.sites), test2)

#say what point are in the same grid cell
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(204, 205), ylim = c(19,20))
plot(rast, add = TRUE)

sum.grid <- aggregate(site.id ~ test2, data = cred.data2, FUN = length)
hist(sum.grid$site.id, breaks = 30)

sum(sum.grid$site.id == 1)/nrow(sum.grid)
sum(sum.grid$site.id[sum.grid$site.id>2])/sum(sum.grid$site.id)


#Mellin et al. 2010 average reef size is ~ 6km
#Figure out the number of cells

test <-raster(xmn=cred.sites@bbox[1,1], xmx=cred.sites@bbox[1,2], ymn=cred.sites@bbox[2,1], ymx=cred.sites@bbox[2,2], 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,777,1085)
#image(xy)

# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords
ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1], cred.sites@bbox[1,2],cred.sites@bbox[2,1],cred.sites@bbox[2,2])
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

test2<-extract(rast,cred.sites)
cred.data2 <- cbind(as.data.frame(cred.sites), test2)

#say what point are in the same grid cell
#plot some islands to see how they fall
plot(rast,  xlim = c(204, 205), ylim = c(19,20))
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(203, 206), ylim = c(19,21))
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(183.4, 183.6), ylim = c(0.1,0.3)) #Baker
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(197.5, 198.5), ylim = c(5.5,6.5)) #Palmyra and Kingman
e <- c(197.5, 198.2,5.8,6.5)
clip.rast <- crop(rast, e) 
rast.poly <- rasterToPolygons(clip.rast)
plot(rast.poly)
plot(cred.sites, add = TRUE)


#plot(rast, add = TRUE)

sum.grid <- aggregate(site.id ~ test2, data = cred.data2, FUN = length)
hist(sum.grid$site.id, breaks = 60)

sum(sum.grid$site.id >2)/nrow(sum.grid)

#Number of sites that fall into a grid cell 2 or more other sites
sum(sum.grid$site.id[sum.grid$site.id>2])/sum(sum.grid$site.id)

colnames(cred.data2)[5] <- "grid.a"

#211 cells have at least 4 sites in them


#*********************
#Shift grid grid 3 times
test <-raster(xmn=cred.sites@bbox[1,1]-.1, xmx=cred.sites@bbox[1,2]+.1, ymn=cred.sites@bbox[2,1]-.1, ymx=cred.sites@bbox[2,2]+.1, 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,test@nrows,test@ncols)
#image(xy)
# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1]-.1, cred.sites@bbox[1,2]+.1,cred.sites@bbox[2,1]-.1,cred.sites@bbox[2,2]+.1)
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

grida<-extract(rast,cred.sites)
cred.data2 <- cbind(as.data.frame(cred.sites), grida)
sum(is.na(cred.data2$grida)) #make sure all sites fall within a grid


#Shift by 0.01 degrees in both directions
test <-raster(xmn=cred.sites@bbox[1,1]-.1+0.01, xmx=cred.sites@bbox[1,2]+.1+0.01, ymn=cred.sites@bbox[2,1]-.1+0.01, ymx=cred.sites@bbox[2,2]+.1+0.01, 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,test@nrows,test@ncols)
#image(xy)
# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
#ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1]-.1+0.01, cred.sites@bbox[1,2]+.1+0.01,cred.sites@bbox[2,1]-.1+0.01,cred.sites@bbox[2,2]+.1+0.01)
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

gridb<-extract(rast,cred.sites)
cred.data2 <- cbind(cred.data2, gridb)
sum(is.na(cred.data2$gridb)) #make sure all sites fall within a grid

#cor.test(cred.data2$grida, cred.data2$gridb) # r = 0.9999994 only one or two sites classified different?
sum(!(cred.data2$grida == cred.data2$gridb)) 

#convert into just 1:x names
cred.data2$grida <- as.factor(cred.data2$grida)
cred.data2$grida <- as.numeric(cred.data2$grida)

cred.data2$gridb <- as.factor(cred.data2$gridb)
cred.data2$gridb <- as.numeric(cred.data2$gridb)
plot(cred.data2$grida, cred.data2$gridb) 
cor.test(cred.data2$grida, cred.data2$gridb) 
sum(!(cred.data2$grida == cred.data2$gridb))

sum.grid.a <- aggregate(site.id ~ grida, data = cred.data2, FUN = length) 
sum.grid.b <- aggregate(site.id ~ gridb, data = cred.data2, FUN = length)

# 11 more "reefs" in second shift


plot(sum.grid.a$site.id, sum.grid.b$site.id)

#Shift by 0.02 degrees in both directions
test <-raster(xmn=cred.sites@bbox[1,1]-.1+0.02, xmx=cred.sites@bbox[1,2]+.1+0.02, ymn=cred.sites@bbox[2,1]-.1+0.02, ymx=cred.sites@bbox[2,2]+.1+0.02, 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,test@nrows,test@ncols)
#image(xy)
# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
#ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1]-.1+0.02, cred.sites@bbox[1,2]+.1+0.02,cred.sites@bbox[2,1]-.1+0.02,cred.sites@bbox[2,2]+.1+0.02)
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

gridc<-extract(rast,cred.sites)
cred.data2 <- cbind(cred.data2, gridc)
sum(is.na(cred.data2$gridc)) #make sure all sites fall within a grid


cred.data2$gridc <- as.factor(cred.data2$gridc)
cred.data2$gridc <- as.numeric(cred.data2$gridc)
plot(cred.data2$gridc, cred.data2$grida) 
cor.test(cred.data2$grida, cred.data2$gridc) 
sum(!(cred.data2$grida == cred.data2$gridc))
sum.grid.c <- aggregate(site.id ~ gridc, data = cred.data2, FUN = length)



plot(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
cor.test(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
sum(!(cred.data2$grida == cred.data2$gridc))


#Grid d
#Shift by 0.02 degrees in both directions
test <-raster(xmn=cred.sites@bbox[1,1]-.1+0.03, xmx=cred.sites@bbox[1,2]+.1+0.03, ymn=cred.sites@bbox[2,1]-.1+0.03, ymx=cred.sites@bbox[2,2]+.1+0.03, 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,test@nrows,test@ncols)
#image(xy)
# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
#ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1]-.1+0.03, cred.sites@bbox[1,2]+.1+0.03,cred.sites@bbox[2,1]-.1+0.03,cred.sites@bbox[2,2]+.1+0.03)
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

gridd<-extract(rast,cred.sites)
cred.data2 <- cbind(cred.data2, gridd)
sum(is.na(cred.data2$gridd)) #make sure all sites fall within a grid


cred.data2$gridd <- as.factor(cred.data2$gridd)
cred.data2$gridd <- as.numeric(cred.data2$gridd)
plot(cred.data2$gridd, cred.data2$grida) 
cor.test(cred.data2$grida, cred.data2$gridd) 
sum(!(cred.data2$grida == cred.data2$gridd))
sum.grid.d <- aggregate(site.id ~ gridd, data = cred.data2, FUN = length)

plot(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
cor.test(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
sum(!(cred.data2$grida == cred.data2$gridc))



#Grid e
#Shift by 0.02 degrees in both directions
test <-raster(xmn=cred.sites@bbox[1,1]-.1+0.04, xmx=cred.sites@bbox[1,2]+.1+0.04, ymn=cred.sites@bbox[2,1]-.1+0.04, ymx=cred.sites@bbox[2,2]+.1+0.04, 
              crs = proj,  resolution = 0.009297958*6, vals=NULL)

length(test) 
## Create a matrix with random data & use image()
xy <- matrix( 1:length(test) ,test@nrows,test@ncols)
#image(xy)
# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
#ext.test <- extent(cred.sites)
extent.rast <-c(cred.sites@bbox[1,1]-.1+0.04, cred.sites@bbox[1,2]+.1+0.04,cred.sites@bbox[2,1]-.1+0.04,cred.sites@bbox[2,2]+.1+0.04)
extent(rast) <- extent.rast
# ... and assign a projection
projection(rast) <-proj
plot(rast)
plot(cred.sites, add = TRUE)

gride<-extract(rast,cred.sites)
cred.data2 <- cbind(cred.data2, gride)
sum(is.na(cred.data2$gride)) #make sure all sites fall within a grid


cred.data2$gride <- as.factor(cred.data2$gride)
cred.data2$gride <- as.numeric(cred.data2$gride)
plot(cred.data2$gride, cred.data2$gridc) 
cor.test(cred.data2$grida, cred.data2$gride) 
sum(!(cred.data2$grida == cred.data2$gride))
sum.grid.e <- aggregate(site.id ~ gride, data = cred.data2, FUN = length)

plot(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
cor.test(cred.data2$grida, cred.data2$gridc) # r = 0.9999994 only one or two sites classified different?
sum(!(cred.data2$grida == cred.data2$gridc))


#e and c assign sites equally among cells and minimize the number of reefs
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$gridd, xlim = c(203, 206), ylim = c(19,21))
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$gride, xlim = c(183.4, 183.6), ylim = c(0.1,0.3)) #Baker
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$gridd, xlim = c(197.5, 198.5), ylim = c(5.5,6.5)) #Palmyra and Kingman

#grid b minimizes number of unique reefs

#number of reefs with at least 3 pts
num.reefs <- aggregate(site.id ~ gridb, data = cred.data2, FUN = length)
sum(num.reefs$site.id >2) #102 reef with at least 3 points

#Save b because it minimizes number of reefs
write.csv(cred.data2, file = "/nfs/coralreef-data/FxnDiv/All_islands/Rarefaction/Data_for_cluster/reef_scale_grid_assignments.csv")



#Determine how many fish to rarefy to
#pull out only reefs with at least 3 sites
b.3.sites <- sum.grid.b[which(sum.grid.b$site.id >2),]
cred.data2.b3sites <- cred.data2[(cred.data2$gridb %in% b.3.sites$gridb),]
cred.data.reef <- merge(cred.data, cred.data2.b3sites, by.x = "unique.site", by.y = "site.id")


reef.count <- aggregate(COUNT ~ gridb, data = cred.data.reef, FUN = sum)
hist(reef.count$COUNT, breaks = 2000, xlim = c(0,10000))
sum(reef.count$COUNT > 1000)/nrow(reef.count)

#Create list of sites within a reef
reef.list <- unique(cred.data.reef[,c("unique.site", "SITE_LEADING_ZEROS", "gridb")])
reef.list$gridb <- paste0("reef_", reef.list$gridb)

write.csv(reef.list, file = "/nfs/coralreef-data/FxnDiv/All_islands/Rarefaction/Data_for_cluster/reef_scale_grid_b_assignments.csv")


#Determine 























#say what point are in the same grid
plot(rast,  xlim = c(204, 205), ylim = c(19,20))
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(203, 206), ylim = c(19,21))
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(183.4, 183.6), ylim = c(0.1,0.3)) #Baker
plot(cred.data2$lon, cred.data2$lat, col =cred.data2$test2, xlim = c(197.5, 198.5), ylim = c(5.5,6.5)) #Palmyra and Kingman
e <- c(197.5, 198.2,5.8,6.5)
clip.rast <- crop(rast, e) 
rast.poly <- rasterToPolygons(clip.rast)
plot(rast.poly)
plot(cred.sites, add = TRUE)


plot(rast, add = TRUE)

sum.grid <- aggregate(site.id ~ test2, data = cred.data2, FUN = length)
hist(sum.grid$site.id, breaks = 60)

sum(sum.grid$site.id >2)/nrow(sum.grid)

#Number of sites that fall into a grid cell 2 or more other sites
sum(sum.grid$site.id[sum.grid$site.id>2])/sum(sum.grid$site.id)

colnames(cred.data2)[5] <- "grid.id"

#211 cells have at least 4 sites in them

