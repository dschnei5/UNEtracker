##########################GPS Collar Handling Revisited#########################################

####Instructions:#### 

#   01 - Place raw tracking text files in a new clean folder
#   02 - Create a new folder called "Boundary" within the clean folder above and put the paddock boundary shapefile in it
#   03 - Set the coordinate system in the UCRS.new command below
#   04 - Set your preferred LRI grid size below (the smaller the size the slower the script)
#   05 - Run the script by highlighting each "Part" separately and pressing "Run" in R Studio for parts 1 and 2
#   06 - Choose the correct files when prompted
#   07 - Don't run all of part 3 until you have used the first part to customise the saga batch file
#   08 - Change the travelling threshold manually if need be after witnessing histogram (camping threshold determined autonomously)
#   09 - Run the remainder of part three by highlighting and pressing "Run"
#   10 - Locate your final output data under the original clean folder
#   11 - Don't rerun unless you've 1st deleted all script generated output or moved raw files to a new clean folder


####Version History####

#1.0 - Comes from DezAutoGPSCollarScript-WithSpeedModelling-1.2.R
#    - All errors with creating spatial variables resolved!
#1.1 - Added a new histogram function


#################################----PART 1----#########################################


####Load Required Packages and Data####

library(lubridate);
library(rgdal);
library(raster);
library(sp)
library(ggplot2)



setwd(choose.dir(caption = "Choose location containing only raw tracking text files"));  ###Location of Raw Tracking .TXT Files###
files <- list.files(getwd(), pattern = ".txt", ignore.case = TRUE);
dname <- vector('list',2);

###Set your projection and LRI grid size###
UCRS.new <- CRS("+proj=utm +zone=56 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs");
LRI.grd.size <- 10 #Grid Size in metres

for (i in seq(along=files)) {
  
  dname[i] <- paste("data",i, sep = ".");
  assign(dname[[i]], read.csv(files[i], header = FALSE, stringsAsFactors = FALSE));
  tmp1.df <- get(dname[[i]]);
  GGA.tmp <- subset(tmp1.df, V1=="$GPGGA");
  RMC.tmp <- subset(tmp1.df, V1=="$GPRMC");
  #tmp1.df <- cbind(GGA.tmp, RMC.tmp);
  tmp1.df <- base::merge(GGA.tmp,RMC.tmp, by = "V2", all.x = FALSE, all.y = FALSE)
  #colnames(tmp1.df) <- c("GPGGA","UTC_Time","Lat","N/S1","Long","E/W1","Fix","Satellites","HDOP","Altitude","M","g.g","M","Unknown","Tag","$GPRMC","Time2","A","Latitude","N/S2","Longitude","E/W2","Speed","Course","UTC_Date","Blank","Checksum","Unk1","Unk2","Unk3");
  colnames(tmp1.df) <- c("UTC_Time","GPGGA","Lat","N/S1","Long","E/W1","Fix","Satellites","HDOP","Altitude","M","g.g","M","Unknown","Tag","$GPRMC","A","Latitude","N/S2","Longitude","E/W2","Speed","Course","UTC_Date","Blank","Checksum","Unk1","Unk2","Unk3");
  #assign(dname[[i]],tmp1.df[,-c(1,4,6,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30)]);
  assign(dname[[i]],tmp1.df[,-c(2,4,6,11,12,13,14,15,16,17,18,19,20,21,22,23,25,26,27,28,29)]);
  tmp2.df <- get(dname[[i]]);
  tmp2.df$UTC_Time <- as.character(tmp2.df$UTC_Time);
  tmp2.df$UTC_Date <- as.numeric(tmp2.df$UTC_Date);
  tmp2.df$UTC_Date <- ifelse(tmp2.df$UTC_Date < 99999, paste0("0",tmp2.df$UTC_Date), tmp2.df$UTC_Date);
  tmp2.df <- cbind(tmp2.df, paste(tmp2.df$UTC_Date,tmp2.df$UTC_Time));
  colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
  tmp2.df <- cbind(tmp2.df, dmy_hms(tmp2.df$UTC_DateTime, tz = "GMT"));
  tmp2.df <- tmp2.df[,-(9)];
  colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
  tmp2.df$EST_DateTime <- with_tz(tmp2.df$UTC_DateTime,"Australia/Brisbane");
  tmp2.df$EST_Hour <- hour(tmp2.df$EST_DateTime);
  tmp2.df <- tmp2.df[order(tmp2.df$EST_DateTime),];
  tmp2.df$SecInt <- tmp2.df$UTC_DateTime[-1] - tmp2.df$UTC_DateTime;
  tmp2.df$SecInt <- as.numeric(tmp2.df$SecInt);
  tmp2.df$Long <- as.character(tmp2.df$Long);
  tmp2.df$Long <- as.numeric(tmp2.df$Long);
  tmp2.df$Lat <- as.character(tmp2.df$Lat);
  tmp2.df$Lat <- as.numeric(tmp2.df$Lat);
  tmp2.df$Long.con <- tmp2.df$Long/100;
  tmp2.df$Lat.con <- tmp2.df$Lat/100;
  tmp2.df$Long.con1 <- as.integer(tmp2.df$Long.con);
  tmp2.df$Lat.con1 <- as.integer(tmp2.df$Lat.con);
  tmp2.df$Long.con2 <- (tmp2.df$Long.con-tmp2.df$Long.con1)*100;
  tmp2.df$Lat.con2 <- (tmp2.df$Lat.con-tmp2.df$Lat.con1)*100;
  tmp2.df$Longitude <- (tmp2.df$Long.con1+(tmp2.df$Long.con2/60));
  tmp2.df$Latitude <- -(tmp2.df$Lat.con1+(tmp2.df$Lat.con2/60));
  tmp2.df <- tmp2.df[,-c(2,3,13,14,15,16,17,18)];
  tmp2.df <- tmp2.df[!is.na(tmp2.df$Longitude),]
  tmp2.df <- tmp2.df[!is.na(tmp2.df$Latitude),]
  coords <- SpatialPoints(data.frame(X = tmp2.df$Longitude,Y = tmp2.df$Latitude),CRS("+proj=longlat +datum=WGS84"))
  d.UTM <- spTransform(coords, UCRS.new);
  tmp2.df <- cbind(tmp2.df,d.UTM@coords);
  tmp2.df$StepLength <- sqrt((tmp2.df$X[-1] - tmp2.df$X)^2+(tmp2.df$Y[-1] - tmp2.df$Y)^2);
  tmp2.df$Speed <- tmp2.df$StepLength/as.numeric(tmp2.df$SecInt);
  tmp2.df$File <- files[i];
  tmp2.df$CollarID <- i;
  assign(dname[[i]],tmp2.df);
  
} #####End of i loop####

###Clean up###

rm(tmp1.df,tmp2.df,GGA.tmp,RMC.tmp,coords,d.UTM,dname,i);

###Write Individual Files to Folder###

data.out <- ls();
a <- length(files)+1
b <- length(files)+2
c <- length(files)+3
data.out <- data.out[-c]
data.out <- data.out[-b]
data.out <- data.out[-a]
rm(a,b,c)

AllDataOut.df <- data.frame();

for (j in seq(along=data.out)) {
  
  tmp3.df <- get(data.out[[j]]);
  write.csv(tmp3.df,paste0(getwd(),"/", files[j],".CSV"));
  AllDataOut.df <- rbind(AllDataOut.df,get(data.out[[j]]));
} ##End j loop##

###Write all files to one file##

write.csv(AllDataOut.df,file = paste0(getwd(),"/AllCollarsUncleaned.CSV"));

###Clean dataset by speed###

AllDataOut.df$Clip <- ifelse(AllDataOut.df$Speed > 3, NA, ifelse(AllDataOut.df$Speed<0,NA,1));
NA.row <- apply(AllDataOut.df, 1, function(x){any(is.na(x))});
sum(NA.row);
AllDataOut.df <- AllDataOut.df[!NA.row,];
rm(j,files,data.out,NA.row);

###Grab Frequency Histogram for Speed Thresholding##
#attach(AllDataOut.df)
bins <- c(0,0.0001,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,3);
bins2 <- c(0,0.0001,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2);

Speed.ms <- AllDataOut.df$Speed;
Speed.hist <- hist(Speed.ms, breaks=bins, plot=FALSE);
a <- ifelse(Speed.hist$counts == 0, 1, Speed.hist$counts)
bp <- barplot(a, log="y", col="white", names.arg=bins2);
text(bp, Speed.hist$counts, labels=Speed.hist$counts, pos=3, cex=0.5);

Speed.ms <- as.data.frame(Speed.ms)
colnames(Speed.ms) <- "Speed"
h.plot <- ggplot(Speed.ms, aes(x = Speed)) + geom_histogram(binwidth = 0.05,position = position_dodge(width = 100)) + scale_x_log10(breaks = c(0.02,0.2))
h.plot

rm(bins,bins2,Speed.hist,Speed.ms,bp,a,h.plot);

write.csv(AllDataOut.df,file = paste0(getwd(),"/AllCollarsCleaned.CSV"));
###################################----PART 2----##########################################


####Create LRIs for quadratic Plateau Behviour Model ####  

#Import Paddock Boundary Shapefile

B.Folder <- choose.dir(caption = "Folder Containing Paddock Boundary Shapefile") ##Location of Paddock Boundary Shapefile##
B.File <- list.files(B.Folder, pattern = ".shp");
B.File <- gsub(".shp", "",B.File[1]);
boundary <- readOGR(B.Folder,B.File);
plot(boundary);
##Write Grid File to Count in###

origin <- SpatialPoints(cbind(7,40),UCRS.new);
grd <- raster();
e <- extent(boundary@bbox[1,1],boundary@bbox[1,2],boundary@bbox[2,1],boundary@bbox[2,2]);
extent(grd) <- e;
res(grd) <- c(LRI.grd.size,LRI.grd.size);
projection(grd) <- UCRS.new;
grd[] <- 0

###Convert Raster Grid File to Polygons###

grd.poly <- rasterToPolygons(grd, fun=function(x) {x == 0}, dissolve=FALSE);
writeOGR(grd.poly,paste0(B.Folder,"\\Grid"),paste0("LRI_GRID_",LRI.grd.size), driver="ESRI Shapefile");
plot(grd.poly, add = TRUE);


####Export All data Points as Shapefiles clipped at various speed thresholds###

sp.in <- c(0.0,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06,0.065,0.07,0.075,0.08,0.085,0.09,0.095,0.1,0.15,0.2,0.25,0.3,0.35,0.4);

for (k in seq_along(sp.in)) {
  
  AllDataOut.df$Info <- ifelse(AllDataOut.df$Speed >= sp.in[k], NA,1);
  wow <- AllDataOut.df[is.na(AllDataOut.df$Info),];
  xy <- SpatialPoints(data.frame(X = wow$Longitude,Y = wow$Latitude),CRS("+proj=longlat +datum=WGS84"))
  d.UTM <- spTransform(xy, UCRS.new);
  pnt.df <- SpatialPointsDataFrame(d.UTM, data.frame(id=1:length(d.UTM)));
  writeOGR(pnt.df,paste0(B.Folder,"\\Output"),paste0(B.File,"_Points_",sp.in[k]), driver="ESRI Shapefile");
  
}  ##End k loop##


AllDataOut.df <- AllDataOut.df[-c(19,20)]


#################################----Part 3----#######################################

####Saga Counting Points in Polygons####

####Get Folder and File name to insert into batch file##### 

print(paste0(B.Folder,"\\Output"));###Work Directory for inside .Bat file
grd.file.loc <- (paste0(B.Folder,"\\Grid\\LRI_GRID_",LRI.grd.size,".shp")); ###Polygons file for inside .bat file
print (grd.file.loc);


####Remember to Copy/Paste the Folder and File names into the batch file and save it####

####Run Saga Count Points in Polygons####

current.wd <-getwd();
setwd(choose.dir(caption = "Select folder containing Saga Batch file after customising it")); ####Select Folder Containing the Saga Batch File after you have pasted the correct file and folder in###
shell('Command_Splitted.bat -f ');
setwd(current.wd);

####Get Max LRI's for each speed threshold####

LRI.shp <- readOGR(paste0(B.Folder,"\\Grid"),paste0("LRI_GRID_",LRI.grd.size));
files2 <- list.files(paste0(B.Folder,"\\Output"), pattern = ".shp");
LRI.max <- vector();
LRI.sum <- vector();
for (i in 1:length(files2)+1) {
  LRI.max[i] <- max(LRI.shp[[i]]);
  LRI.sum[i] <- sum(LRI.shp[[i]]);
} ##End of i loop

LRI.max <- LRI.max[-1];
LRI.sum <- LRI.sum[-1];
LRI.max <-c(LRI.max[27],LRI.max);
LRI.sum <-c(LRI.sum[27],LRI.sum);
LRI.max <- LRI.max[-28];
LRI.sum <- LRI.sum[-28];
#LRI.max <- (LRI.max/LRI.sum)*100

####Do Quadratic Plateau####

Mean <- function(x, alpha, beta, gamma) { 
  ifelse(x < -beta/(2 * gamma), alpha + beta*x + gamma*x*x, 
         alpha - beta^2/(4 * gamma)) 
} 
fm <- nls(LRI.max ~ Mean(sp.in, alpha, beta, gamma), start = list(alpha = 0.45, beta = 0.05, gamma = -0.0025));
fm;


plot(LRI.max ~ sp.in);
lines(fitted(fm) ~ sp.in);
with(as.list(coef(fm)), abline(v = -beta/(2 * gamma)));

thresh <- with(as.list(coef(fm)), (v = -beta/(2 * gamma)));
print(thresh);

thresh <- round(thresh,2)

Travel <- 0.25; #Set travelling threshold here usually 0.2 or 0.3m/s....

Speed.ms <- as.data.frame(AllDataOut.df$Speed)
colnames(Speed.ms) <- "Speed"
h.plot <- ggplot(Speed.ms, aes(x = Speed)) + geom_histogram(binwidth = 0.05,position = position_dodge(width = 100)) + scale_x_log10(breaks = c(0.005,0.05,thresh,Travel,0.5,1,2))
h.plot



####Code dataset and export - Change travelling threshold here####

AllDataOut.df$Behaviour <- ifelse(AllDataOut.df$Speed < thresh, "Camping", ifelse(AllDataOut.df$Speed > Travel, "Travelling","Grazing"));

####Write all data to CSV####

write.csv(AllDataOut.df,file = paste0(getwd(),"/AllCollars_Coded.CSV"));


####Write an all data shapefile####

xy <- SpatialPoints(data.frame(X = AllDataOut.df$Longitude,Y = AllDataOut.df$Latitude),CRS("+proj=longlat +datum=WGS84"));
xy <- spTransform(xy, UCRS.new);
AllDataOut2.df <- SpatialPointsDataFrame(xy, AllDataOut.df);
writeOGR(AllDataOut2.df,B.Folder,paste0(B.File,"_AllPoints"), driver="ESRI Shapefile");
plot(boundary);
plot(AllDataOut2.df,add=TRUE, pch = 16, col = "orange", cex = 0.5);

####Final Clean Up####

rm(list = ls());

####END AUTO SCRIPT - Thank Dez next time you see him####

