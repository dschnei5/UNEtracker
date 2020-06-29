########################################################################################################
##########################GPS Collar Handling Revisited - Ver 2#########################################
########################################################################################################
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

# 1.0 - Comes from DezAutoGPSCollarScript-WithSpeedModelling-1.2.R
#     - All errors with creating spatial variables resolved!
# 1.1 - Added a new histogram function

# 3.0 - Adapted from GPS Collar Handling 2.0
#     - Fixes timestamp issue
#     - Doesnt include LRI Modelling
#     - Next step is to incorporate gDistance and spatial metrics
# 4.0 - Fixes GPS Date and Time Problems occuring post mid 2019
#     - Simplifies and refines script - handles stuffed up data files better
#     - enables sourcing to run script standalone
#     - Creates 3D steplength 
#     - Works with R version 3.6


#################################----PART 1----#########################################
# Gets Raw Data In

####Required Libraries####

library(lubridate);
library(rgdal);

#####Add functions for later####

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

####Set WD and check for data####

message("Choose your data directory to begin...");
Sys.sleep(2);
setwd(choose.dir(default = "C:/Data5/Livestock_Tracking/GPS_Tracking_Data/", caption = "Choose location containing only raw tracking text files"));  ###Location of Raw Tracking .TXT Files###
print(paste("Working Directory Is:", getwd()));
files <- list.files(getwd(), pattern = ".txt", ignore.case = TRUE);
dname <- vector('list',2);
print(paste("There are", length(files), "collar files to process"));

Sys.sleep(2);

####Set your projection####

message("This script will use: WGS 1984 and UTM");
UCRS <- c(CRS("+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),CRS("+proj=utm +zone=51 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),CRS("+proj=utm +zone=52 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),CRS("+proj=utm +zone=53 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), CRS("+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),CRS("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), CRS("+proj=utm +zone=56 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"));
#UCRS <- c(CRS(SRS_string='EPSG:32750'),CRS(SRS_string='EPSG:32751'),CRS(SRS_string='EPSG:32752'),CRS(SRS_string='EPSG:32753'), CRS(SRS_string='EPSG:32754'),CRS(SRS_string='EPSG:32755'), CRS(SRS_string='EPSG:32756'));


con <- if (interactive()) stdin() else file('stdin');
message("Which UTM Zone is your data from");
message(paste0("Zone:","[",c(50:56),"]",sep="\n"))

UCRS.new <- scan(file=con, sep=',', nlines=1, what = 'integer', quiet=TRUE);
UCRS.new <- as.numeric(UCRS.new) -49;
UCRS.new <- UCRS[[UCRS.new]];
rm(con);
print(paste("Coordinate Reference System set to:",UCRS.new@projargs));

Sys.sleep(2);

####Read in Data Files and Process####

error.log <- data.frame(File = NA,
                        Problem = NA,
                        stringsAsFactors = FALSE);

for (i in seq(along=files)) {
  #i=3
  {dname[i] <- paste("data",i, sep = ".");
  suppressWarnings(assign(dname[[i]], read.csv(files[i], header = FALSE)));
  tmp1.df <- get(dname[[i]]);
  GGA.tmp <- subset(tmp1.df, V1=="$GPGGA");
  RMC.tmp <- subset(tmp1.df, V1=="$GPRMC");
  
  if (dim(GGA.tmp)[1] != dim(RMC.tmp)[1] | dim(RMC.tmp)[1] == 0) {
    message(paste("File", files[i], "has varying lengths of GGA and RMC records or no data, skipping file..."));
    rm(list = dname[[i]]);
    error.log[i,] <- c(files[i],"varying lengths of GGA and RMC records or no data");}
  else {
    print(paste("File",files[i], "Successfully Imported"));
    error.log[i,] <- c(files[i],"Successfully Imported")
    tmp1.df <- cbind(GGA.tmp, RMC.tmp);
    if (length(tmp1.df) != 30) {
      message(paste("File", files[i], "import error, columns not delimited correctly, skipping file..."));
      rm(list = dname[[i]]);
      error.log[i,] <- c(files[i],"import error, columns not delimited correctly, skipping file");
    } else {
      colnames(tmp1.df) <- c("GPGGA","UTC_Time","Lat","N/S1","Long","E/W1","Fix","Satellites","HDOP","Altitude","M","g.g","M","Unknown","Tag","$GPRMC","Time2","A","Latitude","N/S2","Longitude","E/W2","Speed","Course","UTC_Date","Blank","Checksum","Unk1","Unk2","Unk3");
      assign(dname[[i]],tmp1.df[,-c(1,4,6,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,30)]);
      tmp2.df <- get(dname[[i]]);
      tmp2.df$UTC_Time <- suppressWarnings(as.numeric.factor(tmp2.df$UTC_Time));
      tmp2.df$UTC_Time <- ifelse(tmp2.df$UTC_Time <= 99.99, paste0("0000",tmp2.df$UTC_Time),
                               ifelse(tmp2.df$UTC_Time <= 999.99, paste0("000",tmp2.df$UTC_Time),
                                      ifelse(tmp2.df$UTC_Time <=9999.99, paste0("00",tmp2.df$UTC_Time),
                                             ifelse(tmp2.df$UTC_Time <= 99999.99, paste0("0", tmp2.df$UTC_Time),
                                                    tmp2.df$UTC_Time))));
      #tmp2.df$UTC_Time <- as.numeric(tmp2.df$UTC_Time)
      tmp2.df$UTC_Date <- ifelse(tmp2.df$UTC_Date < 99999, paste0("0",tmp2.df$UTC_Date), tmp2.df$UTC_Date);
      tmp2.df <- cbind(tmp2.df, as.character(paste(tmp2.df$UTC_Date,tmp2.df$UTC_Time)));
      colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
      tmp2.df <- cbind(tmp2.df, dmy_hms(tmp2.df$UTC_DateTime, tz = "GMT"));
      tmp2.df <- tmp2.df[,-(9)];
      colnames(tmp2.df) <- c("UTC_Time","Lat","Long","Fix","Satellites","HDOP","Altitude","UTC_Date", "UTC_DateTime");
      tmp2.df$EST_DateTime <- with_tz(tmp2.df$UTC_DateTime,"Australia/Brisbane");
      tmp2.df$EST_Hour <- hour(tmp2.df$EST_DateTime);
      suppressWarnings(tmp2.df$SecInt <- tmp2.df$UTC_DateTime[-1] - tmp2.df$UTC_DateTime);
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
      tmp2.df$Longitude <- ifelse(tmp2.df$Longitude <= 1,NA,tmp2.df$Longitude);
      tmp2.df <- tmp2.df[!is.na(tmp2.df$Longitude),]
      tmp2.df$Latitude <- ifelse(tmp2.df$Latitude >= 1,NA,tmp2.df$Latitude);
      tmp2.df <- tmp2.df[!is.na(tmp2.df$Latitude),];
      tmp2.df$Longitude <- as.numeric(tmp2.df$Longitude);
      tmp2.df$Latitude <- as.numeric(tmp2.df$Latitude);
      coords <- SpatialPoints(data.frame(X = tmp2.df$Longitude,Y = tmp2.df$Latitude),CRS("+proj=longlat +datum=WGS84"))
      d.UTM <- spTransform(coords, UCRS.new);
      tmp2.df <- cbind(tmp2.df,d.UTM@coords);
      suppressWarnings(tmp2.df$StepLength <- sqrt((tmp2.df$X[-1] - tmp2.df$X)^2+(tmp2.df$Y[-1] - tmp2.df$Y)^2));
      tmp2.df$Speed <- tmp2.df$StepLength/as.numeric(tmp2.df$SecInt);
      tmp2.df$File <- files[i];
      tmp2.df$CollarID <- i;
      assign(dname[[i]],tmp2.df);
    }
  }
  }
  }# End of i loop

rm(tmp1.df,tmp2.df,GGA.tmp,RMC.tmp,coords,d.UTM,dname,i);

####Create Output DF####

files.out <- ls();
files.out <- files.out[grepl("data.",files.out)];
AllDataOut.df <- data.frame();

for (j in seq(along=files.out)) {
  
  tmp3.df <- get(files.out[[j]]);
  write.csv(tmp3.df,paste0(getwd(),"/", files[j],".CSV"));
  AllDataOut.df <- rbind(AllDataOut.df,get(files.out[[j]]));
}; # End j loop

write.csv(AllDataOut.df,file = paste0(getwd(),"/AllCollarsUncleaned.CSV"));
