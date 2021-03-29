#created by Mary Martin and Nina Lany

#function to load Campbell datalogger file
readCDL=function(file){
  
  # read data file starting on 5th line
  setwd("C:\\Users\\Caleb\\Documents\\CMDA 2014\\drive-download-20210326T183903Z-001.zip")
  dat <- read.csv("C:\\Users\\Caleb\\Documents\\CMDA 2014\\drive-download-20210326T183903Z-001.zip", sep=",",header=FALSE,skip=4,stringsAsFactors=F)

  
  # Read in just the header line (l2)
  # unlist the line, and remove quotes 
  h <- readLines(file, n=2)[2]
  n <- as.factor(unlist(strsplit(h, ",")) )
  n2 <- gsub('"', "", n)
  
  # assign column names to dataframe
  colnames(dat) = n2
  
  return(dat)
}
well9_Precip <- read_csv("rrg19_Rg_19-2019-08-09.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="GageMinV", 
                                          X4= "ActTemp" , X5= "ActDepth", X6= "ReportPCP", 
                                          X7= "ODPCounts", X8= "blockedSec", X9=  "Scan10" , 
                                          X10= "ActDepthRA")) %>% 
  select(TIMESTAMP, RECORD,  ReportPCP) 


well3_Discharge <- read_csv("weir3_Ws_3b.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                          X5= "WLOptical_median",X6="Optical_WL_max",X7="Optical_WL_min",X8="flow_equation",
                                          X9="Q",X10="Specific_Discharge",X11="Streamtemp")) %>% 
  select(TIMESTAMP, RECORD, Specific_Discharge) %>% 
  pivot_longer(cols = c(RECORD, Specific_Discharge))

#Filter

well9_PrecipF <- filter(well9_Precip, as.Date(well9_Precip$TIMESTAMP) >= "2020-12-14")


#Plots
ggplot(data= well9_PrecipF, aes(x= TIMESTAMP, y= ReportPCP)) +
  geom_bar(stat= "identity") +
  xlab("Date")+
  ylab("Amount of Precipitation (cm)")+
  ggtitle("Precipitation Data: Well 9")+
  theme_bw()