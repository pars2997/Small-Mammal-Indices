##Set working dirrectory to location with stations file##


##Install Packages##
require(camtrapR)
require(tidyr)

##Read in Stations file, add column with site-trap combo for station, set dates##
stations<-read.csv("./Data/Stations.csv")
stations$Station<-paste(stations$Site,stations$Trap,sep="_")
stations$Set.Date<-as.Date(stations$Set.Date,format="%m/%d/%Y")
stations$Pull.Date<-as.Date(stations$Pull.Date,format="%m/%d/%Y")


##Read in small mammal camera data##
combined.df<-read.csv("./Data/SmallMamTL1617.csv")
combined.df$Date<-as.Date(combined.df$Date,format="%m/%d/%Y")
combined.df$DateTimeOriginal<-with(combined.df, as.POSIXct(paste(Date, Time),format="%Y-%m-%d %H:%M:%S"))

##Read in abundance estimates from live trapping##
densitydat<-read.csv("./Results/densityestimates.csv")

##read in Total individual data##
Capturedata<- read.csv("./Data/CaptureData.csv")




####Proportion Detected####

##Make table of camera operation times for creating detection histories##
camop<- cameraOperation (CTtable = stations, stationCol = "Station",setupCol = "Set.Date",retrievalCol = "Pull.Date", 
                         writecsv = F,hasProblems = F, dateFormat = "%Y-%m-%d")

##Create station column in combined.df in same format as in stations file##
combined.df$Station2<-paste(combined.df$Folder,combined.df$Station,sep="_")
head(combined.df)
#Set column names so they match station file
colnames(combined.df)<-c("File","Trap","Folder","Date","Time","ImageQuality","DeleteFlag","Species",
                         "Number","If_Other","Door_Closed","Notes","Noteable","DateTimeOriginal","Station")
#check that all stations in combined.df are in stations file
#Should be all trues
unique(combined.df$Station)%in%stations$Station

test<-data.frame(stations$Site,(stations$Pull.Date-stations$Set.Date))
test$X.stations.Pull.Date...stations.Set.Date.<-as.numeric(test$X.stations.Pull.Date...stations.Set.Date.)
days2<-data.frame(tapply(test$X.stations.Pull.Date...stations.Set.Date.,test$stations.Site,mean))
colnames(days2)<-"Nights"
days2$Site<-rownames(days2)
rownames(days2)<-NULL
days2<-days2[order(days2$Site),]
days2



##Create Detection Histories##
#table of 1s and 0s showing what cameras detected species on what day
#need to make one for Mice, voles and chipmunks
#Need to change the name of the object and the "species" option

##From Here to "table" commands needs to be run for each species##

DetHistMouse <- detectionHistory (recordTable = combined.df,  #table that has photo info with dates
                                  camOp = camop, #table that has dates cameras were opperated 
                                  stationCol = "Station",  #Which column gives station ID, needs to be the same in each table
                                  speciesCol = "Species",  # Which column gives the species ID
                                  recordDateTimeCol = "DateTimeOriginal",  #Which column gives the date and time of photo
                                  species = "Mouse",  #What species you are making a detection history for
                                  occasionLength = 1,  #How many days is one detection period
                                  occasionStartTime = 12,  #Offset day so the detection period switches at noon
                                  day1 = "station",  #Detection histories start on day camera is set
                                  includeEffort = F,  #If had information on survey effort would be T. Irrelevant for us
                                  timeZone = "US/Pacific",  #What timezone you're in
                                  writecsv = F,  #Whether to save a csv file
                                  outDir = getwd())  #Where to save the csv file, getwd() gives current working directory

#Turn detection history into a dataframe 
#Rename columns and add column with station names
#organize so station names are in first column
Mouse<-as.data.frame(DetHistMouse)  #Make output a dataframe, easier to work with
colnames(Mouse)<-c("O1","O2","O3","O4","O5","O6","O7","O8")  #rename columns, occasions 1-8
head(Mouse,10) #Look at the first 10 rows
Mouse$Station<-rownames(Mouse) #Add new column that is the station, was rownames before
rownames(Mouse)<-NULL  #Now that have column, can get rid of rownames
Mouse<-Mouse[,c(9,1:8)]  #reorder so that station ID is first


Mouse$Site<-stations$Site  #Add column that has site ID so we can look at site by site data



DetHistVole <- detectionHistory (recordTable = combined.df,  #table that has photo info with dates
                                 camOp = camop, #table that has dates cameras were opperated 
                                 stationCol = "Station",  #Which column gives station ID, needs to be the same in each table
                                 speciesCol = "Species",  # Which column gives the species ID
                                 recordDateTimeCol = "DateTimeOriginal",  #Which column gives the date and time of photo
                                 species = "Vole",  #What species you are making a detection history for
                                 occasionLength = 1,  #How many days is one detection period
                                 occasionStartTime = 12,  #Offset day so the detection period switches at noon
                                 day1 = "station",  #Detection histories start on day camera is set
                                 includeEffort = F,  #If had information on survey effort would be T. Irrelevant for us
                                 timeZone = "US/Pacific",  #What timezone you're in
                                 writecsv = T,  #Whether to save a csv file
                                 outDir = getwd())  #Where to save the csv file, getwd() gives current working directory

#Turn detection history into a dataframe 
#Rename columns and add column with station names
#organize so station names are in first column
Vole<-as.data.frame(DetHistVole)  #Make output a dataframe, easier to work with
colnames(Vole)<-c("O1","O2","O3","O4","O5","O6","O7","O8")  #rename columns, occasions 1-8
head(Vole,10) #Look at the first 10 rows
Vole$Station<-rownames(Vole) #Add new column that is the station, was rownames before
rownames(Vole)<-NULL  #Now that have column, can get rid of rownames
Vole<-Vole[,c(9,1:8)]  #reorder so that station ID is first

Vole$Site<-stations$Site  #Add column that has site ID so we can look at site by site data



DetHistChip <- detectionHistory (recordTable = combined.df,  #table that has photo info with dates
                                 camOp = camop, #table that has dates cameras were opperated 
                                 stationCol = "Station",  #Which column gives station ID, needs to be the same in each table
                                 speciesCol = "Species",  # Which column gives the species ID
                                 recordDateTimeCol = "DateTimeOriginal",  #Which column gives the date and time of photo
                                 species = "Chipmunk",  #What species you are making a detection history for
                                 occasionLength = 1,  #How many days is one detection period
                                 occasionStartTime = 12,  #Offset day so the detection period switches at noon
                                 day1 = "station",  #Detection histories start on day camera is set
                                 includeEffort = F,  #If had information on survey effort would be T. Irrelevant for us
                                 timeZone = "US/Pacific",  #What timezone you're in
                                 writecsv = T,  #Whether to save a csv file
                                 outDir = getwd())  #Where to save the csv file, getwd() gives current working directory

#Turn detection history into a dataframe 
#Rename columns and add column with station names
#organize so station names are in first column
Chip<-as.data.frame(DetHistChip)  #Make output a dataframe, easier to work with
colnames(Chip)<-c("O1","O2","O3","O4","O5","O6","O7","O8")  #rename columns, occasions 1-8
head(Chip,10) #Look at the first 10 rows
Chip$Station<-rownames(Chip) #Add new column that is the station, was rownames before
rownames(Chip)<-NULL  #Now that have column, can get rid of rownames
Chip<-Chip[,c(9,1:8)]  #reorder so that station ID is first

Chip$Site<-stations$Site  #Add column that has site ID so we can look at site by site data


#Calculate the proportion of camera nights where each species was detected
#Sum of nights detected divided by sum of trap nights
Mouse$rowsum<-rowSums(Mouse[,2:9],na.rm=T)
densitydat$MouseProp2<-as.numeric(tapply(Mouse$rowsum,Mouse$Site,sum)/(table(Mouse$Site)*days2$Nights))
Vole$rowsum<-rowSums(Vole[,2:9],na.rm=T)
densitydat$VoleProp2<-as.numeric(tapply(Vole$rowsum,Vole$Site,sum)/(table(Vole$Site)*days2$Nights))
Chip$rowsum<-rowSums(Chip[,2:9],na.rm=T)
densitydat$ChipProp2<-as.numeric(tapply(Chip$rowsum,Chip$Site,sum)/(table(Chip$Site)*days2$Nights))


#Create dataframe for all combined. Sum detections of each species
#Any value >0 becomes 1
proptotal<- Mouse[2:9]+Vole[2:9]+Chip[2:9]
proptotal[proptotal>0]<-1
totalrowsum<-rowSums(proptotal,na.rm=T)
totalrowsum<-data.frame(totalrowsum,Mouse$Site)
head(totalrowsum)  
densitydat$TotProp2<-as.numeric(tapply(totalrowsum$totalrowsum,totalrowsum$Mouse.Site,sum)/(table(totalrowsum$Mouse.Site)*days2$Nights))

  




####Independent Detection Events####
combined.df$Folder<-as.factor(combined.df$Folder)

vole<-combined.df[combined.df$Species=="Vole",]
chip<-combined.df[combined.df$Species=="Chipmunk",]
mouse<-combined.df[combined.df$Species=="Mouse",]

vole<-vole[order(vole$Station,vole$DateTimeOriginal),]
chip<-chip[order(chip$Station,chip$DateTimeOriginal),]
mouse<-mouse[order(mouse$Station,mouse$DateTimeOriginal),]

test<-data.frame(stations$Site,(stations$Pull.Date-stations$Set.Date))
test$X.stations.Pull.Date...stations.Set.Date.<-as.numeric(test$X.stations.Pull.Date...stations.Set.Date.)
days<-data.frame(tapply(test$X.stations.Pull.Date...stations.Set.Date.,test$stations.Site,sum))
colnames(days)<-"Nights"
days$Site<-rownames(days)
rownames(days)<-NULL
days<-days[order(days$Site),]
days
#this argument creates an empty vector to store values in
independentMouse5<-numeric(nrow(mouse)) 
independentMouse10<-numeric(nrow(mouse))
independentMouse15<-numeric(nrow(mouse))
independentMouse20<-numeric(nrow(mouse))
independentMouse25<-numeric(nrow(mouse))
independentMouse30<-numeric(nrow(mouse)) 
independentMouse45<-numeric(nrow(mouse)) 
independentMouse60<-numeric(nrow(mouse)) 
independentMouse75<-numeric(nrow(mouse)) 
independentMouse90<-numeric(nrow(mouse)) 
independentMouse105<-numeric(nrow(mouse)) 
independentMouse120<-numeric(nrow(mouse)) 
independentMouse135<-numeric(nrow(mouse))
independentMouse150<-numeric(nrow(mouse))
independentMouse165<-numeric(nrow(mouse))
independentMouse180<-numeric(nrow(mouse))
independentMouse195<-numeric(nrow(mouse))
independentMouse210<-numeric(nrow(mouse))
independentMouse225<-numeric(nrow(mouse))
independentMouse240<-numeric(nrow(mouse))
independentMouse255<-numeric(nrow(mouse))
independentMouse270<-numeric(nrow(mouse))
independentMouse285<-numeric(nrow(mouse))
independentMouse300<-numeric(nrow(mouse))
independentMouse330<-numeric(nrow(mouse))
independentMouse360<-numeric(nrow(mouse))
independentMouse390<-numeric(nrow(mouse))
independentMouse420<-numeric(nrow(mouse))

independentMouse5[1]<-1 #set the first photo to be independent
independentMouse10[1]<-1
independentMouse15[1]<-1
independentMouse20[1]<-1
independentMouse25[1]<-1
independentMouse30[1]<-1
independentMouse45[1]<-1
independentMouse60[1]<-1
independentMouse75[1]<-1
independentMouse90[1]<-1
independentMouse105[1]<-1
independentMouse120[1]<-1
independentMouse135[1]<-1
independentMouse150[1]<-1
independentMouse165[1]<-1
independentMouse180[1]<-1
independentMouse195[1]<-1
independentMouse210[1]<-1
independentMouse225[1]<-1
independentMouse240[1]<-1
independentMouse255[1]<-1
independentMouse270[1]<-1
independentMouse285[1]<-1
independentMouse300[1]<-1
independentMouse330[1]<-1
independentMouse360[1]<-1
independentMouse390[1]<-1
independentMouse420[1]<-1

#begin the for loop
for(i in 2:nrow(mouse)){#for each row in mouse
  #calculate time difference with previous row
  x<-difftime(mouse$DateTimeOriginal[i],mouse$DateTimeOriginal[i-1],units = "mins")
  #if that value is greater than 30 minutes, the photo is independent
  #this is the line you will change to test different times
  if(abs(x)>5)  independentMouse5[i]<-1
  if(abs(x)>10)  independentMouse10[i]<-1
  if(abs(x)>15)  independentMouse15[i]<-1
  if(abs(x)>20)  independentMouse20[i]<-1
  if(abs(x)>25)  independentMouse25[i]<-1
  if(abs(x)>30)  independentMouse30[i]<-1
  if(abs(x)>45)  independentMouse45[i]<-1
  if(abs(x)>60)  independentMouse60[i]<-1
  if(abs(x)>75)  independentMouse75[i]<-1
  if(abs(x)>90)  independentMouse90[i]<-1
  if(abs(x)>105)  independentMouse105[i]<-1
  if(abs(x)>120)  independentMouse120[i]<-1
  if(abs(x)>135)  independentMouse135[i]<-1
  if(abs(x)>150)  independentMouse150[i]<-1
  if(abs(x)>165)  independentMouse165[i]<-1
  if(abs(x)>180)  independentMouse180[i]<-1
  if(abs(x)>195)  independentMouse195[i]<-1
  if(abs(x)>210)  independentMouse210[i]<-1
  if(abs(x)>225)  independentMouse225[i]<-1
  if(abs(x)>240)  independentMouse240[i]<-1
  if(abs(x)>255)  independentMouse255[i]<-1
  if(abs(x)>270)  independentMouse270[i]<-1
  if(abs(x)>285)  independentMouse285[i]<-1
  if(abs(x)>300)  independentMouse300[i]<-1
  if(abs(x)>330)  independentMouse330[i]<-1
  if(abs(x)>360)  independentMouse360[i]<-1
  if(abs(x)>390)  independentMouse390[i]<-1
  if(abs(x)>420)  independentMouse420[i]<-1
}
#cbind vector of independence values to original data frame
mouseinddet<-cbind(mouse,independentMouse5,independentMouse10,independentMouse15,independentMouse20,
                  independentMouse25,independentMouse30,independentMouse45
                  ,independentMouse60,independentMouse75,independentMouse90,independentMouse105
                  ,independentMouse120,independentMouse135,independentMouse150,independentMouse165,
                  independentMouse180,independentMouse195,independentMouse210,independentMouse225,
                  independentMouse240,independentMouse255,independentMouse270,independentMouse285,
                  independentMouse300,independentMouse330,independentMouse360,independentMouse390,independentMouse420)
#sum number of independent photos

mouseinddet$allind<-rep(1,times=nrow(mouseinddet))
Mouse0<-tapply(mouseinddet$allind,mouseinddet$Folder,sum)/days$Nights
Mouse0[is.na(Mouse0)]<-0
Mouse5<-tapply(mouseinddet$independentMouse5, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse5[is.na(Mouse5)]<-0
Mouse10<-tapply(mouseinddet$independentMouse10, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse10[is.na(Mouse10)]<-0
Mouse15<-tapply(mouseinddet$independentMouse15, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse15[is.na(Mouse15)]<-0
Mouse20<-tapply(mouseinddet$independentMouse20, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse20[is.na(Mouse20)]<-0
Mouse25<-tapply(mouseinddet$independentMouse25, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse25[is.na(Mouse25)]<-0
Mouse30<-tapply(mouseinddet$independentMouse30, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse30[is.na(Mouse30)]<-0
Mouse45<-tapply(mouseinddet$independentMouse45, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse45[is.na(Mouse45)]<-0
Mouse60<-tapply(mouseinddet$independentMouse60, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse60[is.na(Mouse60)]<-0
Mouse75<-tapply(mouseinddet$independentMouse75, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse75[is.na(Mouse75)]<-0
Mouse90<-tapply(mouseinddet$independentMouse90, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse90[is.na(Mouse90)]<-0
Mouse105<-tapply(mouseinddet$independentMouse105, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse105[is.na(Mouse105)]<-0
Mouse120<-tapply(mouseinddet$independentMouse120, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse120[is.na(Mouse120)]<-0
Mouse135<-tapply(mouseinddet$independentMouse135, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse135[is.na(Mouse135)]<-0
Mouse150<-tapply(mouseinddet$independentMouse150, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse150[is.na(Mouse150)]<-0
Mouse165<-tapply(mouseinddet$independentMouse165, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse165[is.na(Mouse165)]<-0
Mouse180<-tapply(mouseinddet$independentMouse180, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse180[is.na(Mouse180)]<-0
Mouse195<-tapply(mouseinddet$independentMouse195, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse195[is.na(Mouse195)]<-0
Mouse210<-tapply(mouseinddet$independentMouse210, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse210[is.na(Mouse210)]<-0
Mouse225<-tapply(mouseinddet$independentMouse225, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse225[is.na(Mouse225)]<-0
Mouse240<-tapply(mouseinddet$independentMouse240, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse240[is.na(Mouse240)]<-0
Mouse255<-tapply(mouseinddet$independentMouse255, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse255[is.na(Mouse255)]<-0
Mouse270<-tapply(mouseinddet$independentMouse270, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse270[is.na(Mouse270)]<-0
Mouse285<-tapply(mouseinddet$independentMouse285, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse285[is.na(Mouse285)]<-0
Mouse300<-tapply(mouseinddet$independentMouse300, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse300[is.na(Mouse300)]<-0
Mouse330<-tapply(mouseinddet$independentMouse330, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse330[is.na(Mouse330)]<-0
Mouse360<-tapply(mouseinddet$independentMouse360, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse360[is.na(Mouse360)]<-0
Mouse390<-tapply(mouseinddet$independentMouse390, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse390[is.na(Mouse390)]<-0
Mouse420<-tapply(mouseinddet$independentMouse420, mouseinddet$Folder, FUN=sum)/days$Nights
Mouse420[is.na(Mouse420)]<-0
MDens<-cbind(Mouse0,Mouse5,Mouse10,Mouse15,Mouse20,Mouse25,Mouse30,Mouse45,Mouse60,Mouse75,Mouse90,Mouse105,Mouse120,
             Mouse135,Mouse150,Mouse165,Mouse180,Mouse195,Mouse210,Mouse225,Mouse240,Mouse255,
             Mouse270,Mouse285,Mouse300,Mouse330,Mouse360,Mouse390,Mouse420)

MDens<-round(MDens,2)

#this argument creates an empty vector to store values in
independentvole5<-numeric(nrow(vole))
independentvole10<-numeric(nrow(vole))
independentvole15<-numeric(nrow(vole))
independentvole20<-numeric(nrow(vole))
independentvole25<-numeric(nrow(vole))
independentvole30<-numeric(nrow(vole)) 
independentvole45<-numeric(nrow(vole)) 
independentvole60<-numeric(nrow(vole)) 
independentvole75<-numeric(nrow(vole)) 
independentvole90<-numeric(nrow(vole)) 
independentvole105<-numeric(nrow(vole)) 
independentvole120<-numeric(nrow(vole)) 
independentvole135<-numeric(nrow(vole))
independentvole150<-numeric(nrow(vole))
independentvole165<-numeric(nrow(vole))
independentvole180<-numeric(nrow(vole))
independentvole195<-numeric(nrow(vole))
independentvole210<-numeric(nrow(vole))
independentvole225<-numeric(nrow(vole))
independentvole240<-numeric(nrow(vole))
independentvole255<-numeric(nrow(vole))
independentvole270<-numeric(nrow(vole))
independentvole285<-numeric(nrow(vole))
independentvole300<-numeric(nrow(vole))
independentvole330<-numeric(nrow(vole))
independentvole360<-numeric(nrow(vole))
independentvole390<-numeric(nrow(vole))
independentvole420<-numeric(nrow(vole))

independentvole5[1]<-1#set the first photo to be independent
independentvole10[1]<-1
independentvole15[1]<-1
independentvole20[1]<-1
independentvole25[1]<-1
independentvole30[1]<-1
independentvole45[1]<-1
independentvole60[1]<-1
independentvole75[1]<-1
independentvole90[1]<-1
independentvole105[1]<-1
independentvole120[1]<-1
independentvole135[1]<-1
independentvole150[1]<-1
independentvole165[1]<-1
independentvole180[1]<-1
independentvole195[1]<-1
independentvole210[1]<-1
independentvole225[1]<-1
independentvole240[1]<-1
independentvole255[1]<-1
independentvole270[1]<-1
independentvole285[1]<-1
independentvole300[1]<-1
independentvole330[1]<-1
independentvole360[1]<-1
independentvole390[1]<-1
independentvole420[1]<-1

#begin the for loop
for(i in 2:nrow(vole)){#for each row in VoleI
  #calculate time difference with previous row
  x<-difftime(vole$DateTimeOriginal[i],vole$DateTimeOriginal[i-1],units = "mins")
  #if that value is greater than 30 minutes, the photo is independent
  #this is the line you will change to test different times
  if(abs(x)>5)  independentvole5[i]<-1
  if(abs(x)>10)  independentvole10[i]<-1
  if(abs(x)>15)  independentvole15[i]<-1
  if(abs(x)>20)  independentvole20[i]<-1
  if(abs(x)>25)  independentvole25[i]<-1
  if(abs(x)>30)  independentvole30[i]<-1
  if(abs(x)>45)  independentvole45[i]<-1
  if(abs(x)>60)  independentvole60[i]<-1
  if(abs(x)>75)  independentvole75[i]<-1
  if(abs(x)>90)  independentvole90[i]<-1
  if(abs(x)>105)  independentvole105[i]<-1
  if(abs(x)>120)  independentvole120[i]<-1
  if(abs(x)>135)  independentvole135[i]<-1
  if(abs(x)>150)  independentvole150[i]<-1
  if(abs(x)>165)  independentvole165[i]<-1
  if(abs(x)>180)  independentvole180[i]<-1
  if(abs(x)>195)  independentvole195[i]<-1
  if(abs(x)>210)  independentvole210[i]<-1
  if(abs(x)>225)  independentvole225[i]<-1
  if(abs(x)>240)  independentvole240[i]<-1
  if(abs(x)>255)  independentvole255[i]<-1
  if(abs(x)>270)  independentvole270[i]<-1
  if(abs(x)>285)  independentvole285[i]<-1
  if(abs(x)>300)  independentvole300[i]<-1
  if(abs(x)>330)  independentvole330[i]<-1
  if(abs(x)>360)  independentvole360[i]<-1
  if(abs(x)>390)  independentvole390[i]<-1
  if(abs(x)>420)  independentvole420[i]<-1
}
#cbind vector of independence values to original data frame
voleinddet<-cbind(vole,independentvole5,independentvole10,independentvole15,independentvole20,
                 independentvole25,independentvole30,independentvole45
                 ,independentvole60,independentvole75,independentvole90,independentvole105
                 ,independentvole120,independentvole135,independentvole150,independentvole165,
                 independentvole180,independentvole195,independentvole210,independentvole225,
                 independentvole240,independentvole255,independentvole270,independentvole285,
                 independentvole300,independentvole330,independentvole360,independentvole390,independentvole420)
#sum number of independent photos

voleinddet$allind<-rep(1,times=nrow(voleinddet))
Vole0<-tapply(voleinddet$allind,voleinddet$Folder,sum)/days$Nights
Vole0[is.na(Vole0)]<-0
Vole5<-tapply(voleinddet$independentvole5, voleinddet$Folder, FUN=sum)/days$Nights
Vole5[is.na(Vole5)]<-0
Vole10<-tapply(voleinddet$independentvole10, voleinddet$Folder, FUN=sum)/days$Nights
Vole10[is.na(Vole10)]<-0
Vole15<-tapply(voleinddet$independentvole15, voleinddet$Folder, FUN=sum)/days$Nights
Vole15[is.na(Vole15)]<-0
Vole20<-tapply(voleinddet$independentvole20, voleinddet$Folder, FUN=sum)/days$Nights
Vole20[is.na(Vole20)]<-0
Vole25<-tapply(voleinddet$independentvole25, voleinddet$Folder, FUN=sum)/days$Nights
Vole25[is.na(Vole25)]<-0
Vole30<-tapply(voleinddet$independentvole30, voleinddet$Folder, FUN=sum)/days$Nights
Vole30[is.na(Vole30)]<-0
Vole45<-tapply(voleinddet$independentvole45, voleinddet$Folder, FUN=sum)/days$Nights
Vole45[is.na(Vole45)]<-0
Vole60<-tapply(voleinddet$independentvole60, voleinddet$Folder, FUN=sum)/days$Nights
Vole60[is.na(Vole60)]<-0
Vole75<-tapply(voleinddet$independentvole75, voleinddet$Folder, FUN=sum)/days$Nights
Vole75[is.na(Vole75)]<-0
Vole90<-tapply(voleinddet$independentvole90, voleinddet$Folder, FUN=sum)/days$Nights
Vole90[is.na(Vole90)]<-0
Vole105<-tapply(voleinddet$independentvole105, voleinddet$Folder, FUN=sum)/days$Nights
Vole105[is.na(Vole105)]<-0
Vole120<-tapply(voleinddet$independentvole120, voleinddet$Folder, FUN=sum)/days$Nights
Vole120[is.na(Vole120)]<-0
Vole135<-tapply(voleinddet$independentvole135, voleinddet$Folder, FUN=sum)/days$Nights
Vole135[is.na(Vole135)]<-0
Vole150<-tapply(voleinddet$independentvole150, voleinddet$Folder, FUN=sum)/days$Nights
Vole150[is.na(Vole150)]<-0
Vole165<-tapply(voleinddet$independentvole165, voleinddet$Folder, FUN=sum)/days$Nights
Vole165[is.na(Vole165)]<-0
Vole180<-tapply(voleinddet$independentvole180, voleinddet$Folder, FUN=sum)/days$Nights
Vole180[is.na(Vole180)]<-0
Vole195<-tapply(voleinddet$independentvole195, voleinddet$Folder, FUN=sum)/days$Nights
Vole195[is.na(Vole195)]<-0
Vole210<-tapply(voleinddet$independentvole210, voleinddet$Folder, FUN=sum)/days$Nights
Vole210[is.na(Vole210)]<-0
Vole225<-tapply(voleinddet$independentvole225, voleinddet$Folder, FUN=sum)/days$Nights
Vole225[is.na(Vole225)]<-0
Vole240<-tapply(voleinddet$independentvole240, voleinddet$Folder, FUN=sum)/days$Nights
Vole240[is.na(Vole240)]<-0
Vole255<-tapply(voleinddet$independentvole255, voleinddet$Folder, FUN=sum)/days$Nights
Vole255[is.na(Vole255)]<-0
Vole270<-tapply(voleinddet$independentvole270, voleinddet$Folder, FUN=sum)/days$Nights
Vole270[is.na(Vole270)]<-0
Vole285<-tapply(voleinddet$independentvole285, voleinddet$Folder, FUN=sum)/days$Nights
Vole285[is.na(Vole285)]<-0
Vole300<-tapply(voleinddet$independentvole300, voleinddet$Folder, FUN=sum)/days$Nights
Vole300[is.na(Vole300)]<-0
Vole330<-tapply(voleinddet$independentvole330, voleinddet$Folder, FUN=sum)/days$Nights
Vole330[is.na(Vole330)]<-0
Vole360<-tapply(voleinddet$independentvole360, voleinddet$Folder, FUN=sum)/days$Nights
Vole360[is.na(Vole360)]<-0
Vole390<-tapply(voleinddet$independentvole390, voleinddet$Folder, FUN=sum)/days$Nights
Vole390[is.na(Vole390)]<-0
Vole420<-tapply(voleinddet$independentvole420, voleinddet$Folder, FUN=sum)/days$Nights
Vole420[is.na(Vole420)]<-0
VDens<-cbind(Vole0,Vole5,Vole10,Vole15,Vole20,Vole25,Vole30,Vole45,Vole60,Vole75,Vole90,Vole105,Vole120,
             Vole135,Vole150,Vole165,Vole180,Vole195,Vole210,Vole225,Vole240,Vole255,
             Vole270,Vole285,Vole300,Vole330,Vole360,Vole390,Vole420)
VDens<-round(VDens,2)


#this argument creates an empty vector to store values in
independentChip5<-numeric(nrow(chip))
independentChip10<-numeric(nrow(chip))
independentChip15<-numeric(nrow(chip))
independentChip20<-numeric(nrow(chip))
independentChip25<-numeric(nrow(chip))
independentChip30<-numeric(nrow(chip)) 
independentChip45<-numeric(nrow(chip)) 
independentChip60<-numeric(nrow(chip)) 
independentChip75<-numeric(nrow(chip)) 
independentChip90<-numeric(nrow(chip)) 
independentChip105<-numeric(nrow(chip)) 
independentChip120<-numeric(nrow(chip)) 
independentChip135<-numeric(nrow(chip))
independentChip150<-numeric(nrow(chip))
independentChip165<-numeric(nrow(chip))
independentChip180<-numeric(nrow(chip))
independentChip195<-numeric(nrow(chip))
independentChip210<-numeric(nrow(chip))
independentChip225<-numeric(nrow(chip))
independentChip240<-numeric(nrow(chip))
independentChip255<-numeric(nrow(chip))
independentChip270<-numeric(nrow(chip))
independentChip285<-numeric(nrow(chip))
independentChip300<-numeric(nrow(chip))
independentChip330<-numeric(nrow(chip))
independentChip360<-numeric(nrow(chip))
independentChip390<-numeric(nrow(chip))
independentChip420<-numeric(nrow(chip))

independentChip5[1]<-1 #set the first photo to be independent
independentChip10[1]<-1
independentChip15[1]<-1
independentChip20[1]<-1
independentChip25[1]<-1
independentChip30[1]<-1
independentChip45[1]<-1
independentChip60[1]<-1
independentChip75[1]<-1
independentChip90[1]<-1
independentChip105[1]<-1
independentChip120[1]<-1
independentChip135[1]<-1
independentChip150[1]<-1
independentChip165[1]<-1
independentChip180[1]<-1
independentChip195[1]<-1
independentChip210[1]<-1
independentChip225[1]<-1
independentChip240[1]<-1
independentChip255[1]<-1
independentChip270[1]<-1
independentChip285[1]<-1
independentChip300[1]<-1
independentChip330[1]<-1
independentChip360[1]<-1
independentChip390[1]<-1
independentChip420[1]<-1

#begin the for loop
for(i in 2:nrow(chip)){#for each row in ChipI
  #calculate time difference with previous row
  x<-difftime(chip$DateTimeOriginal[i],chip$DateTimeOriginal[i-1],units = "mins")
  #if that value is greater than 30 minutes, the photo is independent
  #this is the line you will change to test different times
  if(abs(x)>5)  independentChip5[i]<-1
  if(abs(x)>10)  independentChip10[i]<-1
  if(abs(x)>15)  independentChip15[i]<-1
  if(abs(x)>20)  independentChip20[i]<-1
  if(abs(x)>25)  independentChip25[i]<-1
  if(abs(x)>30)  independentChip30[i]<-1
  if(abs(x)>45)  independentChip45[i]<-1
  if(abs(x)>60)  independentChip60[i]<-1
  if(abs(x)>75)  independentChip75[i]<-1
  if(abs(x)>90)  independentChip90[i]<-1
  if(abs(x)>105)  independentChip105[i]<-1
  if(abs(x)>120)  independentChip120[i]<-1
  if(abs(x)>135)  independentChip135[i]<-1
  if(abs(x)>150)  independentChip150[i]<-1
  if(abs(x)>165)  independentChip165[i]<-1
  if(abs(x)>180)  independentChip180[i]<-1
  if(abs(x)>195)  independentChip195[i]<-1
  if(abs(x)>210)  independentChip210[i]<-1
  if(abs(x)>225)  independentChip225[i]<-1
  if(abs(x)>240)  independentChip240[i]<-1
  if(abs(x)>255)  independentChip255[i]<-1
  if(abs(x)>270)  independentChip270[i]<-1
  if(abs(x)>285)  independentChip285[i]<-1
  if(abs(x)>300)  independentChip300[i]<-1
  if(abs(x)>330)  independentChip330[i]<-1
  if(abs(x)>360)  independentChip360[i]<-1
  if(abs(x)>390)  independentChip390[i]<-1
  if(abs(x)>420)  independentChip420[i]<-1
}
#cbind vector of independence values to original data frame
chipindet<-cbind(chip,independentChip5,independentChip10,independentChip15,independentChip20,
                 independentChip25,independentChip30,independentChip45
                 ,independentChip60,independentChip75,independentChip90,independentChip105
                 ,independentChip120,independentChip135,independentChip150,independentChip165,
                 independentChip180,independentChip195,independentChip210,independentChip225,
                 independentChip240,independentChip255,independentChip270,independentChip285,
                 independentChip300,independentChip330,independentChip360,independentChip390,independentChip420)
#sum number of independent photos

chipindet$allind<-rep(1,times=nrow(chipindet))
Chip0<-tapply(chipindet$allind,chipindet$Folder,sum)/days$Nights
Chip0[is.na(Chip0)]<-0
Chip5<-tapply(chipindet$independentChip5, chipindet$Folder, FUN=sum)/days$Nights
Chip5[is.na(Chip5)]<-0
Chip10<-tapply(chipindet$independentChip10, chipindet$Folder, FUN=sum)/days$Nights
Chip10[is.na(Chip10)]<-0
Chip15<-tapply(chipindet$independentChip15, chipindet$Folder, FUN=sum)/days$Nights
Chip15[is.na(Chip15)]<-0
Chip20<-tapply(chipindet$independentChip20, chipindet$Folder, FUN=sum)/days$Nights
Chip20[is.na(Chip20)]<-0
Chip25<-tapply(chipindet$independentChip25, chipindet$Folder, FUN=sum)/days$Nights
Chip25[is.na(Chip25)]<-0
Chip30<-tapply(chipindet$independentChip30, chipindet$Folder, FUN=sum)/days$Nights
Chip30[is.na(Chip30)]<-0
Chip45<-tapply(chipindet$independentChip45, chipindet$Folder, FUN=sum)/days$Nights
Chip45[is.na(Chip45)]<-0
Chip60<-tapply(chipindet$independentChip60, chipindet$Folder, FUN=sum)/days$Nights
Chip60[is.na(Chip60)]<-0
Chip75<-tapply(chipindet$independentChip75, chipindet$Folder, FUN=sum)/days$Nights
Chip75[is.na(Chip75)]<-0
Chip90<-tapply(chipindet$independentChip90, chipindet$Folder, FUN=sum)/days$Nights
Chip90[is.na(Chip90)]<-0
Chip105<-tapply(chipindet$independentChip105, chipindet$Folder, FUN=sum)/days$Nights
Chip105[is.na(Chip105)]<-0
Chip120<-tapply(chipindet$independentChip120, chipindet$Folder, FUN=sum)/days$Nights
Chip120[is.na(Chip120)]<-0
Chip135<-tapply(chipindet$independentChip135, chipindet$Folder, FUN=sum)/days$Nights
Chip135[is.na(Chip135)]<-0
Chip150<-tapply(chipindet$independentChip150, chipindet$Folder, FUN=sum)/days$Nights
Chip150[is.na(Chip150)]<-0
Chip165<-tapply(chipindet$independentChip165, chipindet$Folder, FUN=sum)/days$Nights
Chip165[is.na(Chip165)]<-0
Chip180<-tapply(chipindet$independentChip180, chipindet$Folder, FUN=sum)/days$Nights
Chip180[is.na(Chip180)]<-0
Chip195<-tapply(chipindet$independentChip195, chipindet$Folder, FUN=sum)/days$Nights
Chip195[is.na(Chip195)]<-0
Chip210<-tapply(chipindet$independentChip210, chipindet$Folder, FUN=sum)/days$Nights
Chip210[is.na(Chip210)]<-0
Chip225<-tapply(chipindet$independentChip225, chipindet$Folder, FUN=sum)/days$Nights
Chip225[is.na(Chip225)]<-0
Chip240<-tapply(chipindet$independentChip240, chipindet$Folder, FUN=sum)/days$Nights
Chip240[is.na(Chip240)]<-0
Chip255<-tapply(chipindet$independentChip255, chipindet$Folder, FUN=sum)/days$Nights
Chip255[is.na(Chip255)]<-0
Chip270<-tapply(chipindet$independentChip270, chipindet$Folder, FUN=sum)/days$Nights
Chip270[is.na(Chip270)]<-0
Chip285<-tapply(chipindet$independentChip285, chipindet$Folder, FUN=sum)/days$Nights
Chip285[is.na(Chip285)]<-0
Chip300<-tapply(chipindet$independentChip300, chipindet$Folder, FUN=sum)/days$Nights
Chip300[is.na(Chip300)]<-0
Chip330<-tapply(chipindet$independentChip330, chipindet$Folder, FUN=sum)/days$Nights
Chip330[is.na(Chip330)]<-0
Chip360<-tapply(chipindet$independentChip360, chipindet$Folder, FUN=sum)/days$Nights
Chip360[is.na(Chip360)]<-0
Chip390<-tapply(chipindet$independentChip390, chipindet$Folder, FUN=sum)/days$Nights
Chip390[is.na(Chip390)]<-0
Chip420<-tapply(chipindet$independentChip420, chipindet$Folder, FUN=sum)/days$Nights
Chip420[is.na(Chip420)]<-0
CDens<-cbind(Chip0,Chip5,Chip10,Chip15,Chip20,Chip25,Chip30,Chip45,Chip60,Chip75,Chip90,Chip105,Chip120,
             Chip135,Chip150,Chip165,Chip180,Chip195,Chip210,Chip225,Chip240,Chip255,
             Chip270,Chip285,Chip300,Chip330,Chip360,Chip390,Chip420)
CDens<-round(CDens,2)


TDens<-MDens+VDens+CDens

MDens<-data.frame(MDens)
VDens<-data.frame(VDens)
CDens<-data.frame(CDens)
TDens<-data.frame(TDens)

#calculate correlation between estimated mouse density and number of independent events at each time
mcor<-data.frame(cor(densitydat$mouse_den,MDens))
mcor
mcor[which(mcor==max(mcor))]

densitydat$MouseInd<-MDens$Mouse5

##VoleDensity##
vcor<-data.frame(cor(densitydat$vole_den,VDens))
vcor
vcor[which(vcor==max(vcor))]

densitydat$VoleInd<-VDens$Vole0

##ChipDensity##
ccor<-data.frame(cor(densitydat$chip_den,CDens))
ccor
ccor[which(ccor==max(ccor))]

densitydat$ChipInd<-CDens$Chip5

##ALL##
tcor<-data.frame(cor(densitydat$total_den,TDens))
tcor
tcor[which(tcor==max(tcor))]

densitydat$TotInd<-TDens$Mouse20

TimeInc<-c(0,5,10,15,20,25,seq(30,300,15),330,360,390,420)


par(mar=c(7,7,3,2))
par(mfrow=c(2,2))
plot(TimeInc,mcor, xlab = "Time increment (min)" , ylab = "Density correlation \n for mice",type = "b",bty="l",
     cex.lab=2,cex.axis=1.5,pch=19,ylim = c(0.45,1),yaxt="n")
axis(side = 2,at = seq(0.5,1.0,0.1),labels=c("0.5","0.6","0.7","0.8","0.9","1.0"),cex.lab=2,cex.axis=1.5,las=2)
text(x=0,y=1,labels = "a",cex=1.5)
plot(TimeInc,vcor,type = "b",pch=19,xlab="Time increment (min)",ylab="Density correlation \n for voles",
     cex.axis=1.5,cex.lab=2,bty="l",ylim = c(0.45,1),yaxt="n")
axis(side = 2,at = seq(0.5,1.0,0.1),labels=c("0.5","0.6","0.7","0.8","0.9","1.0"),cex.lab=2,cex.axis=1.5,las=2)

text(x=0,y=1,labels = "b",cex=1.5)
plot(TimeInc,ccor,type = "b",pch=19,cex.lab=2,cex.axis=1.5,xlab="Time increment (min)",
     ylab="Density correlation \n for chipmunks",bty="l",ylim = c(0.45,1),yaxt="n")
axis(side = 2,at = seq(0.5,1.0,0.1),labels=c("0.5","0.6","0.7","0.8","0.9","1.0"),cex.lab=2,cex.axis=1.5,las=2)

text(x=0,y=1,labels = "c",cex=1.5)
plot(TimeInc,tcor,type = "b",bty="l",xlab="Time increment (min)",ylab = "Density correltaion \n for all species",
     cex.lab=2,cex.axis=1.5,pch=19,ylim = c(0.45,1),yaxt="n")
axis(side = 2,at = seq(0.5,1.0,0.1),labels=c("0.5","0.6","0.7","0.8","0.9","1.0"),cex.lab=2,cex.axis=1.5,las=2)
text(x=0,y=1,labels = "d",cex=1.5)

####Minimum Number Alive####
#Input capture data and format as a row for each individual with its capture history, site, and species
head(Capturedata) #look at the data
Capturedata$Site<-as.factor(Capturedata$Site)
Capturedata$Species<-as.factor(Capturedata$Species)
Capturedata$Tag<-NULL  #get rid of tag names
Capturedata<-Capturedata[,c(2,1,3:9)]  #reorder so that station ID is first
head(Capturedata)
#Convert no data to zeros
#No data are from the one site with only 5 capture ocassions
Capturedata$Captured.Session.6<-as.character(Capturedata$Captured.Session.6)
Capturedata$Captured.Session.7<-as.character(Capturedata$Captured.Session.7)
Capturedata[Capturedata=="."]<-0
Capturedata$Captured.Session.6<-as.numeric(Capturedata$Captured.Session.6)
Capturedata$Captured.Session.7<-as.numeric(Capturedata$Captured.Session.7)

#Split into cumulative data after each event
CapDat1<-Capturedata[,1:3]
CapDat2<-Capturedata[,1:4]
CapDat3<-Capturedata[,1:5]
CapDat4<-Capturedata[,1:6]
CapDat5<-Capturedata[,1:7]
CapDat6<-Capturedata[,1:8]
CapDat7<-Capturedata[,1:9]


#Limit cumulative data to only those individuals that have been captured
CapDat1<-CapDat1[CapDat1$Captured.Session.1!=0,]
CapDat2<-CapDat2[which(rowSums(CapDat2[,-c(1,2)])>0),]
CapDat3<-CapDat3[which(rowSums(CapDat3[,-c(1,2)])>0),]
CapDat4<-CapDat4[which(rowSums(CapDat4[,-c(1,2)])>0),]
CapDat5<-CapDat5[which(rowSums(CapDat5[,-c(1,2)])>0),]
CapDat6<-CapDat6[which(rowSums(CapDat6[,-c(1,2)])>0),]
CapDat7<-CapDat7[which(rowSums(CapDat7[,-c(1,2)])>0),]

#Create a list for looping
CapDatAll<-list(CapDat1,CapDat2,CapDat3,CapDat4,CapDat5,CapDat6,CapDat7)
testing<-list(length=7)

#This loop sums the number of indiviudals of each species captured at each site
for(i in 1:length(CapDatAll)){
  SUMT_i<-tapply(CapDatAll[[i]]$Species,CapDatAll[[i]]$Site,FUN=table)
  SUMT_i<-as.list(SUMT_i)
  temp_i<-as.numeric(unlist(SUMT_i))
  temp_ii<-as.data.frame(matrix(data=temp_i,nrow=21,3,byrow=T))
  temp_ii$Site<-names(SUMT_i)
  #This line selects only those sites where cameras were also used
  temp_iii<-temp_ii[c(3,4,5,6,8,9,10,11,12,13,14,17,18,19,21),]
  testing[[i]]<-temp_iii
  colnames(testing[[i]])<-c("Chip.MNA","Mouse.MNA","Vole.MNA","Site")
  testing[[i]]$Total.MNA<-testing[[i]]$Mouse.MNA+testing[[i]]$Chip.MNA+testing[[i]]$Vole.MNA
}
densitydat$mouseMNA_1 <- testing[[1]]$Mouse.MNA
densitydat$mouseMNA_2 <- testing[[2]]$Mouse.MNA
densitydat$mouseMNA_3 <- testing[[3]]$Mouse.MNA
densitydat$mouseMNA_4 <- testing[[4]]$Mouse.MNA
densitydat$mouseMNA_5 <- testing[[5]]$Mouse.MNA
densitydat$mouseMNA_6 <- testing[[6]]$Mouse.MNA
densitydat$mouseMNA_7 <- testing[[7]]$Mouse.MNA
densitydat$voleMNA_1 <- testing[[1]]$Vole.MNA
densitydat$voleMNA_2 <- testing[[2]]$Vole.MNA
densitydat$voleMNA_3 <- testing[[3]]$Vole.MNA
densitydat$voleMNA_4 <- testing[[4]]$Vole.MNA
densitydat$voleMNA_5 <- testing[[5]]$Vole.MNA
densitydat$voleMNA_6 <- testing[[6]]$Vole.MNA
densitydat$voleMNA_7 <- testing[[7]]$Vole.MNA
densitydat$chipMNA_1 <- testing[[1]]$Chip.MNA
densitydat$chipMNA_2 <- testing[[2]]$Chip.MNA
densitydat$chipMNA_3 <- testing[[3]]$Chip.MNA
densitydat$chipMNA_4 <- testing[[4]]$Chip.MNA
densitydat$chipMNA_5 <- testing[[5]]$Chip.MNA
densitydat$chipMNA_6 <- testing[[6]]$Chip.MNA
densitydat$chipMNA_7 <- testing[[7]]$Chip.MNA
densitydat$totMNA_1 <- testing[[1]]$Total.MNA
densitydat$totMNA_2 <- testing[[2]]$Total.MNA
densitydat$totMNA_3 <- testing[[3]]$Total.MNA
densitydat$totMNA_4 <- testing[[4]]$Total.MNA
densitydat$totMNA_5 <- testing[[5]]$Total.MNA
densitydat$totMNA_6 <- testing[[6]]$Total.MNA
densitydat$totMNA_7 <- testing[[7]]$Total.MNA

####Captures per 100 trapping nights####
CapDat100<-Capturedata

#Sum how many times each individual was captured after each ocassion
CapDat100$Through2<-rowSums(CapDat100[,3:4])
CapDat100$Through3<-rowSums(CapDat100[,3:5])
CapDat100$Through4<-rowSums(CapDat100[,3:6])
CapDat100$Through5<-rowSums(CapDat100[,3:7])
CapDat100$Through6<-rowSums(CapDat100[,3:8])
CapDat100$Through7<-rowSums(CapDat100[,3:9])

#Sum captures by species and site
TotalCapsList<-list(
as.data.frame(tapply(CapDat100$Captured.Session.1,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through2,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through3,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through4,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through5,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through6,list(CapDat100$Site,CapDat100$Species),sum)),
as.data.frame(tapply(CapDat100$Through7,list(CapDat100$Site,CapDat100$Species),sum)))

#Turn NAs to zeros
TotalCapsList[[1]][is.na(TotalCapsList[[1]])]<-0
TotalCapsList[[2]][is.na(TotalCapsList[[2]])]<-0
TotalCapsList[[3]][is.na(TotalCapsList[[3]])]<-0
TotalCapsList[[4]][is.na(TotalCapsList[[4]])]<-0
TotalCapsList[[5]][is.na(TotalCapsList[[5]])]<-0
TotalCapsList[[6]][is.na(TotalCapsList[[6]])]<-0
TotalCapsList[[7]][is.na(TotalCapsList[[7]])]<-0

#Select only the sites with cameras and sum total captures across species
for(i in 1:length(TotalCapsList)){
  TotalCapsList[[i]]<-TotalCapsList[[i]][c(3,4,5,6,8,9,10,11,12,13,14,17,18,19,21),]
  TotalCapsList[[i]]$Tot.Caps<-TotalCapsList[[i]]$Chip+TotalCapsList[[i]]$Mouse+TotalCapsList[[i]]$Vole
}


#Create list of number of trap nights, accounting for the site that stopped at 5 ocassions
hundrednights<-list(rep(1,15),
                    rep(2,15),
                    rep(3,15),
                    rep(4,15),
                    rep(5,15),
                    c(6,6,6,6,6,6,5,6,6,6,6,6,6,6,6),
                    c(7,7,7,7,7,7,5,7,7,7,7,7,7,7,7))

for(i in 1:length(TotalCapsList)){
  TotalCapsList[[i]]$Nights<-hundrednights[[i]]
}

TotalCapsList

TotalCapsList[[7]]/TotalCapsList[[7]]$Nights

densitydat$mouseCPUE_1 <- TotalCapsList[[1]]$Mouse/TotalCapsList[[1]]$Nights
densitydat$mouseCPUE_2 <- TotalCapsList[[2]]$Mouse/TotalCapsList[[2]]$Nights
densitydat$mouseCPUE_3 <- TotalCapsList[[3]]$Mouse/TotalCapsList[[3]]$Nights
densitydat$mouseCPUE_4 <- TotalCapsList[[4]]$Mouse/TotalCapsList[[4]]$Nights
densitydat$mouseCPUE_5 <- TotalCapsList[[5]]$Mouse/TotalCapsList[[5]]$Nights
densitydat$mouseCPUE_6 <- TotalCapsList[[6]]$Mouse/TotalCapsList[[6]]$Nights
densitydat$mouseCPUE_7 <- TotalCapsList[[7]]$Mouse/TotalCapsList[[7]]$Nights


densitydat$voleCPUE_1 <- TotalCapsList[[1]]$Vole/TotalCapsList[[1]]$Nights
densitydat$voleCPUE_2 <- TotalCapsList[[2]]$Vole/TotalCapsList[[2]]$Nights
densitydat$voleCPUE_3 <- TotalCapsList[[3]]$Vole/TotalCapsList[[3]]$Nights
densitydat$voleCPUE_4 <- TotalCapsList[[4]]$Vole/TotalCapsList[[4]]$Nights
densitydat$voleCPUE_5 <- TotalCapsList[[5]]$Vole/TotalCapsList[[5]]$Nights
densitydat$voleCPUE_6 <- TotalCapsList[[6]]$Vole/TotalCapsList[[6]]$Nights
densitydat$voleCPUE_7 <- TotalCapsList[[7]]$Vole/TotalCapsList[[7]]$Nights

densitydat$chipCPUE_1 <- TotalCapsList[[1]]$Chip/TotalCapsList[[1]]$Nights
densitydat$chipCPUE_2 <- TotalCapsList[[2]]$Chip/TotalCapsList[[2]]$Nights
densitydat$chipCPUE_3 <- TotalCapsList[[3]]$Chip/TotalCapsList[[3]]$Nights
densitydat$chipCPUE_4 <- TotalCapsList[[4]]$Chip/TotalCapsList[[4]]$Nights
densitydat$chipCPUE_5 <- TotalCapsList[[5]]$Chip/TotalCapsList[[5]]$Nights
densitydat$chipCPUE_6 <- TotalCapsList[[6]]$Chip/TotalCapsList[[6]]$Nights
densitydat$chipCPUE_7 <- TotalCapsList[[7]]$Chip/TotalCapsList[[7]]$Nights

densitydat$totCPUE_1 <- TotalCapsList[[1]]$Tot.Caps/TotalCapsList[[1]]$Nights
densitydat$totCPUE_2 <- TotalCapsList[[2]]$Tot.Caps/TotalCapsList[[2]]$Nights
densitydat$totCPUE_3 <- TotalCapsList[[3]]$Tot.Caps/TotalCapsList[[3]]$Nights
densitydat$totCPUE_4 <- TotalCapsList[[4]]$Tot.Caps/TotalCapsList[[4]]$Nights
densitydat$totCPUE_5 <- TotalCapsList[[5]]$Tot.Caps/TotalCapsList[[5]]$Nights
densitydat$totCPUE_6 <- TotalCapsList[[6]]$Tot.Caps/TotalCapsList[[6]]$Nights
densitydat$totCPUE_7 <- TotalCapsList[[7]]$Tot.Caps/TotalCapsList[[7]]$Nights

densitydat$nights_1 <-TotalCapsList[[1]]$Nights
densitydat$nights_2 <-TotalCapsList[[2]]$Nights
densitydat$nights_3 <-TotalCapsList[[3]]$Nights
densitydat$nights_4 <-TotalCapsList[[4]]$Nights
densitydat$nights_5 <-TotalCapsList[[5]]$Nights
densitydat$nights_6 <-TotalCapsList[[6]]$Nights
densitydat$nights_7 <-TotalCapsList[[7]]$Nights

write.csv(densitydat,file="./Results/DensityData.csv")
