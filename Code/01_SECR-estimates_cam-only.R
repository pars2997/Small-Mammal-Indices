##Set directory and install/load packages##
library(secr)
library(tidyverse)
#Read in data#
mousedat<-read.capthist("./Data/SECR_MOUSE_cam.txt",list("./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                            "./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                            "./Data/FullSession.csv","./Data/FullSession.csv","./Data/Minustwotraps.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv"),
                      fmt="trapID",noccasions=c(7,7,7,7,
                                                7,7,7,7,
                                                7,7,5,7,7,7,7),
                     covnames="PM")

voledat<-read.capthist("./Data/SECR_VOLE_cam.txt",list("./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                              "./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                              "./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv"),
                         fmt="trapID",noccasions=c(7,7,7,7,
                                                   7,7,7,
                                                   7,7,7,7),
                         covnames = "PM")

chipdat<-read.capthist("./Data/SECR_CHIP_cam.txt",list("./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                                  "./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                                  "./Data/FullSession.csv","./Data/FullSession.csv","./Data/Minustwotraps.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv"),
                         fmt="trapID",noccasions=c(7,7,7,
                                                   7,7,7,
                                                   7,7,5,7,7,7),
                         covnames = "PM")
totdat<-read.capthist("./Data/SECR_Total_cam.txt",list("./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                                "./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv",
                                                "./Data/FullSession.csv","./Data/FullSession.csv","./Data/Minustwotraps.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv","./Data/FullSession.csv"),
                      fmt="trapID",noccasions=c(7,7,7,7,
                                                7,7,7,7,
                                                7,7,5,7,7,7,7),
                        covnames=c("PM","Vole","Chip"))

##Fit models##
#Trace is set to false so you wont see anything#
#but I suspect each of these models will take at least an hour to run#
#mouse and totmods will likely take several hours#
#if you're running in the background, change ncores to 1 probably#
#although I'm not sure it actually does anything without other packages installed#
mousemod<-secr.fit(mousedat,model=list(D~1,g0~session,sigma~1),trace=F,buffer=200,ncores=2)
mouseder<-derived(mousemod)

volemod<-secr.fit(voledat,model=list(D~1,g0~session,sigma~1),trace=F,details = list(autoini=10),buffer=200,ncores=2)
voleder<-derived(volemod)

chipmod<-secr.fit(chipdat,model=list(D~1,g0~session,sigma~1),trace=F,details = list(autoini=11),buffer=200,ncores=2)
chipder<-derived(chipmod)

totmod<-secr.fit(totdat,model=list(D~1,g0~session,sigma~1),trace=F,buffer=200,ncores=2)
totder<-derived(totmod)

#Please do note any errors or warnings. Most likely to occur with vole and chip models#

##Format output and export to csv##

saveRDS(mousemod,"./Results/mousesecr.rds")
saveRDS(volemod,"./Results/volesecr.rds")
saveRDS(chipmod,"./Results/chipsecr.rds")
saveRDS(totmod,"./Results/totsecr.rds")

mousevals<-mouseder[[1]]
for(i in 2:length(mouseder)){
  mousevals<-rbind(mousevals,mouseder[[i]])
}
mousevals$Site<-rep(names(mouseder),each=2)
mousevals

volevals<-voleder[[1]]
for(i in 2:length(voleder)){
  volevals<-rbind(volevals,voleder[[i]])
}
volevals$Site<-rep(names(voleder),each=2)
volevals

chipvals<-chipder[[1]]
for(i in 2:length(chipder)){
  chipvals<-rbind(chipvals,chipder[[i]])
}
chipvals$Site<-rep(names(chipder),each=2)
chipvals

totvals<-totder[[1]]
for(i in 2:length(totder)){
  totvals<-rbind(totvals,totder[[i]])
}
totvals$Site<-rep(names(totder),each=2)
totvals

head(mousevals)
mousevals<-mousevals[seq(2,nrow(mousevals),2),]
mousevals<-mousevals[,c("estimate","SE.estimate","lcl","ucl","Site")]
colnames(mousevals)<-c("mouse_den","mouse_se","mouse_low","mouse_up","Site")
volevals<-volevals[seq(2,nrow(volevals),2),]
volevals<-volevals[,c("estimate","SE.estimate","lcl","ucl","Site")]
colnames(volevals)<-c("vole_den","vole_se","vole_low","vole_up","Site")
chipvals<-chipvals[seq(2,nrow(chipvals),2),]
chipvals<-chipvals[,c("estimate","SE.estimate","lcl","ucl","Site")]
colnames(chipvals)<-c("chip_den","chip_se","chip_low","chip_up","Site")
totvals<-totvals[seq(2,nrow(totvals),2),]
totvals<-totvals[,c("estimate","SE.estimate","lcl","ucl","Site")]
colnames(totvals)<-c("total_den","total_se","total_low","total_up","Site")

densityestimates<-merge(mousevals,volevals,by="Site")
densityestimates<-left_join(mousevals,volevals,by="Site")
densityestimates<-left_join(densityestimates,chipvals,by="Site")
densityestimates<-left_join(densityestimates,totvals,by="Site")
densityestimates<-densityestimates[,c(5,1:4,6:17)]
densityestimates<-densityestimates[c(1:4,9:15,5:8),]
densityestimates[,2:17]<-round(densityestimates[,2:17],1)
densityestimates$Site<-c("MidC","MidD","MidE","MidF","OldA","OldB","OldC","OldD",
                         "OldE","OldF","OldG","YoungC","YoungD","YoungE","YoungG")
write.csv(densityestimates,"./Results/densityestimates.csv")
