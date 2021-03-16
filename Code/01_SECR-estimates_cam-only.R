##Set directory and install/load packages##
library(secr)
setwd("./Data")
#Read in data#
mousedat<-read.capthist("SECR_MOUSE_cam.txt",list("FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv",
                                            "FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv",
                                            "FullSession.csv","FullSession.csv","Minustwotraps.csv","FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv"),
                      fmt="trapID",noccasions=c(7,7,7,7,
                                                7,7,7,7,
                                                7,7,5,7,7,7,7),
                     covnames="PM")

voledat<-read.capthist("SECR_VOLE_cam.txt",list("FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv",
                                              "FullSession.csv","FullSession.csv","FullSession.csv",
                                              "FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv"),
                         fmt="trapID",noccasions=c(7,7,7,7,
                                                   7,7,7,
                                                   7,7,7,7),
                         covnames = "PM")

chipdat<-read.capthist("SECR_CHIP_cam.txt",list("FullSession.csv","FullSession.csv","FullSession.csv",
                                                  "FullSession.csv","FullSession.csv","FullSession.csv",
                                                  "FullSession.csv","FullSession.csv","Minustwotraps.csv","FullSession.csv","FullSession.csv","FullSession.csv"),
                         fmt="trapID",noccasions=c(7,7,7,
                                                   7,7,7,
                                                   7,7,5,7,7,7),
                         covnames = "PM")
totdat<-read.capthist("SECR_Total_cam.txt",list("FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv",
                                                "FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv",
                                                "FullSession.csv","FullSession.csv","Minustwotraps.csv","FullSession.csv","FullSession.csv","FullSession.csv","FullSession.csv"),
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
mousemod<-secr.fit(mousedat,model=list(D~1,g0~session,sigma~1),trace=F,buffer=200,ncores=3)
mouseder<-derived(mousemod)

volemod<-secr.fit(voledat,model=list(D~1,g0~session,sigma~1),trace=F,details = list(autoini=10),buffer=200,ncores=3)
voleder<-derived(volemod)

chipmod<-secr.fit(chipdat,model=list(D~1,g0~session,sigma~1),trace=F,details = list(autoini=11),buffer=200,ncores=3)
chipder<-derived(chipmod)

totmod<-secr.fit(totdat,model=list(D~1,g0~session,sigma~1),trace=F,buffer=200,ncores=3)
totder<-derived(totmod)

#Please do note any errors or warnings. Most likely to occur with vole and chip models#

##Format output and export to csv##
setwd("../results")
mousevals<-mouseder[[1]]
for(i in 2:length(mouseder)){
  mousevals<-rbind(mousevals,mouseder[[i]])
}
mousevals$Site<-rep(names(mouseder),each=2)
mousevals
write.csv(file="mousedensity.csv",mousevals)

volevals<-voleder[[1]]
for(i in 2:length(voleder)){
  volevals<-rbind(volevals,voleder[[i]])
}
volevals$Site<-rep(names(voleder),each=2)
volevals
write.csv(file="voledensity.csv",volevals)

chipvals<-chipder[[1]]
for(i in 2:length(chipder)){
  chipvals<-rbind(chipvals,chipder[[i]])
}
chipvals$Site<-rep(names(chipder),each=2)
chipvals
write.csv(file="chipdensity.csv",chipvals)

totvals<-totder[[1]]
for(i in 2:length(totder)){
  totvals<-rbind(totvals,totder[[i]])
}
totvals$Site<-rep(names(totder),each=2)
totvals
write.csv(file="totdensity.csv",totvals)


