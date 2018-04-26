library(nlme)
library(reshape)
library(plyr)
library(ggplot2)


TS.enz<-rbind(read.table("data/Jan2017_3Temps.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
#              TS.enz<-rbind(read.table("Jan2017/Jan2017_Edt.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
#              read.table("May2015/May2015.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
              read.table("data/May2015_3Temps.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
#              read.table("Aug2016/Aug2016.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
              read.table("data/Aug2016_3Temps.csv",header=TRUE,sep=",",stringsAsFactors = FALSE),
              read.table("data/Sept2014_3Temps.csv",header=TRUE,sep=",",stringsAsFactors = FALSE))
#              read.table("Sept2014/Sep2014_all.csv",header=TRUE,sep=",",stringsAsFactors = FALSE))
head(TS.enz)

Wood.enz<-melt(TS.enz,id=c("Sample_ID", "Date", "Time", "Temp","Standard", 
                                  "Quench_Contr", "Homog_Contr", "BG_Contr", "BX_Contr",
                                  "CBH_Contr", "NAG_Contr", "Blank"))
head(Wood.enz)
tail(Wood.enz)
Wood.enz$Assay<-""
Wood.enz$Sub_Contr<-""
Wood.enz$Assay[Wood.enz$variable=="BG_Assay"]<-"BG"
Wood.enz$Assay[Wood.enz$variable=="BX_Assay"]<-"BX"
Wood.enz$Assay[Wood.enz$variable=="CBH_Assay"]<-"CBH"
Wood.enz$Assay[Wood.enz$variable=="NAG_Assay"]<-"NAG"
Wood.enz$Sub_Contr[Wood.enz$variable=="BG_Assay"]<-Wood.enz$BG_Contr
Wood.enz$Sub_Contr[Wood.enz$variable=="BX_Assay"]<-Wood.enz$BX_Contr
Wood.enz$Sub_Contr[Wood.enz$variable=="CBH_Assay"]<-Wood.enz$CBH_Contr
Wood.enz$Sub_Contr[Wood.enz$variable=="NAG_Assay"]<-Wood.enz$NAG_Contr

tail(Wood.enz)
head(Wood.enz)

Wood.enz$variable<-NULL
Wood.enz$BG_Contr<-NULL
Wood.enz$BX_Contr<-NULL
Wood.enz$CBH_Contr<-NULL
Wood.enz$NAG_Contr<-NULL

Wood.enz<-Wood.enz[ ,c(1,2,3,4,10,9,11,5,6,7,8)]

#detach(Wood.enz)

Wood.enz<-na.omit(Wood.enz)

means<-ddply(Wood.enz, .(as.factor(Time), Date, Sample_ID, Assay), summarize,
             meanSC = mean(as.numeric(Sub_Contr)), meanQC = mean(Quench_Contr), 
             meanStan = mean (Standard), meanHC = mean(Homog_Contr), meanBlnk = mean(Blank))
head(means)

Wood.enz.ave<-merge(Wood.enz, means, by = c("Date", "Sample_ID", "Assay"))
Wood.enz.ave$'as.factor(Time)'<-NULL
Wood.enz.ave$Sub_Contr<-NULL
Wood.enz.ave$Homog_Contr<-NULL
Wood.enz.ave$Quench_Contr<-NULL
Wood.enz.ave$Standard<-NULL
Wood.enz.ave$Blank<-NULL

head(Wood.enz.ave)

Wood.enz.ave$FLU<-with(Wood.enz.ave, ((value - meanHC) / ((meanQC - meanHC) / meanStan)) - meanSC)
Wood.enz.ave$Activity<-with(Wood.enz.ave,  FLU / (meanStan / (25 * 0.02 )) / (0.00005 * (0.2 / 0.006) * Time))

Wood.enz.Activity<-ddply(Wood.enz.ave, .(Date, Sample_ID,Temp, Assay), summarize,
                         meanAct = mean(Activity))
head(Wood.enz.Activity)

class(Wood.enz.Activity$Temp)

Wood.enz.Activity$Date <- factor(Wood.enz.Activity$Date, levels=c("Jan2017","May2015","Aug2016","Sep2014"))
Wood.enz.Activity$Assay<-factor(Wood.enz.Activity$Assay)
Wood.enz.Activity$Tempf<-as.factor(Wood.enz.Activity$Temp)
Wood.enz.Activity$Tempf<-factor(Wood.enz.Activity$Tempf,levels=c("1","15.6","28"))

Wood.enz.MeanAct<-summarySEwithin(Wood.enz.Activity, measurevar="meanAct", withinvars=c("Date","Temp","Assay"), idvar="Sample_ID")
head(Wood.enz.MeanAct)  

x11()
ggplot(Wood.enz.Activity, aes(x=Tempf,y=meanAct, col=Tempf))+geom_boxplot()+facet_grid(Assay~Date, scales = "free")+theme_bw()

ggplot(Wood.enz.Activity, aes(x=Temp,y=meanAct,col=Sample_ID))+geom_line()+facet_grid(Assay~Date,scales="free_y")+theme_bw()

BG.Act<-Wood.enz.Activity[Wood.enz.Activity$Assay=="BG",]
head(BG.Act)

lm.BG<-lm(meanAct~Temp*Date, BG.Act)

summary(lm.BG)


#smatter slope test--test for common slope (bween 1C and 28C)
#Richards et al. 2013 Sapwood capacitance-- Look for packaging