#n-back

##### REDEFINE REACTIVE COST
###################################
nback_dir="/Volumes/Dropbox/Box Sync/Dissertation/DissExp1/n-back Data"
nback_files<-list.files(nback_dir,pattern="*.csv",full.names=TRUE)
nback_list=lapply(nback_files, read.csv)
#Fill in participant names and subset out what we want
for(i in 1:length(nback_files)){
	file<-as.character(nback_files[i])
	subject=substr(file,nchar(file)-6,nchar(file))
	subject<-sub(".csv","",subject)
	subject<-paste0('S',subject,'')
	nback_list[[i]]$Speaker=as.factor(rep(subject,nrow(nback_list[[i]])))
	keep=c('Speaker','Type','Correct','correct_Response','count_Response','response_Response','response_time_Response','n')
	nback_list[[i]]=subset(nback_list[[i]],select=keep)
}

#Bind together and set factors
nback<-do.call(rbind,nback_list)
nback$n=as.factor(nback$n)
nback$correct_Response=as.factor(nback$correct_Response)

#throw out wonky RTs
nback_new=subset(nback,abs(response_time_Response-mean(nback$response_time_Response))<=2.5*sd(nback$response_time_Response))
#Now we set up two measures. The first, a lure cost, comes from the response time difference in correct lure and nolure trials, relative to the mean response time for correct trials. The second is a ratio of accuracy costs by lure condition. To get these, we need groups divided by just accuracy, just lure type, and by both.

#Both
nback_LureAcc=ddply(nback_new,.(Speaker,Type,correct_Response),summarize,meanRT=mean(na.omit(response_time_Response)))

#Individually
nback_Lure=ddply(nback_new,.(Speaker,Type),summarize,meanRT=mean(na.omit(response_time_Response)))
nback_Acc=ddply(nback_new,.(Speaker,correct_Response),summarize,meanRT=mean(na.omit(response_time_Response)))

#Reshape
speakers=ddply(nback_new,.(Speaker),summarize,Speaker=unique(Speaker))
library(reshape2)
casted_LureAcc=dcast(nback_LureAcc,Speaker~Type+correct_Response)
casted_Lure=dcast(nback_Lure,Speaker~Type)
casted_Acc=dcast(nback_Acc,Speaker~correct_Response)

#Get Lure Costs
casted_LureAcc$LureDiff=with(casted_LureAcc,Lure_1-Control_1)
casted_Acc$correctRT=casted_Acc[,3]
temp<-cbind(speakers,casted_Acc$correctRT,casted_LureAcc$LureDiff);colnames(temp)<-c('Speaker','meanCorrect','LureDiff')
temp$LureCost=with(temp,LureDiff/meanCorrect)

#Get Accuracy Cost Ratios
#casted_LureAcc$LureAccCost=with(casted_LureAcc,Lure_0-Lure_1)
#casted_LureAcc$NoLureAccCost=with(casted_LureAcc,Control_0-Control_1)
#temp2=cbind(speakers,casted_LureAcc$LureAccCost,casted_LureAcc$NoLureAccCost,casted_Lure$Control,casted_Lure$Lure);colnames(temp2)<-#c('Speaker','LureAccCost','NoLureAccCost','mean_Control','mean_Lure')
#temp2$AccuracyRatio=with(temp2,(mean_Control*LureAccCost)/(mean_Lure*NoLureAccCost))
cts=as.data.frame(xtabs(~Speaker+Type,nback_new))
l_cts=droplevels(subset(cts,Type=='Lure'))
c_cts=droplevels(subset(cts,Type=='Control'))
lures=droplevels(subset(nback_new,Type=='Lure'))
control=droplevels(subset(nback_new,Type=='Control'))
templ=as.data.frame(xtabs(~Speaker+correct_Response,lures))
lurewrong=droplevels(subset(templ,correct_Response=='0'))
lureright=droplevels(subset(templ,correct_Response=='1'))
tempc=as.data.frame(xtabs(~Speaker+correct_Response,control))
controlright=droplevels(subset(tempc,correct_Response=='1'))
combined=cbind(speakers,l_cts$Freq,lureright$Freq,c_cts$Freq,controlright$Freq);colnames(combined)<-c('Speaker','LureFreq','LureCorrect','ControlFreq','ControlCorrect')
combined$LureAcc=with(combined,LureCorrect/LureFreq);combined$ControlAcc=with(combined,ControlCorrect/ControlFreq)
combined$AccCost=with(combined,LureAcc/ControlAcc)
#Overwrite Inf with NA; Inf would have arisen if they had zero correct in the control trials or did not do the control trials.
combined$AccCost=with(combined,ifelse(AccCost<Inf,AccCost,NA)) 
nback_costs=cbind(speakers,combined$AccCost,temp$LureCost);colnames(nback_costs)<-c('Speaker','nback_AccCost','nback_LureCost')

##Need to re-orient measures
#Proactive:
##nback_LureCost: Higher ratios index WEAKER control

#Reactive:
##nback_AccCost: Higher ratios index STRONGER control 

#********SOLUTION: Multiply by -1 before standardizing (or after; doesn't matter)

nback_costs$nback_LureCost=-1*nback_costs$nback_LureCost
