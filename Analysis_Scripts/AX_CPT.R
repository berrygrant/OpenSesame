#AX-CPT
###################################
#Fill in your directory for AX-CPT files
ax_cpt_dir="/Volumes/Dropbox/Box Sync/Dissertation/DissExp1/AX-CPT Data"
#For Experiment 2:
# ax_cpt_dir="/Volumes/Dropbox/Box Sync/Dissertation/DissExp2/Experiment Data/AX-CPT/"
ax_cpt_files<-list.files(ax_cpt_dir,pattern="*.csv",full.names=TRUE)
ax_cpt_list=lapply(ax_cpt_files, read.csv)
#Fill in participant names and subset out what we want
for(i in 1:length(ax_cpt_files)){
	file<-as.character(ax_cpt_files[i])
	subject=substr(file,nchar(file)-6,nchar(file))
	subject<-sub(".csv","",subject)
	subject<-paste0('S',subject,'')
	ax_cpt_list[[i]]$Speaker=as.factor(rep(subject,nrow(ax_cpt_list[[i]])))
	keep=c('Speaker','correct_PROBE_ANSWER','count_PROBE_ANSWER','Condition','Weight','response_time_PROBE_ANSWER')
	ax_cpt_list[[i]]=subset(ax_cpt_list[[i]],select=keep)
}

#Bind together
ax_cpt<-do.call(rbind,ax_cpt_list)

#Calculating AY/BX
library(plyr)
#Calculate Accuracy
ax_cpt=subset(ax_cpt, count_PROBE_ANSWER>3)
ax_cpt_acc=ddply(ax_cpt, .(Speaker,Condition),summarize,acc=10*sum(correct_PROBE_ANSWER/(Weight)));ax_cpt_acc
ax_cpt_rts=ddply(ax_cpt, .(Speaker,Condition),summarize, avg_RT=mean(response_time_PROBE_ANSWER));ax_cpt_rts

#Get only correct trials and throw out wonky RTs (> 2.5 SDs)
ax_cpt_correct=droplevels(subset(ax_cpt, correct_PROBE_ANSWER=="1"&abs(response_time_PROBE_ANSWER-mean(ax_cpt$response_time_PROBE_ANSWER))<=2.5*sd(ax_cpt$response_time_PROBE_ANSWER)))

#Combine to get new data
ax_cpt_acc_temp=ddply(ax_cpt_correct, .(Speaker,Condition),summarize,acc=10*sum(correct_PROBE_ANSWER/(Weight)))
ax_cpt_rts_temp=ddply(ax_cpt_correct, .(Speaker,Condition),summarize, avg_RT=mean(response_time_PROBE_ANSWER))
ax_cpt_avgs=merge(ax_cpt_acc_temp,ax_cpt_rts_temp, by=c('Speaker','Condition'));ax_cpt_avgs

#Calculate condition-level accuracy and avg RTs)
ay=droplevels(subset(ax_cpt_avgs,Condition=="AY"))
ax=droplevels(subset(ax_cpt_avgs,Condition=="AX"))
bx=droplevels(subset(ax_cpt_avgs,Condition=="BX"))
by=droplevels(subset(ax_cpt_avgs,Condition=="BY"))
ax_ay_temp=merge(ay,ax,by='Speaker')
ayax=ddply(ax_ay_temp,.(Speaker),summarize,AY_AX=avg_RT.x/avg_RT.y)
bx_by_temp=merge(bx,by,by='Speaker')
bxby=ddply(bx_by_temp,.(Speaker),summarize,BX_BY=avg_RT.y/avg_RT.x)
ay_bx_temp=merge(ay,bx,by='Speaker');ay_bx_temp
aybx=ddply(ay_bx_temp,.(Speaker),summarize,AY_BX=avg_RT.x/avg_RT.y)
temp1=merge(ayax,bxby,by='Speaker');temp2=merge(temp1,aybx,by='Speaker')
ay_bx=temp2

#Try accuracy
library(reshape2)
acc_bycond=dcast(ax_cpt_acc,Speaker~Condition);acc_bycond$ProAcc=with(acc_bycond,AY/AX);acc_bycond$ReAcc=with(acc_bycond,BX/BY)
