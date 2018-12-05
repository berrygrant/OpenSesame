#Stroop DMC
###################################

#####	REMOVE PROACTIVE BLOCK COST
stroop=read.csv("/Volumes/Dropbox/Box Sync/Dissertation/DissExp1/Stroop_Coded/Stroop_RTs_11-1-2017.csv")
#Throw out weird RTs (including those that are coded 999, meaning the script failed to find an RT)
stroop=subset(stroop,RT>=0.25&RT!='999'&abs(RT-mean(stroop$RT))<=2.5*sd(stroop$RT))
#Get averages per person, per condition
stroop_avgs=ddply(stroop,.(Participant,Block,Congruency,Condition),summarize,meanRT=mean(RT),sdRT=sd(RT))

#Set up to get costs

#Proactive Control Measures
proactive_lwmc_congruent=droplevels(subset(stroop,Block=='LWMC'&Congruency=='Congruent'))
proactive_lwmc_congruent_avg=ddply(proactive_lwmc_congruent,.(Participant),summarize,meanRT_con=mean(RT))
proactive_lwmc_incongruent=droplevels(subset(stroop,Block=='LWMC'&Congruency=='Incongruent'))
proactive_lwmc_incongruent_avg=ddply(proactive_lwmc_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

pro_lwmc_cost=merge(proactive_lwmc_congruent_avg,proactive_lwmc_incongruent_avg,by='Participant')
pro_lwmc_cost$lwmc_conCost=with(pro_lwmc_cost,meanRT_incon-meanRT_con)

proactive_lwmi_congruent=droplevels(subset(stroop,Block=='LWMI'&Congruency=='Congruent'))
proactive_lwmi_congruent_avg=ddply(proactive_lwmi_congruent,.(Participant),summarize,meanRT_con=mean(RT))
proactive_lwmi_incongruent=droplevels(subset(stroop,Block=='LWMI'&Congruency=='Incongruent'))
proactive_lwmi_incongruent_avg=ddply(proactive_lwmi_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

pro_lwmi_cost=merge(proactive_lwmi_congruent_avg,proactive_lwmi_incongruent_avg,by='Participant')
pro_lwmi_cost$lwmi_conCost=with(pro_lwmi_cost,meanRT_incon-meanRT_con)

pro_blockconcost=merge(pro_lwmc_cost,pro_lwmi_cost,by='Participant')
pro_blockconcost$Proactive_BlockCost=with(pro_blockconcost,lwmc_conCost-lwmi_conCost)

proactive_lwmc_pc50_congruent=droplevels(subset(stroop,Block=='LWMC'&Condition=='PC50'&Congruency=='Congruent'))
proactive_lwmc_pc50_congruent_avg=ddply(proactive_lwmc_pc50_congruent,.(Participant),summarize,meanRT_lwmc_con=mean(RT))
#proactive_lwmc_pc50_incongruent=droplevels(subset(stroop,Block=='LWMC'&Condition=='PC50'&Congruency=='Incongruent'))
#proactive_lwmc_pc50_incongruent_avg=ddply(proactive_lwmc_pc50_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

#pro_lwmc_pc50_cost=merge(proactive_lwmc_pc50_congruent_avg,proactive_lwmc_pc50_incongruent_avg,by='Participant')
#pro_lwmc_pc50_cost$lwmc_pc50_conCost=with(pro_lwmc_pc50_cost,meanRT_incon-meanRT_con)

proactive_lwmi_pc50_congruent=droplevels(subset(stroop,Block=='LWMI'&Condition=='PC50'&Congruency=='Congruent'))
proactive_lwmi_pc50_congruent_avg=ddply(proactive_lwmi_pc50_congruent,.(Participant),summarize,meanRT_lwmi_con=mean(RT))
#proactive_lwmi_pc50_incongruent=droplevels(subset(stroop,Block=='LWMI'&Condition=='PC50'&Congruency=='Incongruent'))
#proactive_lwmi_pc50_incongruent_avg=ddply(proactive_lwmi_pc50_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

#pro_lwmi_pc50_cost=merge(proactive_lwmi_pc50_congruent_avg,proactive_lwmi_pc50_incongruent_avg,by='Participant')
#pro_lwmi_pc50_cost$lwmi_pc50_conCost=with(pro_lwmi_pc50_cost,meanRT_incon-meanRT_con)

pro_blockPCcost=merge(proactive_lwmi_pc50_congruent_avg,proactive_lwmc_pc50_congruent_avg,by='Participant')
pro_blockPCcost$Proactive_PCcost=with(pro_blockPCcost,meanRT_lwmc_con/meanRT_lwmi_con); proactive_costs = pro_blockPCcost

#Create proactive cost table. For now, we're ignoring the PC block costs, because I need to check if this should be based on congruency or not.
#proactive_costs=merge(pro_blockPCcost,pro_blockconcost,by="Participant")
#proactive_costs=subset(proactive_costs,select=c('Participant','Proactive_BlockCost','Proactive_PCcost'))
proactive_costs=subset(proactive_costs,select=c('Participant','Proactive_PCcost'))
#Reactive Control Measure
reactive_pc75_congruent=droplevels(subset(stroop,Block=='ISPC'&Condition=='MC'&Congruency=='Congruent'))
reactive_pc75_congruent_avg=ddply(reactive_pc75_congruent,.(Participant),summarize,meanRT_con=mean(RT))
reactive_pc75_incongruent=droplevels(subset(stroop,Block=='ISPC'&Condition=='MC'&Congruency=='Incongruent'))
reactive_pc75_incongruent_avg=ddply(reactive_pc75_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

re_pc75_cost=merge(reactive_pc75_congruent_avg,reactive_pc75_incongruent_avg,by='Participant')
re_pc75_cost$PC75_concost=with(re_pc75_cost,meanRT_incon-meanRT_con)

reactive_pc25_congruent=droplevels(subset(stroop,Block=='ISPC'&Condition=='MI'&Congruency=='Congruent'))
reactive_pc25_congruent_avg=ddply(reactive_pc25_congruent,.(Participant),summarize,meanRT_con=mean(RT))
reactive_pc25_incongruent=droplevels(subset(stroop,Block=='ISPC'&Condition=='MI'&Congruency=='Incongruent'))
reactive_pc25_incongruent_avg=ddply(reactive_pc25_incongruent,.(Participant),summarize,meanRT_incon=mean(RT))

re_pc25_cost=merge(reactive_pc25_congruent_avg,reactive_pc25_incongruent_avg,by='Participant')
re_pc25_cost$PC25_concost=with(re_pc25_cost,meanRT_incon-meanRT_con)

re_PCcost=merge(re_pc25_cost,re_pc75_cost,by='Participant')
re_PCcost$Reactive_PCcost=with(re_PCcost,PC75_concost/PC25_concost)

reactive_costs=subset(re_PCcost,select=c('Participant','Reactive_PCcost'))

stroop_costs=merge(reactive_costs,proactive_costs,by='Participant')
stroop_costs$Speaker=stroop_costs$Participant

#Need to re-orient measures
#Proactive:
##Stroop PC: Higher ratios index STRONGER control


#Reactive:
##Stroop PC: Higher ratios index WEAKER control

stroop_costs$Reactive_PCcost=-1*stroop_costs$Reactive_PCcost

