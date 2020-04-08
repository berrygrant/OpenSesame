##########################################
#Digit Span
#(c) Grant M. Berry, 2015
#grantberry@psu.edu
#http://grantberry.info
##########################################

#This section calls all .csv files from your directory and combines them into a single data frame.
mydir="/////Where your files are//////"
dspan_dir<-mydir
dspan_files<-list.files(dspan_dir,pattern="*.csv",full.names=TRUE)
dspan=do.call(rbind,lapply(dspan_files, read.csv))

#Get DSpan Score
library(plyr)
dspan_check=ddply(dspan, .(subject_nr, Nback),summarize, response=response,Num1=num0,Num2=num1,Num3=num2,Num4=num3,Num5=num4,Num6=num5,Num7=num6,Num8=num7,Num9=num8,Num10=num9)
dspan_check$answer=with(dspan_check,paste0(Num1,Num2,Num3,Num4,Num5,Num6,Num7,Num8,Num9,Num10,sep=""))
dspan_check$answer=with(dspan_check, substr(answer, 1,Nback))
#Check their answer with the corret answer
dspan_correct=subset(dspan_check, match(response,answer)!=0)

#Count correct answers and divide by speaker and Nback condition
dspan_correct$count=rep(1,nrow(dspan_correct))
dspan_acc=ddply(dspan_correct, .(subject_nr,Nback),summarize,CorrectSeqs=sum(count)/5)

#Set the threshold for a given N-back condition to those where participants got 3 or more sequences correct
dspan_thresh=subset(dspan_acc, CorrectSeqs>0.4)
#Now identify the maximum score
dspan_score=ddply(dspan_thresh, .(subject_nr),summarize, DSpanScore=max(Nback))
print(dspan_score)
