
## First we read in the data sets using read.table(). No special usage required.
ste<-read.table("test/subject_test.txt")
xte<-read.table("test/x_test.txt")
yte<-read.table("test/y_test.txt")
varnames<-read.table("features.txt")
stn<-read.table("train/subject_train.txt")
xtn<-read.table("train/x_train.txt")
ytn<-read.table("train/y_train.txt")

## Then we use for a while loop to assign names to the activity vectors yte,ytn.
i<-c(as.numeric(yte[[1]]),as.numeric(ytn[[1]]))
t<-1
while (t <= length(i)){
    if(i[t]==1){
        i[t]<-"Walking"
    }
    if(i[t]==2){
        i[t]<-"Walking Upstairs"
    }
    if(i[t]==3){
        i[t]<-"Walking Downstairs"
    }
    if(i[t]==4){
        i[t]<-"Sitting"
    }
    if(i[t]==5){
        i[t]<-"Standing"
    }
    if(i[t]==6){
        i[t]<-"Laying"
    }
    t<-t+1
}
## Now we rowbind the large data frames
Fullset<-rbind(xte,xtn)
## And combine the subject test vectors
j<-c(as.numeric(ste[[1]]),as.numeric(stn[[1]]))
## Then we column bind the activities and the full set data frame
Fullset<-cbind(Fullset, i,j)
## And apply labels
p<-as.factor(c(as.character(varnames[[2]]),"Activity","Participant"))
colnames(Fullset)<-p
## Now we're going to clean up the global environment to preserve memory
rm(ste)
rm(stn)
rm(xte)
rm(xtn)
rm(varnames)
rm(yte)
rm(ytn)
rm(t)
rm(i)
rm(j)
rm(p)
## Now we subset the dataframe so we only have values corresponding to means and
## standard deviations.
Fullset<-Fullset[c(562:563,542,543,529,530,516,517,503,504,424:429,345:350,266:271,253,254,240,241,227,228,214,215,201,202,161:166,121:126,81:86,41:46,1:6)]
## Finally, we build a new dataframe using sapply to take the means of the columns
## that are remaining after subsetting. Using 6 while loops, we go through each
## activity and each participant. Nesting loops are a viable alternative, but
## this provides the added benefit of being more readable to those without a
## background in programming.

meandat<-data.frame(matrix(ncol=68))
colnames(meandat)<-colnames(Fullset)
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Walking",],mean)
    k[1]<-"Walking"
    meandat<-rbind(meandat,k)
    i<-i+1
}
meandat<-meandat[2:31,]
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Walking Upstairs",],mean)
    k[1]<-"Walking Upstairs"
    meandat<-rbind(meandat,k)
    i<-i+1
}
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Walking Downstairs",],mean)
    k[1]<-"Walking Downstairs"
    meandat<-rbind(meandat,k)
    i<-i+1
}
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Sitting",],mean)
    k[1]<-"Sitting"
    meandat<-rbind(meandat,k)
    i<-i+1
}
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Standing",],mean)
    k[1]<-"Standing"
    meandat<-rbind(meandat,k)
    i<-i+1
}
i<-1
while (i<=30){
    k<-sapply(Fullset[Fullset$Participant==i&Fullset$Activity=="Laying",],mean)
    k[1]<-"Laying"
    meandat<-rbind(meandat,k)
    i<-i+1
}
## Just a little more clean up
rm(i)
rm(k)
## And to end things we write the tidy file using write.csv() to a txt file so it
## can be accepted by the grading system
write.csv(meandat,file="Tidy_data.txt")