#This file contains the codes for the analysis we did on the project.

rt = './ComplexCommunicationDynamics/Data files' #The path to the working directory
setwd(rt) #Set the working directory

#Let's load up the datasets:

AllDataRaw = read.table('AllDataRaw.txt', header=T, sep = ' ')

AllDataScaled = read.table('AllDataScaled.txt', header=T, sep = ' ')

AllDataScaledInd = read.table('AllDataScaledInd.txt', header=T, sep = ' ')

MatrixScaledInd = read.table('MatrixScaledInd.txt', header=T, sep = ' ') #This is the matrix (standardized per speaker) in which we performed PCA

MatrixScaledSample = read.table('MatrixScaledSample.txt', header=T, sep = ' ') #This is the matrix (standardized across the sample) in which we performed PCA


#Mixed effects models
#The relevant variable names for the model are "mean.bodyw.", "sRate", "speechrate..nsyll.dur.", 
#"articulation.rate..nsyll...phonationtime.", "f0mean", "meanIntensity"

lmeo=lmer(meanIntensity~i+isq+(1+i+isq|fl), data=AllDataScaledInd)
summary(lmeo)

#plotting the average trends for each DV
avg= aggregate(AllDataRaw[,3]~i, data = AllDataRaw, mean) #The column will change depending on what variable we want to plot
plot(avg, type='l', xlab = "Time Window",
     ylab = "Body movement", cex.lab=1.4)



#Principal Component Analysis

PcaModel = princomp(ScaledDataSample) #This analyses were performed over the matrices that only included the 6 variables of interest
                                      #They were done both on the data scaled across the sample, and the data scaled per subject.
PcaModelSub$loadings

VarExp = cumsum(PcaModel$sdev^2)/sum(PcaModel$sdev^2)*100 
VarExp


#Plotting PCA

speakerId = rep(0,10) #creating a variable to use when plotting every subject with a different shape
for (i in 1:20) {
  speakerId = c(speakerId,rep(i,10))
}

cols = c() # creating a variable so we can plot each time window with a different shade of grey
for (i in 1:300) {
  cols = c(cols,rgb(scaledDataTime$i/12,scaledDataTime$i/12,scaledDataTime$i/12))
}


plot(PcaModel$scores[,1],PcaModel$scores[,2],pch=speakerId, col=cols, #Plotting the data projected into the first two components
     xlab = "Component 1", ylab="Component 2", cex.lab=1.2)
