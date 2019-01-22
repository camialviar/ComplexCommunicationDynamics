######## This file contains the codes for the analysis we did on the project.

######## Libraries

library(lme4)
library(lmerTest)
library(matrixStats)

rt = "./ComplexCommunicationDynamics/Data files" # The path to the working directory
setwd(rt) # Set the working directory

####### Let's load up the datasets:

AllDataRaw = read.table('AllDataRaw.txt', header=T, sep = '')

AllDataScaled = read.table('AllDataScaled.txt', header=T, sep = '')

AllDataScaledInd = read.table('AllDataScaledInd.txt', header=T, sep = '')

MatrixScaledInd = read.table('MatrixScaledInd.txt', header=T, sep = '') # This is the matrix (standardized per speaker) in which we performed PCA

MatrixScaledSample = read.table('MatrixScaledSample.txt', header=T, sep = '') # This is the matrix (standardized across the sample) in which we performed PCA


######## Mixed effects models
# The relevant variable names for the model are "mean.bodyw.", "sRate", "speechrate..nsyll.dur.", 
# "articulation.rate..nsyll...phonationtime.", "f0mean", "meanIntensity"

lmeoFullRandom=lmer(meanIntensity~i+isq
                    +(1+i+isq|fl), data=AllDataScaledInd)
summary(lmeoFullRandom)
BIC(lmeoFullRandom)

lmeo=lmer(meanIntensity~i+isq
          +(1|fl)
          +(0+i|fl),
          #+(0+isq|fl), 
          data=AllDataScaledInd)
summary(lmeo)
BIC(lmeo)

lmeoIntercept=lmer(meanIntensity~1
                        +(1|fl), 
                        data=AllDataScaledInd)

######## Plotting the average trends for each DV
avg= aggregate(AllDataRaw[,3]~i, data = AllDataRaw, mean) #The column will change depending on what variable we want to plot
plot(avg, type='l', xlab = "Time Window",
     ylab = "Body movement", cex.lab=1.4)



######## Principal Component Analysis

### Scaled across sample
PcaModel = prcomp(MatrixScaledSample[,1:6],scale=F,center=F) 
PcaModel$sdev^2
(PcaModel$sdev^2)/sum(PcaModel$sdev^2)
PcaModel$rotation

# Reconstruct fully with svd 
svdModel = svd(MatrixScaledSample[,1:6]) # omit t from the solution
svdNewScores = svdModel$u %*% diag(svdModel$d)
svdModel$v # same as rotation above
colSds(svdNewScores)^2 # same as PcaModel$sdev^2 above
plot(svdNewScores[,1],PcaModel$x[,1]) # equivalent

### Scaled within individual speakers
PcaModel = prcomp(MatrixScaledInd[,1:6],scale=F,center=F) # omit t from the solution
PcaModel$sdev^2
(PcaModel$sdev^2)/sum(PcaModel$sdev^2)
PcaModel$rotation

# Reconstruct fully with svd along
svdModel = svd(MatrixScaledInd[,1:6]) # omit t from the solution
svdNewScores = svdModel$u %*% diag(svdModel$d)
svdModel$v # same as rotation above
colSds(svdNewScores)^2 # same as PcaModel$sdev^2 above
plot(svdNewScores[,1],PcaModel$x[,1]) # equivalent

######## Plotting PCA. NB: This was removed from the final paper; this plots speaker windows in PC1 and PC2 space; original plotting used princomp (scores vs. x)

speakerId = rep(0,10) # creating a variable to use when plotting every subject with a different shape
for (i in 1:20) {
  speakerId = c(speakerId,rep(i,10))
}

cols = c() # creating a variable so we can plot each time window with a different shade of grey
for (i in 1:300) {
  cols = c(cols,rgb(scaledDataTime$i/12,scaledDataTime$i/12,scaledDataTime$i/12))
}

plot(PcaModel$scores[,1],PcaModel$scores[,2],pch=speakerId, col=cols, # Plotting the data projected into the first two components
     xlab = "Component 1", ylab="Component 2", cex.lab=1.2)

######## Plotting the each speaker's time series for each DV
dev.off()
par(mfrow=c(6,5))
par(mar = c(0.8, 2, 0, 0.5), oma = c(4, 3, 0.8, 0.5))
par(mar = c(0.5,0.5,0.5,0.5), oma=c(4,4,0.5,0.5))

for(i in seq(1,300,10)){
  plot(AllDataScaledInd[i:(i+9),17], axes = FALSE, type = "l")
  if (i>250){
    axis(1, at = seq(1, 10, 1))
  }
  #if(i%%50==1){
  #axis(2, at=seq(floor(min(AllDataScaledInd[i:(i+9),23])),ceiling(max(AllDataScaledInd[i:(i+9),23])),0.5))
  #}
  box(col = "black") 
}
mtext("Time Window", side = 1, outer = TRUE, cex = 0.9, line = 2.2)
mtext("Fundamental Frequency (Scaled)", side = 2, outer = TRUE, cex = 0.9, line = 1)
