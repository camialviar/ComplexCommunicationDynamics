#Functions
#This function gets the slide rate based on the number of spikes that go over the 
#threshold selected for each speaker.

getSlideRate = function(time_series, threshold=3) {
  slice_value = mean(time_series) + threshold*sd(time_series)
  change_indices = which(time_series>slice_value)
  jumps = which(diff(change_indices)*.04 > 10) # X-second restriction
  numSlide = c(jumps, length(change_indices))
  sRate = length(numSlide)/((length(time_series)*.04)/60)
  return(sRate)
}


#Let's load the files

rt = './ComplexCommunicationDynamics/Data files/Raw data' #The path to the working directory
setwd(rt) #Set the working directory
fls = list.files('.',pattern='txt') # '.' = current directory; pattern matches certain files



#Plotting the slides time series to decide the threshold for each speaker

dat = read.table(fls[30],header=T) #Reading the files. Plotting and eye-balling parameters so we do it one by one
ts_slides = scale(dat$SlidesBmag)
plot(ts_slides, type = 'l', ylab = 'Total Pixel Change in the Slide Area', 
       cex.lab = 1.2, xlab='Time (min)')
points(c(0, length(ts_slides)), c(5, 5), type = 'l', col='red') #This creates a red line to estimate the treshold. Change the numbers in the second set of terms to try different tresholds 
change_indices = which(ts_slides>5) #Which spikes is it capturing
jumps = which(diff(change_indices)*.04 > 10) # 10-second restriction
points(change_indices[jumps], ts_slides[change_indices[jumps]], type='p', cex=1.3, col='blue') #plotting the spikes being captured with this threshold



#Let's get the body movement mean and slide rate for each segment

averagesBody = c()
averagesSlides = c()
thrs = c(1, 7, 1.5, 5, 1, 2.5, 1, 4.5, 3.5, 6, 1.8, 2, 1.8, 2.8, 
         6, 1.8, 3, 9, 2, 2.6, 2, 4, 1, 2.5, 7, 6, 3.5, 2, 2.5, 2.5) #Thresholds to count the slides for each speaker
count = 1

for (fl in fls){
  print(fl)
  dat = read.table(fl,header=T)
  
  ts_speaker = dat$Speakermag
  ts_slides = scale(dat$SlidesBmag)
  
  cp = seq(1, length(ts_speaker), length(ts_speaker)/10) #getting 10 even windows
  cp = c(cp, length(ts_slides))
  for (i in 1:(length(cp)-1)) {
    bodyw = (ts_speaker[cp[i]:cp[i+1]-1]) #Selecting only the data within each window
    averagesBody = rbind(averagesBody,data.frame(fl, i, mean(bodyw), sd(bodyw),
                                                 sd(bodyw)/mean(bodyw), max(bodyw), min(bodyw))) #Writing the mean, sd and other measures into a data frame
    
    slidesw = (ts_slides[cp[i]:cp[i+1]-1])
    sRate=getSlideRate(slidesw, thrs[count]) #Getting the slide rate
    averagesSlides = rbind(averagesSlides,data.frame(sRate)) #Writing the slide rate into a data frame the data frame to save
  }
  count= count+1
}



#Putting together the dataset

bdsd = cbind(averagesBody, averagesSlides)

#Getting the speech rate and F0 data and putting it all together. These data come from Praat.
setwd('/./ComplexCommunicationDynamics/Data files/Sound files')
speechTable = read.table("AllSR.txt", header=T, sep = ',')
averageBSlSp = cbind(bdsd, speechTable[,2:ncol(speechTable)])

setwd('./ComplexCommunicationDynamics/Data files/Sound files')
fTable = read.table("AllF0.txt", header=T, sep = '')
averageBSlSpF = cbind(averageBSlSp, fTable)

setwd('./ComplexCommunicationDynamics/Data files/Sound files')
intensityTable = read.table("intensityAll.txt", header=T, sep = ',')
AllDataRaw = cbind(averageBSlSpF, intensityTable[,2:ncol(intensityTable)])

#Let's write the data to a .txt file
write.table(AllDataRaw, file="./ComplexCommunicationDynamics/Data files/AllDataRaw.txt", sep=" ", row.names = FALSE, col.names = TRUE)



# Standardizing the dataset

AllDataScaled=scale(AllData[,2:ncol(AllData)]) # Standardizing across the sample

write.table(AllDataScaled, file="./ComplexCommunicationDynamics/Data files/AllDataScaled.txt", sep=" ", row.names = FALSE, col.names = TRUE)


cp = seq(1, 300, 10) # Standardizing per subject
AllDataScaledInd=c()
for(i in cp){
  tempdb = AllDataRaw[i:(i+9),] # put the name of the dataset to scale here
  tempdb = scale(tempdb)
  AllDataScaledInd = rbind(AllDataScaledInd, tempdb)
}

write.table(scaledInd, file="./ComplexCommunicationDynamics/Data files/AllDataScaledInd.txt", sep=" ", row.names = FALSE, col.names = TRUE)


