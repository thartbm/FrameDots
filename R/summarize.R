
summarizeFIPS <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  nullresponse <- 0
  
  amp    <- NA
  ampvel <- NA
  
  for (ppno in c(1:dim(participants)[1])) {
    
    # read participant's data:
    pp <- participants$participant[ppno]
    ts <- participants$timestamp[ppno]
    filename <- sprintf('data/Pavlovia/%s_FrameDots_%s.csv', pp, ts)
    ppFIPS <- read.csv(filename, stringsAsFactors = F)
    
    # doing anything with the catch trials?
    
    trialtypes <- c('base', 'amp', 'amp+vel')
    ppFIPS <- ppFIPS[which(ppFIPS$trialtype %in% trialtypes),]
    
    # correct for xfactor:
    ppFIPS$norm_percept <- ppFIPS$distancepercept_rel * ppFIPS$xfactor
    
    pp.amp    <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp')),], FUN=mean)
    pp.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp')),], FUN=mean)
    
    if (any(pp.amp$norm_percept == 0) | any(pp.ampvel$norm_percept == 0)) {
      nullresponse <- nullresponse + 1
    } else {
      
      pp.amp$participant <- pp
      pp.ampvel$participant <- pp
      
      if (is.data.frame(amp)) {
        amp    <- rbind(amp, pp.amp)
        ampvel <- rbind(ampvel, pp.ampvel)
      } else {
        amp    <- pp.amp
        ampvel <- pp.ampvel
      }
    }
    
  }
  
  #print(nullresponse)
  
  return(list('amp'=amp, 'ampvel'=ampvel))
  
}


plotFIPSpercepts <- function() {
  
  data <- summarizeFIPS()
  
  plot(-1000, -1000, main='', xlab='frame movement [nsu]', ylab='percieved distance [nsu]', xlim=c(0.0,0.3), ylim=c(0.0, 0.3), ax=F, bty='n')
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
  
  agg.amp    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=mean)
  agg.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=mean)
  
}


getDemographics <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  cat(sprintf('number of participants: %d\n', dim(participants)[1]))
  
  cat('age:\n')
  cat(sprintf('  mean: %0.1f, sd: %0.1f\n', mean(participants$age), sd(participants$age)))
  cat(sprintf('  range: %d - %d\n', min(participants$age), max(participants$age)))
  
  cat('sex:\n')
  Nfemale <- length(which(participants$sex == 'Female'))
  Nmale <- length(which(participants$sex == 'Male'))
  Nother <- dim(participants)[1] - Nfemale - Nmale
  cat(sprintf('  female: %d\n  male: %d\n  other: %d\n', Nfemale, Nmale, Nother ))
  
  cat('handedness:\n')
  Nright <- length(which(participants$handedness == 'Right'))
  Nleft <- length(which(participants$handedness == "Left "))
  Notherh <- dim(participants)[1] - Nright - Nleft
  cat(sprintf('  right: %d\n  left: %d\n  other: %d\n', Nright, Nleft, Notherh))
  
}