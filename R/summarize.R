
summarizeFIPS <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  nullresponse <- 0
  
  amp     <- NA
  ampvel  <- NA
  wallach <- NA
  catch   <- NA
  DVAinfo <- NA
  
  for (ppno in c(1:dim(participants)[1])) {
    
    # read participant's data:
    pp <- participants$participant[ppno]
    ts <- participants$timestamp[ppno]
    filename <- sprintf('data/Pavlovia/%s_FrameDots_%s.csv', pp, ts)
    ppFIPS <- read.csv(filename, stringsAsFactors = F)
    
    dva <- ppFIPS[1,c('cc_width', 'cc_height', 'eye_distance_cm', 'one_dva_width', 'one_dva_height')]
    
    # doing anything with the catch trials?
    
    # we want the Wallach and catch trials after all
    #trialtypes <- c('base', 'amp', 'amp+vel')
    #ppFIPS <- ppFIPS[which(ppFIPS$trialtype %in% trialtypes),]
    
    # correct for xfactor:
    ppFIPS$norm_percept <- ppFIPS$distancepercept_rel * ppFIPS$xfactor
    
    
    frame_size <- 0.245
    # outer frame: 0.245 nsu
    # inner frame: 0.225 nsu
    # dot width & height: 0.025 nsu
    

    ppFIPS$norm_percept      <- ppFIPS$norm_percept / frame_size
    ppFIPS$framedistance_rel <- ppFIPS$framedistance_rel / frame_size
    
    pp.amp     <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp')),], FUN=mean)
    pp.ampvel  <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp+vel')),], FUN=mean)
    pp.wallach <- aggregate(norm_percept ~ framedistance_rel + period_s, data=ppFIPS[which(ppFIPS$trialtype == 'Wallach'),], FUN=mean)
    #pp.catch  <- aggregate(norm_percept ~ horizontal_offset + vertical_offset, data=ppFIPS[which(ppFIPS$trialtype == 'catch'),], FUN=mean)
    pp.catch   <- ppFIPS[which(ppFIPS$trialtype == 'catch'),c('horizontal_offset', 'distancepercept_rel', 'participant')]
    #str(pp.catch)
    #str(pp.wallach)
    
    if (any(pp.amp$norm_percept == 0) | any(pp.ampvel$norm_percept == 0)) {
      nullresponse <- nullresponse + 1
    } else if (any(pp.amp$norm_percept > (0.54/frame_size)) | any(pp.ampvel$norm_percept > (0.54/frame_size))) {
      nullresponse <- nullresponse + 1
    } else {
      
      pp.amp$participant <- pp
      pp.ampvel$participant <- pp
      dva$participant <- pp
      
      if (is.data.frame(amp)) {
        amp     <- rbind(amp, pp.amp)
        ampvel  <- rbind(ampvel, pp.ampvel)
        DVAinfo <- rbind(DVAinfo, dva)
        catch   <- rbind(catch, pp.catch)
      } else {
        amp     <- pp.amp
        ampvel  <- pp.ampvel
        DVAinfo <- dva
        catch   <- pp.catch
      }
      
      # does this make sense for Wallach trials?
      if (any(pp.wallach$norm_percept == 0 | pp.wallach$norm_percept > (0.54/frame_size))) {
        # do we need to do anything here?
      } else {
        
        pp.wallach$participant <- pp
        
        if (is.data.frame(wallach)) {
          wallach <- rbind(wallach, pp.wallach)
        } else {
          wallach <- pp.wallach
        }
        
      }
      
    }
    
  }
  
  
  
  #print(nullresponse)
  
  return(list('amp'=amp, 'ampvel'=ampvel, 'wallach'=wallach, 'dva'=DVAinfo, 'catch'=catch))
  
}


plotFIPSpercepts <- function() {
  
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(136, 153, 255, 255, max = 255),
                   rgb(127, 0,   216, 255, max = 255),
                   rgb(255, 147, 41,  255, max = 255))
  
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255),
                   rgb(127, 0,   216, 47,  max = 255),
                   rgb(255, 147, 41,  47,  max = 255))
  
  
  data <- summarizeFIPS()

  layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
    
  #plot(-1000, -1000, main='', xlab='frame movement [nsu]', ylab='percieved distance [nsu]', xlim=c(0.08,0.2), ylim=c(0.08, 0.2), ax=F, bty='n', asp=1)
  plot(-1000, -1000, main='vertically aligned dots', xlab='frame movement [% frame size]', ylab='percieved distance [% frame size]', xlim=c(0.3,0.8), ylim=c(0.3,0.8), ax=F, bty='n', asp=1)
  
  lines(c(0.35, 0.8), c(0.35, 0.8), col='gray', lty=2)
  
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
  
  N <- length(unique(amp$participant))
  
  agg.amp    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=mean)
  agg.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=mean)
  
  agg.amp.CI    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=SMCL::getConfidenceInterval, method='b')
  agg.ampvel.CI <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=SMCL::getConfidenceInterval, method='b')
  
  Ya  <- c(agg.amp.CI[,2][,1], rev(agg.amp.CI[,2][,2]))
  Yav <- c(agg.ampvel.CI[,2][,1], rev(agg.ampvel.CI[,2][,2]))
  
  x <- sort(unique(agg.amp$framedistance_rel))

  X <- c(x, rev(x))
  polygon(X, Ya, border=NA, col=transcolors[1])
  polygon(X, Yav, border=NA, col=transcolors[2])
  
  lines(x=agg.amp$framedistance_rel, y=agg.amp$norm_percept, col=solidcolors[1])
  lines(x=agg.ampvel$framedistance_rel, y=agg.ampvel$norm_percept, col=solidcolors[2])
  
  legend(.3,.8, legend=c('constant velocity', 'constant period'), col=solidcolors, lw=1, lty=1, bty='n')
  text(.65,.35,sprintf('N=%d',N))
  
  
  #print(agg.ampvel)
  
  axis(side=1, at=c(.4,.5,.6,.7,.8))
  axis(side=2, at=c(.4,.5,.6,.7,.8))
  
  # # # # # # # # # # # # # # # # #
  # DVA SUBPLOT
  # # # # # # # # # # # # # # # # #
  
  plot(-1000, -1000, main='vertically aligned dots', xlab='dva [nsu]', ylab='percieved distance slope', xlim=c(0,.15), ylim=c(-.5,3), ax=F, bty='n')
  
  dva <- data[['dva']]
  # get participants who say they were carefull in calibration:
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  calibrated <- participants$participant[which(participants$card_used == "Yes" & participants$distance_method == "measured")]
  dva <- dva[which(dva$participant %in% calibrated & dva$participant %in% unique(amp$participant)),]
  
  
  
  dva$one_dva <- (dva$one_dva_height + dva$one_dva_width) / 2
  dva <- dva[which(dva$one_dva > 0),]
  
  #print(dva$one_dva)
  dva.amp    <- amp[   which(amp$participant    %in% dva$participant),]
  dva.ampvel <- ampvel[which(ampvel$participant %in% dva$participant),]
  
  dva.amp$norm_percept      <- dva.amp$norm_percept * .245
  dva.amp$framedistance_rel <- dva.amp$framedistance_rel * .245
  
  dva$amp.slope <- NA
  
  for (ppno in c(1:length(dva$participant))) {
    
    pp <- dva$participant[ppno]
    one_dva <- dva$one_dva[ppno]
    
    idx <- which(dva.amp$participant == pp)
    
    linmod <- lm(norm_percept ~ framedistance_rel, data=dva.amp[idx,])
    dva$amp.slope[ppno] <- linmod[['coefficients']][['framedistance_rel']]
    
    dva.amp$framedistance_rel[idx] <- dva.amp$framedistance_rel[idx] / one_dva
    dva.amp$norm_percept[idx]      <- dva.amp$norm_percept[idx]      / one_dva
    
  }
  
  #points(dva.amp$framedistance_rel, dva.amp$norm_percept, col=transcolors[1])
  one_dva <- dva$one_dva
  amp.slope <- dva$amp.slope
  
  points(one_dva, amp.slope, col=transcolors[1])
  
  D2S <- lm(amp.slope ~ one_dva)
  print(summary(D2S))
  
  coef <- D2S$coefficients
  at <- c(0.013, 0.136)
  lines(at, coef[1]+(at*coef[2]), col=as.character(solidcolors[1]))
  
  ci <- predict( D2S,
                 newdata=data.frame(one_dva=seq(.013,.136,.001)),
                 interval = "confidence")
  
  X <- c(seq(.013,.136,.001),rev(seq(.013,.136,.001)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=as.character(transcolors[1]),border=NA)
  
  
  
  text(0.1,3,sprintf('N=%d',dim(dva)[1]))
  
  axis(side=1, at=c(0,.05,.1,.15))
  axis(side=2, at=c(0,1,2,3))
  
  
  # # # # # # # # # # # # # # # # #
  # WALLACH DATA SUBPLOT
  # # # # # # # # # # # # # # # # #
  
  plot(-1000, -1000, main='vertically offset dots', xlab='frame movement [% frame size]', ylab='percieved distance [% frame size]', xlim=c(0.3,0.8), ylim=c(0.3,0.8), ax=F, bty='n', asp=1)
  
  lines(c(0.35, 0.8), c(0.35, 0.8), col='gray', lty=2)
  
  wallach <- data[['wallach']]
  
  N <- length(unique(wallach$participant))
  
  # get frame speed to split conditions
  
  wallach$frame_velocity <- round(wallach$framedistance_rel / wallach$period_s, digits=4)
  
  # get period and speed for constant speed and constant period data
  base_time <- unique(wallach$period_s)[2]
  base_speed <- round(unique(wallach$framedistance_rel)[2] / base_time, digits = 4)
  
  # wallach <- wallach[which(wallach$norm_percept > 0),]
  # wallach <- wallach[which(wallach$norm_percept < (0.54/0.245)),]
  
  # get data frames for constant speed and constant period lines:
  const_speed  <- wallach[which(wallach$frame_velocity == base_speed),]
  const_period <- wallach[which(wallach$period_s == base_time),]
  
  
  agg.speed    <- aggregate(norm_percept ~ framedistance_rel, data=const_speed, FUN=mean)
  agg.period   <- aggregate(norm_percept ~ framedistance_rel, data=const_period, FUN=mean)
  
  agg.speed.CI    <- aggregate(norm_percept ~ framedistance_rel, data=const_speed, FUN=SMCL::getConfidenceInterval, method='b')
  agg.period.CI   <- aggregate(norm_percept ~ framedistance_rel, data=const_period, FUN=SMCL::getConfidenceInterval, method='b')
  
  Ys  <- c(agg.speed.CI[,2][,1], rev(agg.speed.CI[,2][,2]))
  Yv  <- c(agg.period.CI[,2][,1], rev(agg.period.CI[,2][,2]))
  
  x <- sort(unique(agg.speed$framedistance_rel))
  
  X <- c(x, rev(x))
  polygon(X, Ys, border=NA, col=transcolors[1])
  polygon(X, Yv, border=NA, col=transcolors[2])
  
  lines(x=agg.speed$framedistance_rel, y=agg.speed$norm_percept, col=solidcolors[1])
  lines(x=agg.period$framedistance_rel, y=agg.period$norm_percept, col=solidcolors[2])
  
  #legend(.3,.8, legend=c('constant velocity', 'constant period'), col=solidcolors, lw=1, lty=1, bty='n')
  text(.65,.35,sprintf('N=%d',N))
  
  axis(side=1, at=c(.4,.5,.6,.7,.8))
  axis(side=2, at=c(.4,.5,.6,.7,.8))
  
  
  # # # # # # # # # # # # # # # # #
  # CATCH TRIALS SUBPLOT
  # # # # # # # # # # # # # # # # #
  
  plot(-1000, -1000, main='catch trials (no frame)', xlab='reported dot offsest [% frame size]', ylab='relative distribution density', xlim=c(-1.5,1.5), ylim=c(0,.12), ax=F, bty='n')
  
  catch <- data[['catch']]
  catch$distancepercept_rel <- catch$distancepercept_rel / .245
  N <- length(unique(catch$participant))
  
  catch0 <- catch[which(catch$horizontal_offset == 0),]
  catch1 <- catch[which(abs(catch$horizontal_offset) == 0.15),]
  #str(catch0)
  #print(range(catch0$distancepercept_rel))
  catch0dist <- density(catch0$distancepercept_rel, n=301, from=-1.5, to=1.5, )$y
  catch0dist <- catch0dist / sum(catch0dist)
  #print(max(catch0dist))
  #str(catch1)
  #print(range(catch1$distancepercept_rel))
  catch1dist <- density(catch1$distancepercept_rel, n=301, from=-1.5, to=1.5, )$y
  catch1dist <- catch1dist / sum(catch1dist)
  #print(max(catch1dist))
  
  x <- seq(-1.5,1.5,3/300)
  X <- c(-1, x, 1)
  
  polygon(X, c(0, catch0dist, 0), col=as.character(transcolors[3]), border=NA)
  polygon(X, c(0, catch1dist, 0), col=as.character(transcolors[4]), border=NA)
  
  lines(x, catch0dist, col=as.character(solidcolors[3]))
  lines(x, catch1dist, col=as.character(solidcolors[4]))
  
  text(.3,.25,sprintf('N=%d',N))
  
  axis(side=1, at=c(-1.5,(-.15/.245),0.,(.15/.245),1.5),labels=c('-1.5','offset','0','offset','1.5'))
  axis(side=2, at=c(0,.03,.06,.09,.12))
  
  
}

plotFIPSperceptDistributions <- function() {
  
  data <- summarizeFIPS()
  
  #plot(-1000, -1000, main='', xlab='frame movement [nsu]', ylab='percieved distance [nsu]', xlim=c(0.0,0.3), ylim=c(0.0, 0.3), ax=F, bty='n', asp=1)
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
   
  # agg.amp    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=median)
  # agg.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=median)
  
  # norm_percept goes from 0 to 1.15
  X <- seq(0,1.15,.01)
  
  layout( matrix(c(1:10), ncol=2, byrow = T) )
  
  for (framedist in c(0.10, 0.12, 0.14, 0.16, 0.18)) {
    
    
    plot(-1000, -1000, main=sprintf('constant velocity %0.02f',framedist), xlab='percieved distance [nsu]', ylab='relative density', xlim=c(0.0,1.15), ylim=c(0.0, 1), ax=F, bty='n')      
    
    percepts <- amp$norm_percept[which(amp$framedistance_rel == framedist)]
    pd <- density(percepts, n=116, from=0, to=1.15)$y
    pd <- pd / max(pd)
    
    lines(X, pd)
    
    med <- median(percepts)
    sd <- sum(abs(percepts - med))/length(percepts)
    for (mult in c(0,1,2,3,4,5)) {
      lines(x=rep(med+(sd*mult),2), y=c(0,1), col='red')
    }
    
    
    axis(side=2, at=seq(0,1))
    axis(side=1, at=c(0,.20,.40,.60,1))
    
    
    
    
    plot(-1000, -1000, main=sprintf('constant period %0.02f',framedist), xlab='percieved distance [nsu]', ylab='relative density', xlim=c(0.0,1.15), ylim=c(0.0, 1), ax=F, bty='n')      
    
    percepts <- ampvel$norm_percept[which(ampvel$framedistance_rel == framedist)]
    pd <- density(percepts, n=116, from=0, to=1.15)$y
    pd <- pd / max(pd)

    lines(X, pd)
    
    med <- median(percepts)
    sd <- sum(abs(percepts - med))/length(percepts)
    for (mult in c(0,1,2,3,4,5)) {
      lines(x=rep(med+(sd*mult),2), y=c(0,1), col='red')
    }
    
    axis(side=2, at=seq(0,1))
    axis(side=1, at=c(0,.20,.40,.60,1))
    
    
  }
  
  # 
  # axis(side=1, at=seq(.08,.24,.04))
  # axis(side=2, at=seq(.08,.24,.04))
  
}

getDemographics <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  datalist <- summarizeFIPS()
  data.pp <- unique(datalist[['amp']]$participant)
  
  participants <- participants[which(participants$participant %in% data.pp),]
  
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

figureData <- function() {

  data <- summarizeFIPS()
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
  
  agg.amp    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=mean)
  agg.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=mean)
  
  agg.amp.CI    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=SMCL::getConfidenceInterval, method='b')
  agg.ampvel.CI <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=SMCL::getConfidenceInterval, method='b')
  
  agg.amp.sd    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=sd)
  agg.ampvel.sd <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=sd)
  
  names(agg.amp) <- c('framemovement_nsu', 'dotpercept_nsu')
  names(agg.ampvel) <- c('framemovement_nsu', 'dotpercept_nsu')
  
  agg.amp$condition    <- 'constant_velocity'
  agg.ampvel$condition <- 'constant_period'
  
  agg.amp$sd <- agg.amp.sd$norm_percept
  agg.ampvel$sd <- agg.ampvel.sd$norm_percept
  
  agg.amp$CIlo <- agg.amp.CI[,2][,1]
  agg.amp$CIhi <- agg.amp.CI[,2][,2]
  agg.ampvel$CIlo <- agg.ampvel.CI[,2][,1]
  agg.ampvel$CIhi <- agg.ampvel.CI[,2][,2]
  
  agg <- rbind(agg.amp, agg.ampvel)
  
  write.csv(agg, 'data/aggregate_FrameDot_data.csv', row.names=F)
  
}
