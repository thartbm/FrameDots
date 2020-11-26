library('afex')
#library('ez')
#library('lme4')
library('lmerTest')

summarizeFIPS <- function() {
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  nullresponse <- 0
  
  amp     <- NA
  ampvel  <- NA
  wallach <- NA
  catch   <- NA
  DVAinfo <- NA
  
  # do people with low illusion strength in the main condition, have higher illusion strength in wallach trials?
  amp.zero     <- NA
  amp.2zero    <- NA
  ampvel.zero  <- NA
  ampvel.2zero <- NA
  wallach.zero <- NA
  allFIPS      <- NA
  
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
    
    # speed
    ppFIPS$velocity <- ppFIPS$framedistance_rel / (ppFIPS$period_s / 2)
    # period: period_s
    
    frame_size <- 0.245
    # outer frame: 0.245 nsu
    # inner frame: 0.225 nsu
    # dot width & height: 0.025 nsu
    

    ppFIPS$norm_percept      <- ppFIPS$norm_percept / frame_size
    ppFIPS$framedistance_rel <- ppFIPS$framedistance_rel / frame_size
    
    pp.amp     <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp')),], FUN=mean)
    pp.ampvel  <- aggregate(norm_percept ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp+vel')),], FUN=mean)
    pp.12FIPS  <- ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp', 'amp+vel')), c('trialtype', 'norm_percept', 'framedistance_rel', 'period_s', 'velocity')]
    pp.wallach <- aggregate(norm_percept ~ framedistance_rel + period_s, data=ppFIPS[which(ppFIPS$trialtype == 'Wallach'),], FUN=mean)
    #pp.catch  <- aggregate(norm_percept ~ horizontal_offset + vertical_offset, data=ppFIPS[which(ppFIPS$trialtype == 'catch'),], FUN=mean)
    pp.catch   <- ppFIPS[which(ppFIPS$trialtype == 'catch'),c('horizontal_offset', 'distancepercept_rel', 'participant')]
    #str(pp.catch)
    #str(pp.wallach)
    pp.amp     <- aggregate(cbind(norm_percept, period_s, velocity) ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp')),], FUN=mean)
    pp.ampvel  <- aggregate(cbind(norm_percept, period_s, velocity) ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp+vel')),], FUN=mean)
    
    
    # add participant info to data:
    pp.amp$participant <- pp
    pp.ampvel$participant <- pp
    pp.12FIPS$participant <- pp
    dva$participant <- pp
    pp.wallach$participant <- pp
    
    if (is.data.frame(allFIPS)) {
      allFIPS <- rbind(allFIPS, pp.12FIPS)
    } else {
      allFIPS <- pp.12FIPS
    }
    
    #if (any(pp.amp$norm_percept == 0) | any(pp.ampvel$norm_percept == 0)) {
    
    if (any(pp.12FIPS$norm_percept == 0)) {
      nullresponse <- nullresponse + 1
      
      if (all(pp.12FIPS < (0.54/frame_size)) & length(which(pp.12FIPS$norm_percept <= 0)) <= 2) {
        
        pp2.amp     <- aggregate(cbind(norm_percept, period_s, velocity) ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp') & ppFIPS$norm_percept > 0),], FUN=mean)
        pp2.ampvel  <- aggregate(cbind(norm_percept, period_s, velocity) ~ framedistance_rel, data=ppFIPS[which(ppFIPS$trialtype %in% c('base', 'amp+vel')  & ppFIPS$norm_percept > 0),], FUN=mean)
        pp2.amp$participant <- pp
        pp2.ampvel$participant <- pp
        
        if (is.data.frame(amp.zero)) {
          amp.2zero     <- rbind(amp.2zero, pp2.amp)
          ampvel.2zero  <- rbind(ampvel.2zero, pp2.ampvel)
        } else {
          amp.2zero     <- pp2.amp
          ampvel.2zero  <- pp2.ampvel
        }
        # next()?
      }
      
      if (is.data.frame(amp.zero)) {
        amp.zero     <- rbind(amp.zero, pp.amp)
        ampvel.zero  <- rbind(ampvel.zero, pp.ampvel)
      } else {
        amp.zero     <- pp.amp
        ampvel.zero  <- pp.ampvel
      }
      
      if (any(pp.wallach$norm_percept == 0 | pp.wallach$norm_percept > (0.54/frame_size))) {
        # do we need to do anything here?
      } else {
        
        if (is.data.frame(wallach.zero)) {
          wallach.zero <- rbind(wallach.zero, pp.wallach)
        } else {
          wallach.zero <- pp.wallach
        }
        
      }
      
      
    } else if (any(pp.12FIPS$norm_percept > (0.54/frame_size))) {
      nullresponse <- nullresponse + 1
    } else {
      
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
        
        if (is.data.frame(wallach)) {
          wallach <- rbind(wallach, pp.wallach)
        } else {
          wallach <- pp.wallach
        }
        
      }
      
    }
    
  }
  
  return(list('amp'=amp, 'ampvel'=ampvel, 'wallach'=wallach, 'dva'=DVAinfo, 'catch'=catch, 'wallach-zero'=wallach.zero, 'amp-zero'=amp.zero, 'ampvel-zero'=ampvel.zero, 'allFIPS'=allFIPS, 'amp-2zero'=amp.2zero, 'ampvel-2zero'=ampvel.2zero))
  
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
  
  
  # amp <- data[['amp']]
  # ampvel <- data[['ampvel']]
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
  amp20 <- data[['amp-2zero']]
  ampvel20 <- data[['ampvel-2zero']]
  amp <- rbind(amp, amp20)
  ampvel <- rbind(ampvel, ampvel20)
  
  
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
  
  plot(-1000, -1000, main='vertically aligned dots', xlab='frame size [dva]', ylab='percieved distance slope', xlim=c(0,20), ylim=c(-.2,3), ax=F, bty='n')
  
  polygon(x=c(7.5,9,9,7.5),y=c(0,0,3,3),col='#EEEEEE',border=NA)
  lines(c(1,19),c(1,1),col='#999999',lty=2)
  
  dva <- data[['dva']]
  # get participants who say they were carefull in calibration:
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  calibrated <- participants$participant[which(participants$card_used == "Yes" & participants$distance_method == "measured")]
  dva <- dva[which(dva$participant %in% calibrated & dva$participant %in% unique(amp$participant)),]
  
  str(dva)
  
  dva$one_dva <- (dva$one_dva_height + dva$one_dva_width) / 2
  dva <- dva[which(dva$one_dva > 0),]
  
  print(dva$one_dva)
  dva.amp    <- amp[   which(amp$participant    %in% dva$participant),]
  dva.ampvel <- ampvel[which(ampvel$participant %in% dva$participant),]
  
  dva.amp$norm_percept      <- dva.amp$norm_percept * .245
  dva.amp$framedistance_rel <- dva.amp$framedistance_rel * .245
  dva.ampvel$norm_percept      <- dva.ampvel$norm_percept * .245
  dva.ampvel$framedistance_rel <- dva.ampvel$framedistance_rel * .245
  
  dva$amp.slope <- NA
  cat('set slope to be NA\n')
  
  for (ppno in c(1:length(dva$participant))) {
    
    pp <- dva$participant[ppno]
    one_dva <- dva$one_dva[ppno]
    
    print(pp)
    
    idx1 <- which(dva.amp$participant    == pp)
    idx2 <- which(dva.ampvel$participant == pp)
    
    linmod <- lm(norm_percept ~ framedistance_rel, data=rbind(dva.amp[idx1,],dva.ampvel[idx2,]))
    print(linmod[['coefficients']][['framedistance_rel']])
    dva$amp.slope[ppno] <- linmod[['coefficients']][['framedistance_rel']]
    
    #dva.amp$framedistance_rel[idx] <- dva.amp$framedistance_rel[idx] / one_dva
    #dva.amp$norm_percept[idx]      <- dva.amp$norm_percept[idx]      / one_dva
    
  }
  
  cat('alle proefpersonen gesloopt\n')
  
  #points(dva.amp$framedistance_rel, dva.amp$norm_percept, col=transcolors[1])
  one_dva <- .245 / (dva$one_dva)
  amp.slope <- dva$amp.slope
  
  print(range(one_dva))
  
  points(one_dva, amp.slope, col=transcolors[1])
  
  D2S <- lm(amp.slope ~ one_dva)
  print(summary(D2S))
  
  coef <- D2S$coefficients
  #at <- c(0.013, 0.136)
  at <- c(1.8, 18.6)
  lines(at, coef[1]+(at*coef[2]), col=as.character(solidcolors[1]))
  
  ci <- predict( D2S,
                 newdata=data.frame(one_dva=seq(1.8,18.6,.1)),
                 interval = "confidence")
  
  #X <- c(seq(.013,.136,.001),rev(seq(.013,.136,.001)))
  X <- c(seq(1.8,18.6,.1),rev(seq(1.8,18.6,.1)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=as.character(transcolors[1]),border=NA)
  
  
  
  text(15,3,sprintf('N=%d',dim(dva)[1]))
  
  axis(side=1, at=c(0,5,10,15,20))
  axis(side=2, at=c(0,1,2,3))
  
  
  # # # # # # # # # # # # # # # # #
  # WALLACH DATA SUBPLOT
  # # # # # # # # # # # # # # # # #
  
  plot(-1000, -1000, main='vertically offset dots\n[participants without main illusion]', xlab='frame movement [% frame size]', ylab='percieved distance [% frame size]', xlim=c(0.3,0.8), ylim=c(0.3,0.8), ax=F, bty='n', asp=1)
  
  lines(c(0.35, 0.8), c(0.35, 0.8), col='gray', lty=2)
  
  #wallach <- data[['wallach']]
  wallach <- data[['wallach-zero']]
  
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
  
  amp$framedistance_rel <- amp$framedistance_rel * .245
  ampvel$framedistance_rel <- ampvel$framedistance_rel * .245
  
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
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # zero-excluded participants' Wallach distributions
  # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  wallach <- data[['wallach-zero']]
  wallach$framedistance_rel <- wallach$framedistance_rel * .245
  #wallach$framedistance_rel <- round(wallach$framedistance_rel, digits=5)
  
  wallach$velocity <- round(wallach$framedistance_rel / wallach$period_s, digits=6)
  
  print(unique(wallach$velocity))
  
  # split into wallach and wallachvel
  wallachvel <- wallach[which(wallach$velocity == 0.2),]
  wallach    <- wallach[which(wallach$period_s == 0.7),]

  str(wallach)
  
  X <- seq(0,2,.01)
  
  layout( matrix(c(1:6), ncol=2, byrow = T) )
  
  for (framedist in c(0.10, 0.14, 0.18)) {
    
    plot(-1000, -1000, main=sprintf('constant velocity %0.02f',framedist), xlab='percieved distance [nsu]', ylab='relative density', xlim=c(0.0,2), ylim=c(0.0, 1), ax=F, bty='n')      
    
    percepts <- wallach$norm_percept[which(wallach$framedistance_rel == framedist)]
    pd <- density(percepts, n=201, from=0, to=2)$y
    pd <- pd / max(pd)
    
    lines(X, pd)
    
    med <- median(percepts)
    sd <- sum(abs(percepts - med))/length(percepts)
    for (mult in c(0,1,2,3,4,5)) {
      lines(x=rep(med+(sd*mult),2), y=c(0,1), col='red')
    }
    
    
    axis(side=2, at=seq(0,1))
    axis(side=1, at=c(0,.20,.40,.60,1,2))
    
    
    
    
    plot(-1000, -1000, main=sprintf('constant period %0.02f',framedist), xlab='percieved distance [nsu]', ylab='relative density', xlim=c(0.0,2), ylim=c(0.0, 1), ax=F, bty='n')      
    
    percepts <- wallachvel$norm_percept[which(wallachvel$framedistance_rel == framedist)]
    pd <- density(percepts, n=201, from=0, to=2)$y
    pd <- pd / max(pd)
    
    lines(X, pd)
    
    med <- median(percepts)
    sd <- sum(abs(percepts - med))/length(percepts)
    for (mult in c(0,1,2,3,4,5)) {
      lines(x=rep(med+(sd*mult),2), y=c(0,1), col='red')
    }
    
    axis(side=2, at=seq(0,1))
    axis(side=1, at=c(0,.20,.40,.60,1,2))
    
    
  }
  
  
}


plotRawZeroData <- function() {
  
  datalist <- summarizeFIPS()
  
  # participants excluded because of a zero-response
  amp.zero <- datalist[['amp-zero']]
  ampvel.zero <- datalist[['ampvel-zero']]
  
  # participants who got included
  amp <- datalist[['amp']]
  ampvel <- datalist[['ampvel']]
  
  # get the wallach responses for the zero-excluded participants:
  wallach.zero <- datalist[['wallach-zero']]
  
  CI <- SMCL::getConfidenceInterval(c(amp$norm_percept,ampvel$norm_percept),method='b')
  #CIr <- SMCL::getConfidenceInterval(c(amp$norm_percept,ampvel$norm_percept),method='b',FUN=range)
  #print(CIr)
  
  LIP <- c()
  
  minmax <- range(c(amp$norm_percept,ampvel$norm_percept))
  
  participants <- unique(c(amp.zero$participant, ampvel.zero$participant))
  N <- length(participants)
  
  layout(matrix(c(1,2,1,3),ncol=2,byrow = T))
  
  plot(-1000,-1000,main='',xlab='participant',ylab='perceptual responses [% frame]', xlim=c(0,N+1), ylim=c(0,max(c(amp.zero$norm_percept,ampvel.zero$norm_percept))),bty='n')
  
  polygon(c(0,0,N+1,N+1),c(CI,rev(CI)),col='#999999',border=NA)
  
  lines(c(0,N+1),rep(minmax[1],2),col='#000000',lty=1)
  lines(c(0,N+1),rep(minmax[2],2),col='#000000',lty=1)
  
  for (ppno in c(1:length(participants))) {
    
    participant <- participants[ppno]
    
    pp.amp    <- amp.zero$norm_percept[which(amp.zero$participant == participant)]
    pp.ampvel <- ampvel.zero$norm_percept[which(ampvel.zero$participant == participant)]
    
    points(rep(ppno-.25,length(pp.amp)),pp.amp,col=rgb(229, 22,  54,  47,  max = 255))
    points(rep(ppno+.25,length(pp.ampvel)),pp.ampvel,col=rgb(136, 153, 255, 47,  max = 255))
    
    
    non.zeroes <- c(pp.amp,pp.ampvel)[which(c(pp.amp,pp.ampvel) > 0)]
    if (length(non.zeroes) == 0) {
      next
    }
    avg.percept <- mean(non.zeroes)
    points(ppno, avg.percept, col=rgb(127, 0,   216, 255, max = 255))
    
    if (avg.percept < CI[1]) {
      LIP <- c(LIP, participant)
      points(ppno, avg.percept, col=rgb(127, 0,   216, 255, max = 255),pch=16)
      lines(c(ppno,ppno),range(non.zeroes), col=rgb(127, 0,   216, 255, max = 255))
    } else {
      points(ppno, avg.percept, col=rgb(127, 0,   216, 255, max = 255))
    }
    
  }
  
  # 19 participants with low scores
  # 16 are in the wallach-zero data
  
  # plot the wallach responses for low performers
  participants <- LIP[which(LIP %in% wallach.zero$participant)]
  N <- length(participants)
  
  plot(-1000,-1000,main='',xlab='participant',ylab='perceptual responses [% frame]', xlim=c(0,N+1), ylim=c(0,2),bty='n',ax=F)
  
  for (ppno in c(1:N)) {
  
    percepts <- wallach.zero$norm_percept[which(wallach.zero$participant == participants[ppno])]  
    points(rep(ppno,length(percepts)),percepts)
    
  }
  
  axis(side=1,at=seq(1,N))
  axis(side=2,at=c(0,0.5,1,1.5,2))
  
  # 
  
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(136, 153, 255, 255, max = 255),
                   rgb(127, 0,   216, 255, max = 255),
                   rgb(255, 147, 41,  255, max = 255))
  
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255),
                   rgb(127, 0,   216, 47,  max = 255),
                   rgb(255, 147, 41,  47,  max = 255))
  
  
  plot(-1000, -1000, main='vertically offset dots\n[participants without main illusion]', xlab='frame movement [% frame size]', ylab='percieved distance [% frame size]', xlim=c(0.1,0.8), ylim=c(0.1,0.8), ax=F, bty='n', asp=1)
  
  lines(c(0.35, 0.8), c(0.35, 0.8), col='gray', lty=2)
  
  wallach <- datalist[['wallach-zero']]
  wallach <- wallach[which(wallach$participant %in% participants),]
  
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
  text(.65,.2,sprintf('N=%d',N))
  
  axis(side=1, at=c(.2,.3,.4,.5,.6,.7,.8))
  axis(side=2, at=c(.2,.3,.4,.5,.6,.7,.8))
  
  
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
  
  cat('task info:\n')
  
  cat(sprintf('  completion time: %0.1f', median(participants$completiontime)))
  
}

figureData <- function() {

  data <- summarizeFIPS()
  
  amp <- data[['amp']]
  ampvel <- data[['ampvel']]
  amp20 <- data[['amp-2zero']]
  ampvel20 <- data[['ampvel-2zero']]
  
  amp <- rbind(amp, amp20)
  ampvel <- rbind(ampvel, ampvel20)
  
  N <- length(unique(amp$participant))
  
  print(N)
  
  agg.amp    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=mean)
  agg.ampvel <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=mean)
  
  agg.amp.CI    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=SMCL::getConfidenceInterval, method='b')
  agg.ampvel.CI <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=SMCL::getConfidenceInterval, method='b')
  
  agg.amp.sd    <- aggregate(norm_percept ~ framedistance_rel, data=amp, FUN=sd)
  agg.ampvel.sd <- aggregate(norm_percept ~ framedistance_rel, data=ampvel, FUN=sd)
  
  names(agg.amp) <- c('framemovement_pf', 'dotpercept_pf')
  names(agg.ampvel) <- c('framemovement_pf', 'dotpercept_pf')
  
  agg.amp$condition    <- 'constant_velocity'
  agg.ampvel$condition <- 'constant_period'
  
  agg.amp$sd <- agg.amp.sd$norm_percept
  agg.amp$sem <- agg.amp.sd$norm_percept / sqrt(N)
  agg.ampvel$sd <- agg.ampvel.sd$norm_percept
  agg.ampvel$sem <- agg.ampvel.sd$norm_percept / sqrt(N)
  
  agg.amp$CIlo <- agg.amp.CI[,2][,1]
  agg.amp$CIhi <- agg.amp.CI[,2][,2]
  agg.ampvel$CIlo <- agg.ampvel.CI[,2][,1]
  agg.ampvel$CIhi <- agg.ampvel.CI[,2][,2]
  
  agg <- rbind(agg.amp, agg.ampvel)
  
  write.csv(agg, 'data/aggregate_FrameDot_data.csv', row.names=F)
  
}


plotNonZeroDistributionWidths <- function() {
  
  datalist <- summarizeFIPS()
  
  FIPS <- datalist[['allFIPS']]
  
  participants <- unique(FIPS$participant)
  N <- length(participants)
  
  belowBounds  <- NA
  aboveBounds  <- NA
  outOfBounds  <- NA
  # and of course:
  withinBounds <- NA
  
  plot(-1000,-1000,
       main='',xlab='participant',ylab='percept [prop. frame size]',
       xlim=c(0,N+1),ylim=c(0,5),
       bty='n',ax=F)
  
  lines(c(0,205),c(0,0),col='green')
  lines(c(0,205),c((0.54 / .245),(0.54 / .245)),col='green')
  
  upr <- 0
  stds <- c()
  
  zero2 <- 0
  
  for (ppno in c(1:N)) {
    
    participant <- participants[ppno]
    
    ppFIPS <- FIPS[which(FIPS$participant == participant),]
    
    below <- F
    above <- F
    
    if (any(ppFIPS$norm_percept <= 0)) {
      below <- T
    }
    if (any(ppFIPS$norm_percept > (0.54 / .245))) {
      above <- T
    }
    below <- length(which(ppFIPS$norm_percept <=  0            ))
    above <- length(which(ppFIPS$norm_percept  > (0.54 / .245) ))
    
    # do we need this:
    within <- length(which(ppFIPS$norm_percept > 0 & ppFIPS$norm_percept <= (0.54 / .245) ))
    
    
    idx <- which(ppFIPS$norm_percept > 0 & !is.nan(ppFIPS$norm_percept))
    percepts <- as.numeric(ppFIPS$norm_percept[idx])
    
    # if (length(percepts) < 12) {
    #   print(length(percepts))
    # }
    
    col <- 'gray'
    
    if (above & below) {
      col <- 'red'
    } else if (above) {
      col <- 'orange'
    } else if (below) {
      col <- 'purple'
      if (length(percepts) >= 10) {
        col <- 'black'
        zero2 <- zero2 + 1
      }
    }
    

    avg <- mean(percepts)
    std <- sd(percepts)
    CI <- SMCL::getConfidenceInterval(percepts)
    
    
    if (col %in% c('red', 'orange', 'purple')) {
      if (!is.na(std) & std < 0.512915) {
        col <- 'blue'
        if (within <= 10 & avg < (0.54 / .245)) {
          col <- 'cyan'
        }
      }
    }
    
    lines(c(ppno,ppno),c(avg-std,avg+std), col=col)
    points(ppno,avg,col=col)
    
    #upr <- max(upr, max(CI, na.rm=T))
    
    if (!is.na(std)) {
      upr <- max(upr, std+avg)
    }
    
    if (col == 'gray') {
      stds <- c(stds, std)
    }
    
  }
  
  print(upr)
  
  print(N)
  
  print(max(stds))
  
  print(zero2)
  
  axis(side=1, at=c(0,50,100,150,200))
  axis(side=2, at=c(0,1,2,3,4,5))
  
}

statsFIPS <- function() {
  
  datalist <- summarizeFIPS()
  
  amp2zero    <- datalist[['amp-2zero']]
  ampvel2zero <- datalist[['ampvel-2zero']]
  amp         <- datalist[['amp']]
  ampvel      <- datalist[['ampvel']]
  
  amp2zero$condition    <- 'constant_velocity'
  ampvel2zero$condition <- 'constant_period'
  amp$condition         <- 'constant_velocity'
  ampvel$condition      <- 'constant_period'
  
  FIPS                  <- rbind(amp, ampvel)
  FIPS$condition         <- as.factor(FIPS$condition)
  
  print(summary(lm(norm_percept ~ framedistance_rel + condition, data=FIPS)))
  
  FIPS20                <- rbind(amp2zero, ampvel2zero)
  FIPS20$condition      <- as.factor(FIPS20$condition)
  
  print(summary(lm(norm_percept ~ framedistance_rel + condition, data=rbind(FIPS, FIPS20))))
  
  
  FIPS$condition         <- as.factor(FIPS$condition)
  FIPS$framedistance_rel <- as.factor(FIPS$framedistance_rel)
  FIPS20 <- rbind(amp2zero, ampvel2zero) 
  
  fit_FIPS <- aov_ez("participant","norm_percept",data=FIPS,within=c("condition","framedistance_rel"))
  print(nice(fit_FIPS))
  
  # print(ezANOVA(data=FIPS,
  #               dv=norm_percept,
  #               wid=participant,
  #               within=c("condition","framedistance_rel")))
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  
  FIPS_lmer <- lmerTest::lmer(norm_percept ~ framedistance_rel * condition - (1|participant),
                              na.action = na.exclude,
                              data = rbind(FIPS, FIPS20),
                              REML = TRUE,
                              control = lmerControl(optimizer ="Nelder_Mead")
  )
  
  print(summary(FIPS_lmer))
  
  #plot(FIPS_lmer)
  
  print(anova(FIPS_lmer,ddf='Satterthwaite',type=3))
  
}
