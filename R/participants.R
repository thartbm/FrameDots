
getParticipantsFromData <- function() {
  
  # filenames: 222988_FrameDots_2020-11-01_18h45.35.258
  # <participant>_FrameDots_<timestamp>.csv
  
  # get all .csv files from data folder:
  # csv_files <- Sys.glob(paths=c('data/*.csv'))
  csv_files <- list.files('data/Pavlovia/', pattern='*.csv')
  
  # will collect participants and timestamps in these lists:
  participant <- c()
  timestamp <- c()
  
  for (csv_file in csv_files) {
    
    # check how many trials in file? remove if not 22
    csv_lines <- readLines(sprintf('data/Pavlovia/%s', csv_file))
    if (length(csv_lines) < 23) {
      next
    }
    
    # analyse string to get participant ID and timestamp
    FDpos <- gregexpr(pattern='_FrameDots_', csv_file)[[1]][1]
    pp <- substr(csv_file, 1, FDpos-1)
    ts <- substr(csv_file, FDpos+11, nchar(csv_file)-4)
    
    participant <- c(participant, pp)
    timestamp <- c(timestamp, ts)

  }
  
  return(data.frame(participant, timestamp))
  
}

getParticipantsFromQualtrics <- function() {
  
  # remove the extra two header lines that mess up the format:
  df <- read.csv('data/Qualtrics_P1_full.csv', stringsAsFactors = F)
  df <- df[-c(1,2),]
  write.csv(df, file='data/Qualtrics_P1.csv', quote=T, row.names=F)
  cat('- prepped part 1 qualtrics data\n')
  # and for part 2:
  df <- read.csv('data/Qualtrics_P2_full.csv', stringsAsFactors = F)
  df <- df[-c(1,2),]
  write.csv(df, file='data/Qualtrics_P2.csv', quote=T, row.names=F)
  cat('- prepped part 2 qualtrics data\n')
  
  # read in the cleaned file:
  df <- read.csv('data/Qualtrics_P2.csv', stringsAsFactors = F)
  #print(dim(df))
    
  # want people who give informed consent:
  df <- df[which(df$Q1 == 'I agree to participate in this study'),]
  cat('- checked informed consent\n')
  
  
  # don't want people who did the task on their phone or tablet:
  df <- df[which(df$Q14 == 'Laptop/PC'),]
  #print(dim(df))
  cat('- select Laptop/PC only\n')
  
  
  # want people wearing their corrective devices if needed:
  if (length(which(df$Q20 == 'No')) > 0) {
    df <- df[-which(df$Q20 == 'No'),]
  }
  #print(dim(df))
  cat('- select normal or correct vision only\n')
  
  # we don't want participants who just clicked through the task:
  if (length(which(df$Q114 == 'Pressed space quickly to get through the task')) > 0) {
    df <- df[-which(df$Q114 == 'Pressed space quickly to get through the task'),]
  }
  #print(dim(df))
  cat('- select carefull responders\n')
  
  # we don't want data from participants who report their data might be useless:
  if (length(which(df$Q128 == 'Yes')) > 0) {
    df <- df[-which(df$Q128 == 'Yes'),]
  }
  #print(dim(df))
  cat('- select useful data\n')
  
  
  # we want to know if the participant measured, estimated or skipped the distance to screen part:
  # Q112
  #Did not use a measuring device or estimate
  #Estimated the distance
  #Used a measuring device
  
  # we want to know if they used a bank card:
  # Q113
  # Yes
  # No
  
  # remove participants who didn't answer these questions:
  #df <- df[which(df$Q112 %in% c('Yes', 'No')),]
  if (length(which(df$Q112.1 == "") > 0)) {
    df <- df[-which(df$Q112.1 == ""),]
  }
  if (length(which(df$Q113 == "") > 0)) {
    df <- df[-which(df$Q113 == ""),]
  }
  cat('- select people who anwered the calibration questions\n')

  # we want to know the 'group'?
  df <- df[,c('id', 'group', 'Finished', 'Q98', 'Q112.1', 'Q113')]
  names(df) <- c('participant', 'group', 'finished', 'redid', 'distance_method', 'card_used')
  
  #print(unique(df$distance_method))
  
  df$distance_method[which(df$distance_method == 'Estimated the distance')] <- 'estimated'
  df$distance_method[which(df$distance_method == 'Used a measuring device')] <- 'measured'
  df$distance_method[which(df$distance_method == 'Did not use a measuring device or estimate')] <- 'skipped'
  cat('- get calibration self-evaluation data\n')
  
  
  # remove double entries somewhat:
  doppelgangers <- unique(df$participant[duplicated(df$participant)])
  for (doppelganger in doppelgangers) {
    if (length(df$finished[which(df$participant == doppelganger)]) > 1) {
      df <- df[ -which( df$finished == "False" & df$participant == doppelganger ), ]
    }
  }
  
  
  
  # read in the cleaned file for the age/sex/handedness data:
  df1 <- read.csv('data/Qualtrics_P1.csv', stringsAsFactors = F)
  
  # age: Q4
  # sex: Q5
  # handedness: Q13
  P2cols <- list('age'='Q4', 'sex'='Q5', 'handedness'='Q13')
  
  for (dv in names(P2cols)) {
    
    #print(dv)
    colname <- P2cols[[dv]]
    df[,dv] <- NA
    colidx  <- which(names(df) == dv)
    colidx1 <- which(names(df1) == colname)
    
    # for every participant get that DV:
    for (ppidx in c(1:length(df$participant))) {
      
      ppid <- df$participant[ppidx]
      #print(ppid)
      
      # there is one match in df2: get the info!
      if (length( which(df1$id == ppid) ) == 1) {
        #print(df1[which(df1$id == ppid),colidx1])
        val <- df1[which(df1$id == ppid),colidx1]
        if (val == 'NA') {
          df <- df[-ppidx,]
        } else {
          df[ppidx,colidx] <- val
        }
        
      }
      
      # there is no match in df2: remove the participant
      if (length( which(df1$id == ppid) ) == 0) {
        df <- df[-ppidx,]
      }
      
      # what if there are multiple matches?
      if (length( which(df1$id == ppid)) > 1) {
        
        goodruns <- which(df1$id == ppid & df1$Finished == "True")
        
        # no good runs? remove participant...
        if ( length(goodruns) == 0 ) {
          df <- df[-ppidx,]
        }
        # 1 good run... get the info:
        if ( length(goodruns) > 0 ) {
          val <- df1[goodruns[1],colidx1]
          if (val == 'NA') {
            df <- df[-ppidx,]
          } else {
            df[ppidx,colidx] <- val
          }
        }
        
      }
      
    }
    
  }
  
  df <- df[-which(is.na(df$age)),]
  
  return(df)
  
}

getParticipants <- function() {
  
  # get participant info from both sources:
  data_pp <- getParticipantsFromData()
  cat('parsed pavlovia data\n')
  data_qq <- getParticipantsFromQualtrics()
  cat('parsed qualtrics data\n')

  # get those that match:
  pp <- unique(intersect(data_pp$participant, data_qq$participant))
  #print(length(pp))
  data_pp <- data_pp[which(data_pp$participant %in% pp),]
  data_qq <- data_qq[which(data_qq$participant %in% pp),]
  
  #print(dim(data_pp))
  #print(dim(data_qq))

  #print(table(data_qq$participant))
  # add timestamp column to data_qq
  data_qq$timestamp <- NA
  
  # these participants appear twice in Qualtrics:
  # 217966
  # 218593
  # not anymore... but how can people do the Qualtrics part twice?
  
  for (rowno in c(1:dim(data_qq)[1])) {
    
    ppid <- data_qq$participant[rowno]
    prows <- which(data_pp == ppid)
    if (length(prows) == 1) {
      
      data_qq$timestamp[rowno] <- as.character(data_pp$timestamp)[prows]
      
    } else {
      
      data_qq$timestamp[rowno] <- as.character(rev(sort(data_pp$timestamp[which(data_pp$participant == ppid)])))[1]
      
    }

  }
  
  write.csv(data_qq, 'data/participants.csv', row.names=F, quote=F)
  
  return(data_qq)

}