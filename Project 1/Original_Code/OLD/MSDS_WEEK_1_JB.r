txt = readLines("C:/Users/Jayson/Documents/offline.final.trace.txt")

processLine = function(x)
{
tokens = strsplit(x, "[;=,]")[[1]]
if (length(tokens) == 10)
    return(NULL)
tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
byrow = TRUE), tmp)
}

lines = txt[ substr(txt, 1, 1) != "#" ]
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)

head(offline)

names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
"orientation", "mac", "signal",
"channel", "type")

# Only interested in type 3
# signal strewngth for the X Y position
head(offline)

offline[offline$time==1139643118358,]

library(tidyverse)
library(magrittr)
devtools::install_github("tidyverse/tidyr")

# 3 of the MACIDs are determined to be on another floor and not useful
# 2 of them are irrelevant
# CD and C0 are the ones
# Introducing CD and excluding C0
# Measuring them both
# Across all clustering methods

select(offline,-c(scanMac,channel,type))

offline[offline['posZ']!='0.0',]

vals = data.frame(table(offline['mac']))
vals[order(-vals$Freq),]

offline$signal %<>% as.integer

out<-select(offline, -c(channel,scanMac)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))

out$nas<-rowSums(is.na(out))

# 21 columns of MACID
# Remove some macID and get to 6 or 7 that provide the meat of our analysis
out

# create a list of the mac IDs and their types
macAndType <- unique(select(offline, c(mac,type)))
macAndType

macID3 <- c('00:14:bf:b1:97:8a'
            ,'00:14:bf:b1:97:90'
            ,'00:0f:a3:39:e1:c0'
            ,'00:14:bf:b1:97:8d'
            ,'00:14:bf:b1:97:81'
            ,'00:14:bf:3b:c7:c6'
            ,'00:0f:a3:39:dd:cd'
            ,'00:0f:a3:39:e0:4b'
            ,'00:0f:a3:39:e2:10'
            ,'00:04:0e:5c:23:fc'
            ,'00:30:bd:f8:7f:c5'
            ,'00:e0:63:82:8b:a9')

# create a new vector for type 3 
offlineReduced <- offline[offline$mac %in% macID3 ,]

# re-pivot the data
offlineReducedout<-select(offlineReduced, -c(channel,scanMac,type)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))

# Convert to DF
offlineReducedout <- data.frame(offlineReducedout)

