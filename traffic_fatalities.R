# DataViz assignment 1: R+git
# Victor Vilchis Tella 

library(tidyr)
library(readr)
library(haven)
library(dplyr)
library(stringr)
library(ggplot2)

acc2015 <- read_csv('accident.csv')
acc2014 <- read_sas('accident.sas7bdat')

### Combining
acc2014 <- acc2014 %>% mutate(TWAY_ID2 = na_if(x=TWAY_ID2, y=""))

#Present in 2015, missing in 2014: "RUR_URB"  "FUNC_SYS" "RD_OWNER"
colnames(acc2015)[!(colnames(acc2015) %in% colnames(acc2014))]

#Present in 2014, missing in 2015: "ROAD_FNC"
colnames(acc2014)[!(colnames(acc2014) %in% colnames(acc2015))]

acc <- bind_rows(acc2014, acc2015)

acc %>% count(RUR_URB)
#There are 30056 NA values because there are 30056 columns in acc2014 which
#did not have this variable.

### Joining with another source

fips <- read_csv('fips.csv')
acc <- acc %>% mutate(STATE = as.character(STATE), COUNTY = as.character(COUNTY))
acc <- acc %>% mutate(STATE = str_pad(STATE, width=2, side='left', pad='0'))
acc <- acc %>% mutate(COUNTY = str_pad(COUNTY, width=3, side='left', pad='0'))
acc <- acc %>% rename(StateFIPSCode=STATE, CountyFIPSCode=COUNTY)

acc <- acc %>% left_join(fips)

### Exploring
TOTAL <- sum(acc$FATALS)

agg <- summarize(group_by(acc,StateFIPSCode,YEAR), fatals=sum(FATALS))
agg_wide <- spread(agg,key=YEAR,value=fatals)
agg_wide <- agg_wide %>% rename(fats2015=`2015`, fats2014=`2014`)

agg_wide <- agg_wide %>% mutate(PercDiff=(fats2015-fats2014)/fats2014*100)
agg_wide <- arrange(agg_wide, desc(PercDiff))
large_diff <- filter(agg_wide, PercDiff >= 15)
large_diff <- filter(large_diff, !is.na(StateFIPSCode))

agg <- summarize(group_by(acc,StateFIPSCode,YEAR), fatals=sum(FATALS)) %>% 
  spread(key=YEAR,value=fatals) %>%
  rename(fats2015=`2015`, fats2014=`2014`) %>%
  mutate(PercDiff=(fats2015-fats2014)/fats2014*100) %>%
  arrange(desc(PercDiff)) %>%
  filter(PercDiff >= 15) %>%
  filter(!is.na(StateFIPSCode))

