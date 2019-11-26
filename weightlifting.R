library(readxl)
library(dplyr)
library(caret)
library(zoo)
library(lubridate)
library(eeptools)
library(mlbench)
library(doParallel)

Book1 <- read_excel("Book1.xlsx")

## removing the = and " from the data
df1 <- lapply(Book1, gsub, pattern='=', replacement='')
df2 <- lapply(df1, gsub, pattern='"', replacement='')

## putting the data back into a data frame
df <- do.call(cbind.data.frame, df2)

## seeing if they medaled or not as a binary variable for a classification model
df <- df %>%
  mutate(medal = ifelse(df$rank %in% 1:3, "medal",
                        ifelse(df$rank_s %in% 1:3, "medal",
                               ifelse(df$rank_cj %in% 1:3, "medal",
                                      "no-medal"))))

## making binary variable for if they bombed or pass
df$total <- as.numeric(as.character(df$total))
df <- df%>%
  mutate(bomb = ifelse(as.numeric(df$total)>0, "pass","bomb"))

## year and month of athelete
df$year <- substr(df$born, 7, nchar(as.character(df$born)))
df$month <- substr(df$born, 4, 5)

## making a difference variable to see the how big their jumps were in snatch and cnj
## changing jerk and snatch to numeric

df$snatch1 <- as.numeric(as.character(df$snatch1))
df$snatch2 <- as.numeric(as.character(df$snatch2))
df$snatch3 <- as.numeric(as.character(df$snatch3))
df$snatch <- as.numeric(as.character(df$snatch))
df$jerk1 <- as.numeric(as.character(df$jerk1))
df$jerk2 <- as.numeric(as.character(df$jerk2))
df$jerk3 <- as.numeric(as.character(df$jerk3))
df$jerk <- as.numeric(as.character(df$jerk))

##finding difference of biggest snatch minus smallest
dfsnatch <- df %>%
  select(snatch1,snatch2,snatch3,snatch,name)
dfjerk <- df %>%
  select(jerk1,jerk2,jerk3,jerk,name)

## looking at the difference in jerk or snatch
dfjerk$diff_jerk <- dfjerk$jerk - apply(dfjerk[1:4], 1, FUN=min, na.rm = TRUE)
dfsnatch$diff_snatch <- dfsnatch$snatch - apply(dfsnatch[1:4], 1, FUN=min, na.rm = TRUE)

## merging and looking for nas
dftest <- merge(df, dfjerk)
dftest1 <- merge(dftest, dfsnatch)
df$diff_snatch <- dfsnatch$diff_snatch
df$diff_jerk <- dfjerk$diff_jerk
count_na <- function(x) sum(is.na(x))
df$snatch_misses <- dfsnatch %>%
  select(snatch1,snatch2,snatch3) %>%
  mutate(count_na = apply(., 1, count_na))
df$jerk_misses <- dfjerk %>%
  select(jerk1,jerk2,jerk3) %>%
  mutate(count_na = apply(., 1, count_na))

## looking at medalling and bombing
df$total_misses <- df$snatch_misses[4]+df$jerk_misses[4]
df$medal <- ifelse(df$medal == "medal", 1, 0)
df$bomb <- ifelse(df$bomb == "bomb", 1, 0)

df$diff_jerk <- ifelse(is.na(df$diff_jerk),0,df$diff_jerk)
df$diff_snatch <- ifelse(is.na(df$diff_snatch),0,df$diff_snatch)

## fixing category
df$category <- ifelse(as.character(df$category)=="p87", 88, as.character(df$category))
df$category <- ifelse(as.character(df$category)=="p109", 110, as.character(df$category))


df88 <- df %>%
  subset(category=="88") %>%
  mutate(mean = median(as.numeric(bweight)))
df110 <- df %>%
  subset(category=="110") %>%
  mutate(mean = median(as.numeric(bweight)))

df$category <- ifelse(df$category=="88",df88$mean,df$category)
df$category <- ifelse(df$category=="110",df110$mean,df$category)

## adding some featrues
df$bw_diff <- abs(as.numeric(as.character(df$category))-as.numeric(as.character(df$bweight)))
df$born_date <- paste("01",df$month, df$year, sep = "-")
df$comp_date <- paste("01",substr(df$date,1,2), substr(df$date,4,7), sep = "-")
df$born_date <- as.Date(df$born_date, "%d-%m-%Y")
df$comp_date <- as.Date(df$comp_date, "%d-%m-%Y")
df$age <- age_calc(df$born_date, enddate = df$comp_date, units = "years")
df$bweight <- as.numeric(df$bweight)

df$medal <- as.factor(df$medal)
df$bomb <- as.factor(df$bomb)
## model dataset
df_model <- df %>% 
  select(medal, gender,  nation, category,bomb, year, month, bw_diff, age, diff_jerk, diff_snatch)






