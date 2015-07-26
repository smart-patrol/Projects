rm(list = ls(all = TRUE))

library(RODBC)
library(dplyr)
library(reshape2)
library(ggplot2)

#------------------------------------------------------------------

## data readin

#------------------------------------------------------------------

names(pat) <- tolower(names(pat))

glimpse(pat)

dim(pat)

summary(pat)

# fixing values
pat$first_activity <- as.Date(pat$first_activity)

pat$patid <- NULL

#------------------------------------------------------------------
#Tenure measures

#pos looks most reasonable

par(mfrow=c(1,2))

# period of tenure based on pos
hist(pat$tenure_dos, col="orange", xlab="pos tenure")

#tenure of visitations vis emails
hist(pat$tenure_visit, col="grey", xlab="email tenure")
par(mfrow=c(0,1))

#------------------------------------------------------------------
#Visitation measures - this is the depenent variable

# patient enagagment appers to be most stable since it is not drive by point in time

par(mfrow=c(1,3))

#total visits via pos
hist(pat$visits_id_cnt, col="green", xlab="total visi pe")

# total visits patient engagment
hist(pat$visits, col="yellow", xlab="total visi pe")

# total visits via email table
hist(pat$max_em_visit_num, col="red", xlab="total visit em")

par(mfrow=c(0,1))


p <- pat %>% select( visits, max_em_visit_num, visits_id_cnt) %>%
  melt(.) %>% ggplot(., aes(factor(variable), value)) + geom_boxplot()

p  + ylim(0,50)

summary(pat$visits_id_cnt)
summary(pat$visits)
summary(pat$max_em_visit_num)

#------------------------------------------------------------------
#Additional

# amount per customer visits
pat$amt_per_visit = pat$ttl_cptamount / pat$visits_id_cnt
summary(pat$amt_per_visit)
hist(pat$amt_per_visit, col="blue", xlab="amount per customer")

# compared to just the total amount
hist(pat$ttl_cptamount, col="blue", xlab="total cust amount")
summary(pat$ttl_cptamount)

# start dates
pat$act_yr <- format(as.Date(pat$first_activity), "%Y")
pat$act_mth <- format(as.Date(pat$first_activity), "%m")

out <- pat %>% 
  filter(!is.na(first_activity), act_yr == 2014) %>%
  select(act_yr, act_mth, patnum) %>%
  mutate( label = paste0(act_yr ,"_", act_mth)) %>%
  group_by(label) %>%
  summarize( cnts = n_distinct(patnum))  %>%
  mutate( chg = (cnts - lag(cnts))  / cnts)

head(as.data.frame(out),22)

mean(out$chg, na.rm=T)

# there is not realy change in new cutomers month to month less than 1% increase

# this will look at only 2014
pat %>% 
  filter(!is.na(first_activity), act_yr == 2014) %>%
  select(act_yr, act_mth, patnum) %>%
  mutate( label = paste0(act_yr ,"_", act_mth)) %>%
  group_by(label) %>%
  summarize( cnts = n_distinct(patnum))  %>%
  ggplot(., aes(x = label, y = cnts)) + geom_bar(stat = "identity")


# distorbution of sends per patuent
barplot(table(pat$sends), col="grey", xlab="sends")
prop.table(table(pat$sends))*100
# 50% have been sent email 3 or less times in data provided


#------------------------------------------------------------------
#Categorial Variables

# diag code - top codes
tail(sort(prop.table(table(pat$diag1))*100),10)

sum(tail(sort(prop.table(table(pat$diag1))*100),10))

# first service to patient
sort(prop.table(table(pat$first_cpt))*100)
#86% therepatutic

#desciption of diagnossi
tail(sort(prop.table(table(pat$desc1_))),20)*100

# sex and age of the customers
prop.table(table(pat$sex))*100

hist(pat$age, col="red", xlab="age")


pat[which.max(pat$age), ]

pat[which.min(pat$age), ]

summary(pat$age)

#------------------------------------------------------------------
# Scoping the depvar

summary(pat$visits)

sum(is.na(pat$visits))
# 357 NA values for visits that do not match

plot(pat$visits, pat$sends)

ggplot(pat, aes(visits, sends)) + geom_point() + stat_smooth() + xlim(1,12) + ylim(0,10)
# it appears that the optimal contatn is peaking at 5 visits after visits to not increase


ggplot(pat, aes(visits, ttl_cptamount)) + geom_point() + stat_smooth() + xlim(1,12) + ylim(0,20000)
# relationship between total amount and # of visits present?


# looking at correlations
nums <- sapply(pat, is.numeric)

cor(na.omit(pat[, nums]) , method = c("spearman"))


# it looks like there is strong intercorrleation between values but also appears to be linear

keep <- c('tenure_dos',	'ttl_cptamount',	'visits_id_cnt','ttl_ebus'
          ,	'ttl_manunits',	'tenure_act',	'visits',	'sends',	'discharged')


cor(na.omit(pat[ , names(pat) %in% keep]), method = c("spearman"))

GGally::ggpairs(pat[ , names(pat) %in% keep])


#------------------------------------------------------------------
# Looking at relationship betwen dischare and total visits

tab = data.frame(prop.table(table(pat$visits, pat$discharged),2))

names(tab) <- c("visits", "discharged", "percent")

ggplot(tab [ tab$percent > 0,], aes(x = visits, y = percent, fill = discharged)) + 
  geom_bar(stat = "identity") + facet_wrap(~discharged) 
  theme_bw()

by(pat$visits, pat$discharged, summary)
# the visits are higher for those how are flagged as discharged
# were they unhealthy?

#------------------------------------------------------------------
# Looking at relationship sends per visits

summary(pat$visits / pat$sends)

# averge of 2.7 visits per send 

hist(pat$visits / pat$sends)

plot(pat$visits, pat$sends)


pat[ which.max(pat$sends), ]



pat$high <- ifelse(pat$visits > 6, 1, 0)

prop.table(table(pat$high))*100

library(lattice)

keep <- c('tenure_dos',  'ttl_cptamount',	'visits_id_cnt','ttl_ebus'
          ,	'ttl_manunits',	'tenure_act',	'high',	'sends',	'discharged')

splom( pat[ , names(pat) %in% keep], groups = pat$high)


cor(na.omit(pat[ , names(pat) %in% keep]), method = c("spearman"))

# adding back in all numerics

nums <- sapply(pat, is.numeric)

cor(na.omit(pat[, nums]) , method = c("spearman"))





















