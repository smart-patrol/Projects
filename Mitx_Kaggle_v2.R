library(tm)
library(SnowballC)
library(doParallel)
library(caret)
library(pROC)
library(rpart)
library(gbm)
library(randomForest)
library(stringr)

registerDoParallel(4)
getDoParWorkers()


setwd("C:\\Users\\Home\\Documents\\R_code\\Data\\Mitx\\")


tst <- read.csv("NYTimesBlogTest.csv",stringsAsFactors =  FALSE)
tr <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors = FALSE)

names(tst) <- tolower(names(tst))
names(tr) <- tolower(names(tr))

dim(tst)
dim(tr)

df <- rbind(tr[,-9], tst)

depvar <- tr$popular
depvar <- as.factor(depvar)
levels(depvar) <- c("unpop","pop")

#-----------------------------------------------------------------------------------
#create date and time vars

df$date <- as.Date(df$pubdate, "%Y-%m-%d")

df$time <- strftime(df$pubdate, format="%H:%M:%S")

df$pubdate <- NULL

#fix blanks 
df$newsdesk <- ifelse(df$newsdesk == "", "Blank", df$newsdesk)
df$subsectionname <- ifelse(df$subsectionname == "", "Blank", df$subsectionname)
df$sectionname <- ifelse(df$sectionname == "", "Blank", df$sectionname)

#clean up  the names

df$sectionname <- str_trim(gsub( "/","",df$sectionname))
df$subsectionname <- str_trim(gsub( "/","",df$subsectionname))
df$subsectionname <- str_trim(gsub( "&","",df$subsectionname))
df$sectionname <- str_replace_all(df$sectionname, fixed(" "), "")
df$subsectionname <- str_replace_all(df$subsectionname, fixed(" "), "")

#table(df$newsdesk)
#table(df$sectionname)
#table(df$subsectionname)

#fix others to factors
df$newsdesk <- as.factor(df$newsdesk)
df$sectionname <- as.factor(df$sectionname)
df$subsectionname <- as.factor(df$subsectionname)

#change text to lower
df$headline <- tolower(df$headline)
df$snippet <- tolower(df$snippet)
df$abstract <- tolower(df$abstract)

#------------------------------------------------------------------------------------
# dervied variables

# create variable for 
# day of the week
df$days <- weekdays(df$date)

#weekdays
df$weekday = ifelse(df$days %in% c("Sunday","Saturday"), 0, 1)

# the hour written
df$hours <- as.numeric(substr(df$time,1,2))

# the month written
df$months <- format(df$date, "%m")

#length of absdfact, snippet, and headline
df$ab_wrds <- nchar(df$abstract)
df$sn_wrds <- nchar(df$snippet)
df$hd_wrds <- nchar(df$headline)

# log of word count
df$lg_wrdcnt <- log(df$wordcount+1)

#create ratio of length
df$ab_rt <- df$ab_wrds / df$wordcount
df$sn_rt <- df$sn_wrds / df$wordcount
df$hd_rt <- df$hd_wrds / df$wordcount

#special strings
df$ab_ques <- ifelse(grepl("//?", df$abstract) == TRUE, 1, 0)
df$hd_ques <- ifelse(grepl("//?", df$headline) == TRUE, 1, 0)

df$ab_ques2 <- ifelse(grepl("question", df$abstract) == TRUE, 1, 0)
df$hd_ques2 <- ifelse(grepl("question", df$headline) == TRUE, 1, 0)

df$ab_ans <- ifelse(grepl("answer", df$abstract) == TRUE, 1, 0)
df$hd_ans <- ifelse(grepl("answer", df$headline) == TRUE, 1, 0)

df$ab_apl <- ifelse(grepl("apple", df$abstract) == TRUE, 1, 0)
df$hd_apl <- ifelse(grepl("apple", df$headline) == TRUE, 1, 0)

#abstarct does not equal snippet
df$uneq <- ifelse(df$ab_wrds != df$sn_wrds, 1, 0)


# other key words
df$york = as.factor(ifelse(grepl("york", df$headline,ignore.case=TRUE),1,0))
df$cmnt = as.factor(ifelse(grepl("open for comments", df$headline,ignore.case=TRUE),1,0))
df$nocmnt = as.factor(ifelse(grepl("no comment necessary", df$headline,ignore.case=TRUE),1,0))


#------------------------------------------------------
# prin comp on factors

# components for newsdesk
n_mat <- model.matrix(~newsdesk, df)[,-1]
preproc <-preProcess(n_mat,  method = c("center", "scale","pca"))
n_pn <- predict(preproc, n_mat)

# components for section
n_mat <- model.matrix(~sectionname, df)[,-1]
preproc <-preProcess(n_mat,  method = c("center", "scale","pca"))
t_s_pn <- predict(preproc, n_mat)

# components for subsection
n_mat <- model.matrix(~subsectionname, df)[,-1]
preproc <-preProcess(n_mat,  method = c("center", "scale","pca"))
ss_pn <- predict(preproc, n_mat)

rm(n_mat)
# fix the names

colnames(n_pn) <- paste0("nd_", colnames(n_pn))
colnames(t_s_pn) <- paste0("sec_", colnames(t_s_pn))
colnames(ss_pn) <- paste0("ssec_", colnames(ss_pn))

# add back to dataframe
#df <- data.frame(df, n_pn, t_s_pn, ss_pn)

rm(n_pn, ss_pn, t_s_pn, preproc)

#------------------------------------------------------
# creating cropus for abstart, headline, snippet

#the headlines
hd <- Corpus(VectorSource(df$headline))
hd <- tm_map(hd,  content_transformer(tolower)) # lower no longer works
hd <- tm_map(hd,  PlainTextDocument) # lower no longer works
hd <- tm_map(hd, removePunctuation)
hd <- tm_map(hd, removeWords, stopwords("english"))
hd <- tm_map(hd, stemDocument)
d_hd <- DocumentTermMatrix(hd)
d_hd <- removeSparseTerms(d_hd, 0.995)
hd_df <- as.data.frame(as.matrix(d_hd))
rm(hd, d_hd)

names(hd_df)
colnames(hd_df) <- paste0("hd_", colnames(hd_df))
row.names(hd_df) <- NULL

# the abstract
ab <- Corpus(VectorSource(df$abstract))
ab <- tm_map(ab,  content_transformer(tolower)) # lower no longer works
ab <- tm_map(ab,  PlainTextDocument) # lower no longer works
ab <- tm_map(ab, removePunctuation)
ab <- tm_map(ab, removeWords, stopwords("english"))
ab <- tm_map(ab, stemDocument)
d_ab <- DocumentTermMatrix(ab)
d_ab <- removeSparseTerms(d_ab, 0.995)
ab_df <- as.data.frame(as.matrix(d_ab))
rm(ab, d_ab)

names(ab_df)
colnames(ab_df) <- paste0("ab_", colnames(ab_df))
row.names(ab_df) <- NULL

wrds <- cbind(hd_df, ab_df)

rm(hd_df, ab_df)

#---------------------------------------------------------------------------
# creating clusters based on words then putting them all back in

# finding best k - options 1 to 12
#e_d <- dist(wrds)
#clus <- hclust(e_d, method="ward.D")
#plot(clus)

preproc <-preProcess(wrds)
scaled <- predict(preproc, wrds)
set.seed(123)
kmns <- kmeans(wrds,7)
table(kmns$cluster)


df <- data.frame(df,  segs = kmns$cluster)


rm(wrds, kmns, preproc, scaled)

#-----------------------------------------------------------------------------------
# modeling 

#removing confounding vars
df2 <- df
df2$date <- NULL
df2$time <- NULL
df2$wordcount <- NULL
df2$uniqueid <- NULL
df2$abstract <- NULL
df2$snippet <- NULL
df2$headline <- NULL
#df2$subsectionname <- NULL
#df2$sectionname <- NULL
#df2$newsdesk <- NULL
df2$sn_wrds <- NULL

df2$days <- as.factor(df2$days)
df2$months <- as.factor(df2$months)

save(df2, file="df2.Rdata")
load("df2.Rdata")

# remove zero variance variables
nzv <- nearZeroVar(df2)
df3 <- df2[, -nzv]



# split back into test and train - ad label
n_tr <- df3[1:6532, ]
n_tst <- df3[6533:8402, ]

str(n_tr)
#cor(tr[,-1])



#-----------------------------------------------------------------------------------------------------
# boosted trees

set.seed(123)
ctrl <- trainControl(method = "repeatedcv"
                     ,number = 10
                     ,summaryFunction = twoClassSummary
                     ,classProbs=TRUE
                     ,allowParallel = TRUE
                     )

grid <- expand.grid(.interaction.depth = seq(2,4,by=2), # look at tree depths from 1 to 4
                    .n.trees=seq(50,100,by=50), # let iterations go from 10 to 100
                    .shrinkage=c(0.01)  )       # Try 2 values of the learning rate parameter

mod <- train(depvar ~.  ,data=n_tr   ,method = "gbm" ,trControl = ctrl, metric = "ROC",  tuneGrid=grid, verbose=FALSE)

mod
mod$finalModel
# ntrees = 960 , depth = 7, shirkage =0.01
# ntreess = 1500, depth = 12, shrinkage = 0.01
varImp(mod)
plot(mod)

probs = predict(mod, newdata=n_tst, type = "prob")


#-----------------------------------------------------------
#names(getModelInfo())

set.seed(123)
ctrl <- trainControl(method = "repeatedcv"
                     ,number = 10
                     ,summaryFunction = twoClassSummary
                     ,classProbs=TRUE)

adaGrid = expand.grid(.maxdepth = 5, .iter = c(100,200,300,400), .nu = 0.1 )

mod <- train( depvar~., data=n_tr, method = "ada", 
                metric = "ROC",
                trControl = ctrl)

probs = predict(mod, newdata=n_tst, type = "prob")



# output for submission

subm = data.frame(UniqueID = tst$UniqueID, Probability1 = probs$pop)

write.csv(subm, "myfirstsubmit.csv", row.names=FALSE)

head(subm)


#-----------------------------------------------------------
#names(getModelInfo())

set.seed(123)
ctrl <- trainControl(method = "repeatedcv"
                     ,number = 10
                     ,summaryFunction = twoClassSummary
                     ,classProbs=TRUE)


mod <- train( depvar~., data=n_tr, method = "rf",
              metric = "ROC",
              trControl = ctrl)

probs = predict(mod, newdata=n_tst, type = "prob")





rf_mod <- randomForest(depvar~. , data=n_tr, ntree = 500,  mtry = 10 )

tst$depvar <- 1
tst$depvar <- as.factor(tst$depvar)

rf_pred <- predict(rf_mod, newdata=tst, type="prob")

head(rf_pred)





#-----------------------------------------------------------------------------------
# turning the rest into vars and such
# sourcing becasue super ugly

source("C:\\Users\\Home\\Documents\\R_code\\Mitx_Kaggle_Words.R")

load("t_wrds.RData")
load("ts_wrds.RData")

#-----------------------------------------------------------------------------------
# putting it all back together

n_tr <- cbind(
  tr[ ,names(tr) %in% 
       c("newsdesk","sectionname","subsectionname","wordcount","popular","hour","days")]
  , t_wrds)

n_ts <- cbind(
  tst[ ,names(tst) %in% 
        c("newsdesk","sectionname","subsectionname","wordcount","popular","hour","days")]
  , ts_wrds)






















