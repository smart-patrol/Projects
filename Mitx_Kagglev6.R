library(tm)
library(SnowballC)
library(doSNOW)
library(caret)
library(gbm)
library(randomForest)
library(stringr)
library(dplyr)
library(reshape2)
library(caretEnsemble)
library(glmnet)


setwd("C:\\Users\\Home\\Documents\\R_code\\Data\\Mitx\\")

tst <- read.csv("NYTimesBlogTest.csv",stringsAsFactors =  FALSE)
tr <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors = FALSE)


names(tst) <- tolower(names(tst))
names(tr) <- tolower(names(tr))

df <- bind_rows(tr, tst)


depvar <- tr$popular
depvar <- as.factor(depvar)
levels(depvar) <- c("unpop","pop")

tr$popular <- NULL

# words to check

#create date and time vars for train
#-----------------------------------------------------------------------------------

tr$date <- as.Date(tr$pubdate, "%Y-%m-%d")
tr$time <- strftime(tr$pubdate, format="%H:%M:%S")
tr$pubdate <- NULL

#fix blanks 
tr$newsdesk <- ifelse(tr$newsdesk == "", "Blank", tr$newsdesk)
tr$subsectionname <- ifelse(tr$subsectionname == "", "Blank", tr$subsectionname)
tr$sectionname <- ifelse(tr$sectionname == "", "Blank", tr$sectionname)

#clean up  the names
tr$sectionname <- str_trim(gsub( "/","",tr$sectionname))
tr$subsectionname <- str_trim(gsub( "/","",tr$subsectionname))
tr$subsectionname <- str_trim(gsub( "&","",tr$subsectionname))
tr$sectionname <- str_replace_all(tr$sectionname, fixed(" "), "")
tr$subsectionname <- str_replace_all(tr$subsectionname, fixed(" "), "")

#bucket newdesk
tr$d_nws_un <- ifelse(tr$newsdesk
                      %in% c("Blank","Culture","Foreign","Magazine","Metro","National","Sports","Travel","TStyle"),1,0)
tr$d_news_pop <- ifelse(tr$newsdesk %in% c("OpEd","Science","Style"),1,0)

#bucket section name
tr$sec_un <- ifelse(tr$sectionname %in% c("Arts","Blank","BusinessDay",
                                          "Magazine","Multimedia","N.Y.Region","Open","Sprots","Style",
                                          "Travel","Wrold"),1,0)
tr$sec_pop <- ifelse(tr$sectionname %in% c("CrosswordsGAmes", "Opinion","Health"),1,0)

# bucketing subsection name
tr$sub_un <- ifelse(tr$subsectionname %in% c("AsiaPacific", "Dealbook","Education","FashionStyle",
                                             "Politics","RoomForDebate","SmallBusiness"), 1, 0)
tr$sub_pop <- ifelse(tr$subsectionname == "ThePublicEditor", 1, 0)




#change text to lower
tr$headline <- tolower(tr$headline)
tr$snippet <- tolower(tr$snippet)
tr$abstract <- tolower(tr$abstract)

#create date and time vars for train
#-----------------------------------------------------------------------------------
tst$date <- as.Date(tst$pubdate, "%Y-%m-%d")
tst$time <- strftime(tst$pubdate, format="%H:%M:%S")
tst$pubdate <- NULL

#fix blanks 
tst$newsdesk <- ifelse(tst$newsdesk == "", "Blank", tst$newsdesk)
tst$subsectionname <- ifelse(tst$subsectionname == "", "Blank", tst$subsectionname)
tst$sectionname <- ifelse(tst$sectionname == "", "Blank", tst$sectionname)

#clean up  the names
tst$sectionname <- str_trim(gsub( "/","",tst$sectionname))
tst$subsectionname <- str_trim(gsub( "/","",tst$subsectionname))
tst$subsectionname <- str_trim(gsub( "&","",tst$subsectionname))
tst$sectionname <- str_replace_all(tst$sectionname, fixed(" "), "")
tst$subsectionname <- str_replace_all(tst$subsectionname, fixed(" "), "")

tst$d_nws_un <- ifelse(tst$newsdesk
                       %in% c("Blank","Culture","Foreign","Magazine","Metro","National","Sports","Travel","TStyle"),1,0)
tst$d_news_pop <- ifelse(tst$newsdesk %in% c("OpEd","Science","Style"),1,0)

#bucket section name
tst$sec_un <- ifelse(tst$sectionname %in% c("Arts","Blank","BusinessDay",
                                            "Magazine","Multimedia","N.Y.Region","Open","Sprots","Style",
                                            "Travel","Wrold"),1,0)
tst$sec_pop <- ifelse(tst$sectionname %in% c("CrosswordsGAmes", "Opinion","Health"),1,0)

# bucketing subsection name
tst$sub_un <- ifelse(tst$subsectionname %in% c("AsiaPacific", "Dealbook","Education","FashionStyle",
                                               "Politics","RoomForDebate","SmallBusiness"), 1, 0)
tst$sub_pop <- ifelse(tst$subsectionname == "ThePublicEditor", 1, 0)


#change text to lower
tst$headline <- tolower(tst$headline)
tst$snippet <- tolower(tst$snippet)
tst$abstract <- tolower(tst$abstract)

# dervied variables for train
#------------------------------------------------------------------------------------
# create variable for # day of the week
tr$days <- weekdays(tr$date)

#weekdays
tr$weekday = ifelse(tr$days %in% c("Sunday","Saturday"), 0, 1)

# the hour written
tr$hours <- as.numeric(substr(tr$time,1,2))

#days since
min_dt <-  as.Date(c("2014-09-01"), "%Y-%m-%d")
tr$days_sin <- difftime(tr$date, min_dt, units = c("days"))

#length of abstract, snippet, and headline
tr$ab_wrds <- nchar(tr$abstract)
tr$sn_wrds <- nchar(tr$snippet)
tr$hd_wrds <- nchar(tr$headline)

# log of word count
tr$lg_wrdcnt <- log(tr$wordcount+1)

#create ratio of length
tr$ab_rt <- tr$ab_wrds / tr$wordcount
tr$sn_rt <- tr$sn_wrds / tr$wordcount
tr$hd_rt <- tr$hd_wrds / tr$wordcount

#special strings
tr$ab_ques <- ifelse(grepl("//?", tr$abstract) == TRUE, 1, 0)
tr$hd_ques <- ifelse(grepl("//?", tr$headline) == TRUE, 1, 0)

tr$ab_ques2 <- ifelse(grepl("question", tr$abstract) == TRUE, 1, 0)
tr$hd_ques2 <- ifelse(grepl("question", tr$headline) == TRUE, 1, 0)

tr$ab_ans <- ifelse(grepl("answer", tr$abstract) == TRUE, 1, 0)
tr$hd_ans <- ifelse(grepl("answer", tr$headline) == TRUE, 1, 0)

tr$ab_apl <- ifelse(grepl("apple", tr$abstract) == TRUE, 1, 0)
tr$hd_apl <- ifelse(grepl("apple", tr$headline) == TRUE, 1, 0)
#abstarct does not equal snippet
tr$uneq <- ifelse(tr$ab_wrds != tr$sn_wrds, 1, 0)

# other key words
tr$york = as.factor(ifelse(grepl("york", tr$headline,ignore.case=TRUE),1,0))
tr$cmnt = as.factor(ifelse(grepl("open for comments", tr$headline,ignore.case=TRUE),1,0))
tr$nocmnt = as.factor(ifelse(grepl("no comment necessary", tr$headline,ignore.case=TRUE),1,0))

tr$q_exp = as.factor(ifelse(grepl("does |is |has |how |what |why |who |where |when |which | whom | \\?", tr$abstract, ignore.case=T), 1,0))

tr$q_exp_hd = as.factor(ifelse(grepl("does |is |has |how |what |why |who |where |when |which | whom | \\?", tr$headline, ignore.case=T), 1,0))

tr$yr_hd = as.factor(ifelse(  grepl("1914 |1939 | 1964 | 2014 | 2015 ", tr$headline, ignore.case=T), 1,0))

tr$yr_ab = as.factor(ifelse(grepl("1914 |1939 | 1964 | 2014 |2015 ", tr$abstract, ignore.case=T), 1,0))

tr$ab_puzz = as.factor(ifelse(  grepl("puzzle | crossword", tr$abstract, ignore.case=T), 1,0))
tr$hd_puzz = as.factor(ifelse(  grepl("puzzle | crossword", tr$headline, ignore.case=T), 1,0))

tr$ab_readers = as.factor(ifelse(grepl("readers", tr$abstract, ignore.case=T), 1,0))
tr$hd_readers = as.factor(ifelse(grepl("readers", tr$headline, ignore.case=T), 1,0))

tr$ab_cany = as.factor(ifelse(grepl("can you", tr$abstract, ignore.case=T), 1,0))
tr$hd_cany = as.factor(ifelse(grepl("can you", tr$headline, ignore.case=T), 1,0))

tr$hd_me = as.factor(ifelse(grepl(" me ", tr$headline, ignore.case=T), 1,0))
tr$ab_me = as.factor(ifelse( grepl(" me ", tr$abstract, ignore.case=T), 1,0))

tr$hd_our = as.factor(ifelse(grepl(" our ", tr$headline, ignore.case=T), 1,0))
tr$ab_our = as.factor(ifelse( grepl(" our ", tr$abstract, ignore.case=T), 1,0))

tr$hd_thei= as.factor(ifelse(grepl(" i ", tr$headline, ignore.case=T), 1,0))
tr$ab_thei = as.factor(ifelse( grepl(" i ", tr$abstract, ignore.case=T), 1,0))






# derived variable for test
#----------------------------------------------------------------------
# create variable for 
# day of the week
tst$days <- weekdays(tst$date)

#weekdays
tst$weekday = ifelse(tst$days %in% c("Sunday","Saturday"), 0, 1)

# the hour written
tst$hours <- as.numeric(substr(tst$time,1,2))

# the days since
tst$days_sin <- difftime(tst$date, min_dt, units = c("days"))

#length of abststact, snippet, and headline
tst$ab_wrds <- nchar(tst$abstract)
tst$sn_wrds <- nchar(tst$snippet)
tst$hd_wrds <- nchar(tst$headline)

# log of word count
tst$lg_wrdcnt <- log(tst$wordcount+1)

#create ratio of length
tst$ab_rt <- tst$ab_wrds / tst$wordcount
tst$sn_rt <- tst$sn_wrds / tst$wordcount
tst$hd_rt <- tst$hd_wrds / tst$wordcount

#special ststings
tst$ab_ques <- ifelse(grepl("//?", tst$abstract) == TRUE, 1, 0)
tst$hd_ques <- ifelse(grepl("//?", tst$headline) == TRUE, 1, 0)

tst$ab_ques2 <- ifelse(grepl("question", tst$abstract) == TRUE, 1, 0)
tst$hd_ques2 <- ifelse(grepl("question", tst$headline) == TRUE, 1, 0)

tst$ab_ans <- ifelse(grepl("answer", tst$abstract) == TRUE, 1, 0)
tst$hd_ans <- ifelse(grepl("answer", tst$headline) == TRUE, 1, 0)

tst$ab_apl <- ifelse(grepl("apple", tst$abstract) == TRUE, 1, 0)
tst$hd_apl <- ifelse(grepl("apple", tst$headline) == TRUE, 1, 0)

#abstarct does not equal snippet
tst$uneq <- ifelse(tst$ab_wrds != tst$sn_wrds, 1, 0)

# other key words
tst$york = as.factor(ifelse(grepl("york", tst$headline,ignore.case=TRUE),1,0))
tst$cmnt = as.factor(ifelse(grepl("open for comments", tst$headline,ignore.case=TRUE),1,0))
tst$nocmnt = as.factor(ifelse(grepl("no comment necessary", tst$headline,ignore.case=TRUE),1,0))

tst$q_exp = as.factor(ifelse(
  grepl("does |is |has |how |what |why |who |where |when |which | whom | \\?", tst$abstract, ignore.case=T), 1,0))

tst$q_exp_hd = as.factor(ifelse(
  grepl("does |is |has |how |what |why |who |where |when |which | whom | \\?", tst$headline, ignore.case=T), 1,0))

tst$yr_hd = as.factor(ifelse(grepl("1914 |1939 | 1964 | 2014 |2015 ", tst$headline, ignore.case=T), 1,0))
tst$yr_ab = as.factor(ifelse(grepl("1914 |1939 | 1964 | 2014 |2015 ", tst$abstract, ignore.case=T), 1,0))

tst$ab_puzz = as.factor(ifelse(  grepl("puzzle | crossword", tst$abstract, ignore.case=T), 1,0))
tst$hd_puzz = as.factor(ifelse(  grepl("puzzle | crossword", tst$headline, ignore.case=T), 1,0))


tst$ab_readers = as.factor(ifelse(grepl("readers", tst$abstract, ignore.case=T), 1,0))
tst$hd_readers = as.factor(ifelse(grepl("readers", tst$headline, ignore.case=T), 1,0))

tst$ab_cany = as.factor(ifelse(grepl("can you", tst$abstract, ignore.case=T), 1,0))
tst$hd_cany = as.factor(ifelse(grepl("can you", tst$headline, ignore.case=T), 1,0))

tst$hd_me = as.factor(ifelse(grepl(" me ", tst$headline, ignore.case=T), 1,0))
tst$ab_me = as.factor(ifelse( grepl(" me ", tst$abstract, ignore.case=T), 1,0))

tst$hd_our = as.factor(ifelse(grepl(" our ", tst$headline, ignore.case=T), 1,0))
tst$ab_our = as.factor(ifelse( grepl(" our ", tst$abstract, ignore.case=T), 1,0))

tst$hd_thei= as.factor(ifelse(grepl(" i ", tst$headline, ignore.case=T), 1,0))
tst$ab_thei = as.factor(ifelse( grepl(" i ", tst$abstract, ignore.case=T), 1,0))

#------------------------------------------------------
# creating cropus for abstart, headline, snippet for train
#the headlines

hd <- Corpus(VectorSource(df$headline))
hd <- tm_map(hd,  content_transformer(tolower)) # lower no longer works
hd <- tm_map(hd,  PlainTextDocument) # lower no longer works
hd <- tm_map(hd, removePunctuation)
hd <- tm_map(hd, removeWords, stopwords("english"))
hd <- tm_map(hd, stemDocument)
d_hd <- DocumentTermMatrix(hd)
d_hd <- removeSparseTerms(d_hd, 0.999)
hd_tr <- as.data.frame(as.matrix(d_hd))
rm(hd, d_hd)

names(hd_tr)
colnames(hd_tr) <- paste0("hd_", colnames(hd_tr))
row.names(hd_tr) <- NULL

# the abstract
ab <- Corpus(VectorSource(df$abstract))
ab <- tm_map(ab,  content_transformer(tolower)) # lower no longer works
ab <- tm_map(ab,  PlainTextDocument) # lower no longer works
ab <- tm_map(ab, removePunctuation)
ab <- tm_map(ab, removeWords, stopwords("english"))
ab <- tm_map(ab, stemDocument)
d_ab <- DocumentTermMatrix(ab)
d_ab <- removeSparseTerms(d_ab, 0.999)
ab_tr <- as.data.frame(as.matrix(d_ab))
rm(ab, d_ab)

names(ab_tr)
colnames(ab_tr) <- paste0("ab_", colnames(ab_tr))
row.names(ab_tr) <- NULL

wrds <- data.frame(ab_tr, hd_tr)



n_tr <- data.frame(tr, wrds[1:6532,  ])
n_tst <- data.frame(tst, wrds[6533:8402, ])


n_tr$days <- as.factor(n_tr$days)
n_tst$days <- as.factor(n_tst$days)
n_tr$days_sin <- as.numeric(n_tr$days_sin)
n_tst$days_sin <- as.numeric(n_tst$days_sin)

n_tr$headline <- NULL
n_tr$snippet <- NULL
n_tr$abstract <- NULL
n_tr$uniqueid <- NULL 
n_tr$date <- NULL
n_tr$time <- NULL
n_tr$ab_rt <- NULL
n_tr$sn_rt <- NULL
n_tr$hd_rt <- NULL
n_tr$wordcount <- NULL


n_tst$headline <- NULL
n_tst$snippet <- NULL
n_tst$abstract <- NULL
n_tst$uniqueid <- NULL 
n_tst$date <- NULL
n_tst$time <- NULL
n_tst$ab_rt <- NULL
n_tst$sn_rt <- NULL
n_tst$hd_rt <- NULL
n_tst$wordcount <- NULL

#----------------------------------------------------------------------
# feature selection using lasso

#blah <- n_tr[!complete.cases(n_tr),]
#View(blah)

set.seed(123)
glm_nt <- cv.glmnet(model.matrix(~., n_tr), depvar, family = "binomial",
                    type.measure = "auc", parallel=TRUE, intercept = FALSE, alpha =1)

coeffs <- coef(glm_nt, s = "lambda.1se")
coeffs <- as.data.frame(as.table(as.matrix(coeffs)))

filt_cf <- coeffs[abs(coeffs$Freq) > 0, ]
filt_cf <- filt_cf[order(abs(filt_cf$Freq), decreasing = TRUE),]


filt_cf %>%
  arrange(desc(Freq)) %>%  tail(.,35)

filt_cf %>%
  arrange(desc(Freq)) %>%  head(.,30)


summary(filt_cf)


keep = names(n_tr) %in% filt_cf$Var1

new_tr <- n_tr[ , keep]
new_tst <- n_tst[ ,keep]


save(keep, file="keepvar.Rdata")

#rm(glm_nt, coeffs, filt_cf, keep)

#----------------------------------------------------------------------
# slap down some models yo


cl <- makeCluster(4, type="SOCK")
registerDoSNOW(cl)

set.seed(123)
ctrl <- trainControl(method = "repeatedcv"
                     ,number = 5
                     ,summaryFunction = twoClassSummary
                     ,classProbs=TRUE
                     ,allowParallel = TRUE)


grid <- expand.grid(.interaction.depth = c(3), # look at tree depths from 1 to 4
                    .n.trees=c(150), # let iterations go from 10 to 100
                    .shrinkage=c(0.01))       # Try 2 values of the learning rate parameter

gbmmod1 <- train(depvar ~.  ,data=new_tr   ,method = "gbm" ,trControl = ctrl, metric = "ROC"
                 ,tuneGrid = grid
                 ,verbose=FALSE)


grid <- expand.grid(.interaction.depth = c(4), # look at tree depths from 1 to 4
                    .n.trees=c(1000), # let iterations go from 10 to 100
                    .shrinkage=c(0.01))       # Try 2 values of the learning rate parameter


gbmmod2 <- train(depvar ~.  ,data=new_tr, method = "gbm" ,trControl = ctrl, metric = "ROC"
                  ,tuneGrid = grid
                 ,verbose=FALSE)

#285 
rfmod <- train(depvar ~.  ,data=new_tr, method = "rf" ,trControl = ctrl, metric = "ROC"
                 ,tuneGrid = data.frame(mtry = c(285))
                 ,verbose=FALSE)

cl <- makeCluster(4, type="SOCK")
registerDoSNOW(cl)

set.seed(123)
# C set to 1 , sigma = 0.0001840407
svmmod <- train(x = model.matrix(~., n_tr), y = depvar, 
                method = "svmRadial",
                preProc = c("center","scale"),
                metric = "ROC",
                trControl = ctrl)

stopCluster(cl)



#----------------------------------------------------------------------------
# comparing finished model performance 

resamps <- resamples(list(GBM1 = gbmmod1,
                          GBM2 = gbmmod2,
                          RF = rfmod,
                          SVM = svmmod
                          ))
resamps

summary(resamps)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")


difValues <- diff(resamps)
difValues
summary(difValues)

trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))



#----------------------------------------------------------------------------
# saving models and then running predictions on the data

save(gbmmod1, file="gbmmod1.R")
save(gbmmod2, file="gbmmod2.R")
save(rfmod, file="rfmod.R")
save(svmmod, file="svmmod.R")


gmb_p <- predict(gbmmod2, newdata=new_tr)
rf_p <- predict(rfmod, newdata=new_tr)

names(gmb_p ) <- c("gmb_p")
names(rf_p ) <- c("rf_p")

head(gmb_p)
str(gmb_p)


new_tr$gmp_p <- NULL
new_tr$rf_p <- NULL
new_tr2$gmp_p <- NULL
new_tr2$rf_p <- NULL

new_tr2 <- data.frame(new_tr, gmb_p, rf_p)




str(new_tr2$gmb_p)


cl <- makeCluster(4, type="SOCK")
registerDoSNOW(cl)

set.seed(123)

en_mod <- train(depvar ~.  ,data=new_tr2   ,method = "gbm" ,trControl = ctrl, metric = "ROC"
                 #,tuneGrid = grid
                 ,verbose=FALSE)

stopCluster(cl)

#----------------------------------------------------------------------------
# trying with caret ensemble instead


cl <- makeCluster(4, type="SOCK")
registerDoSNOW(cl)

ctrl <- trainControl(method = "repeatedcv"
                     ,number = 5
                     ,summaryFunction = twoClassSummary
                     ,classProbs=TRUE
                     ,savePredictions = TRUE
                     ,allowParallel = TRUE)



mod_list <- caretList(
  depvar ~., data= new_tr,
  trControl = ctrl, metric = "ROC", 
  methodList = c('rpart','gbm','rf')
  , tuneList = list(
    rp_mod <- caretModelSpec(method = "rpart",  tuneLength = 30)
   ,rf_mod <- caretModelSpec(method = "rf", tuneGrid = data.frame(.mtry = 285))
   ,gmb_mod <- caretModelSpec(method = "gbm", tuneGrid = data.frame(.interaction.depth = c(4), # look at tree depths from 1 to 4
                                                                    .n.trees=c(1000), # let iterations go from 10 to 100
                                                                    .shrinkage=c(0.01)))
    )
  )

stopCluster(cl)

xyplot(resamples(mod_list))



modelCor(resamples(mod_list))


greedy_ensemble <- caretEnsemble(mod_list)
summary(greedy_ensemble)


varImp(greedy_ensemble)

head(data.frame(model_preds))


library('caTools')
model_preds <- lapply(mod_list, predict, newdata=new_tst, type='prob')
model_preds <- lapply(model_preds, function(x) x[,'M'])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=new_tst)



head(data.frame(ens_preds))


