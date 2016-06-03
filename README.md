# HomedepotRelevance
# Code for review
> train <- read.csv("~/School/CKME163/Raw Data/train.csv (1)/train.csv")
>   View(train)
> product_descriptions <- read.csv("~/School/CKME163/Raw Data/product_descriptions.csv/product_descriptions.csv")
>   View(product_descriptions)
> product_descriptions <- read.csv("~/School/CKME163/Raw Data/product_descriptions.csv/product_descriptions.csv")
>   View(product_descriptions)
> attributes <- read.csv("~/School/CKME163/Raw Data/attributes.csv/attributes.csv")
>   View(attributes)
> hist(train$relevance, freq=F)
> hist(train$relevance, xlab="relevance", main="Frequency of Relevance Classes", breaks=1)
> hist(train$relevance, xlab="relevance", main="Frequency of Relevance Classes", breaks=.5)


> setwd("~/School/CKME163/Raw Data")
> library(ggplot2, quietly=T)
Warning message:
package ‘ggplot2’ was built under R version 3.2.5 
> library(gbm)
Loading required package: survival
Loading required package: lattice
Loading required package: splines
Loading required package: parallel
Loaded gbm 2.1.1
Warning messages:
1: package ‘gbm’ was built under R version 3.2.5 
2: package ‘lattice’ was built under R version 3.2.5 
> library(readr)
> cat("Reading data\n")
Reading data
> train <- read_csv('../input/train.csv')
Error: '../input/train.csv' does not exist in current working directory ('C:/Users/KarmaAgent/Documents/School/CKME163/Raw Data').
> setwd("~/School/CKME163/Raw Data/DATA")
> train <- read_csv('../input/train.csv')
Error: '../input/train.csv' does not exist in current working directory ('C:/Users/KarmaAgent/Documents/School/CKME163/Raw Data/DATA').
> train <- read_csv('../train.csv')
Error: Cannot read file C:/Users/KarmaAgent/Documents/School/CKME163/Raw Data/train.csv
> train <- read_csv('../DATA/train.csv')
> test <- read_csv('../DATA/test.csv')
> desc <- read_csv('../DATA/product_descriptions.csv')
> cat("Merge description with train and test data \n")
Merge description with train and test data 
> train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
> test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
> t <- Sys.time()
> word_match <- function(words,title,desc){
+     n_title <- 0
+     n_desc <- 0
+     words <- unlist(strsplit(words," "))
+     nwords <- length(words)
+     for(i in 1:length(words)){
+         pattern <- paste("(^| )",words[i],"($| )",sep="")
+         n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
+         n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
+     }
+ return(c(n_title,nwords,n_desc))
+ }
> cat("Get number of words and word matching title in train\n")
Get number of words and word matching title in train
> train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))

There were 50 or more warnings (use warnings() to see the first 50)
> cat("Get number of words and word matching title in train\n")
Get number of words and word matching title in train
> train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
There were 50 or more warnings (use warnings() to see the first 50)
> train$nmatch_title <- train_words[,1]
> train$nwords <- train_words[,2]
> train$nmatch_desc <- train_words[,3]
> cat("Get number of words and word matching title in test\n")
Get number of words and word matching title in test
> test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
There were 50 or more warnings (use warnings() to see the first 50)
> test$nmatch_title <- test_words[,1]
> test$nwords <- test_words[,2]
> test$nmatch_desc <- test_words[,3]
> rm(train_words,test_words)
> cat("A simple linear model on number of words and number of words that match\n")
A simple linear model on number of words and number of words that match
> gbm_model <- gbm.fit(train[,7:9],train$relevance,distribution = "gaussian",
+                      interaction.depth = 4, shrinkage=0.0125,n.trees=1100)
Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        0.2845             nan     0.0125    0.0006
     2        0.2839             nan     0.0125    0.0006
     3        0.2833             nan     0.0125    0.0006
     4        0.2827             nan     0.0125    0.0006
     5        0.2822             nan     0.0125    0.0006
     6        0.2816             nan     0.0125    0.0005
     7        0.2811             nan     0.0125    0.0005
     8        0.2807             nan     0.0125    0.0004
     9        0.2802             nan     0.0125    0.0005
    10        0.2797             nan     0.0125    0.0005
    20        0.2755             nan     0.0125    0.0004
    40        0.2694             nan     0.0125    0.0003
    60        0.2652             nan     0.0125    0.0002
    80        0.2620             nan     0.0125    0.0001
   100        0.2596             nan     0.0125    0.0001
   120        0.2578             nan     0.0125    0.0001
   140        0.2564             nan     0.0125    0.0001
   160        0.2553             nan     0.0125    0.0000
   180        0.2545             nan     0.0125    0.0000
   200        0.2538             nan     0.0125    0.0000
   220        0.2532             nan     0.0125    0.0000
   240        0.2528             nan     0.0125    0.0000
   260        0.2524             nan     0.0125    0.0000
   280        0.2520             nan     0.0125    0.0000
   300        0.2518             nan     0.0125    0.0000
   320        0.2516             nan     0.0125    0.0000
   340        0.2514             nan     0.0125    0.0000
   360        0.2512             nan     0.0125    0.0000
   380        0.2511             nan     0.0125    0.0000
   400        0.2509             nan     0.0125    0.0000
   420        0.2508             nan     0.0125    0.0000
   440        0.2507             nan     0.0125    0.0000
   460        0.2507             nan     0.0125    0.0000
   480        0.2506             nan     0.0125    0.0000
   500        0.2505             nan     0.0125    0.0000
   520        0.2505             nan     0.0125    0.0000
   540        0.2504             nan     0.0125    0.0000
   560        0.2504             nan     0.0125   -0.0000
   580        0.2503             nan     0.0125    0.0000
   600        0.2503             nan     0.0125    0.0000
   620        0.2502             nan     0.0125    0.0000
   640        0.2502             nan     0.0125    0.0000
   660        0.2502             nan     0.0125    0.0000
   680        0.2501             nan     0.0125    0.0000
   700        0.2501             nan     0.0125   -0.0000
   720        0.2501             nan     0.0125    0.0000
   740        0.2501             nan     0.0125   -0.0000
   760        0.2500             nan     0.0125   -0.0000
   780        0.2500             nan     0.0125   -0.0000
   800        0.2500             nan     0.0125   -0.0000
   820        0.2500             nan     0.0125    0.0000
   840        0.2500             nan     0.0125   -0.0000
   860        0.2499             nan     0.0125   -0.0000
   880        0.2499             nan     0.0125   -0.0000
   900        0.2499             nan     0.0125    0.0000
   920        0.2499             nan     0.0125   -0.0000
   940        0.2499             nan     0.0125   -0.0000
   960        0.2499             nan     0.0125   -0.0000
   980        0.2499             nan     0.0125   -0.0000
  1000        0.2498             nan     0.0125   -0.0000
  1020        0.2498             nan     0.0125   -0.0000
  1040        0.2498             nan     0.0125   -0.0000
  1060        0.2498             nan     0.0125   -0.0000
  1080        0.2498             nan     0.0125   -0.0000
  1100        0.2498             nan     0.0125   -0.0000

> test_relevance <- predict(gbm_model,test[,6:8],n.trees=1100)
> test_relevance <- ifelse(test_relevance>3,3,test_relevance)
> test_relevance <- ifelse(test_relevance<1,1,test_relevance)
> submission <- data.frame(id=test$id,relevance=test_relevance)
> write_csv(submission,"gbm_relevance.csv")
> print(Sys.time()-t)
Time difference of 17.03814 mins
> write_csv(submission,"gbm.csv")
> gbm_relevance <- read.csv("~/School/CKME163/Raw Data/DATA/gbm_relevance.csv")
>   View(gbm_relevance)
> 
> save.image("~/School/CKME163/Raw Data/DATA/Code 1.RData")
> 
