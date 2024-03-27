library(tidyverse)
library(lubridate)
library(quanteda)
library(rstudioapi)
library(ngram)
library(tm)
library(randomForest)
library(caret)


setwd(dirname(getActiveDocumentContext()$path))

df_speeches <- read_rds("Corp_Bundestag_V2.rds")

df_speeches <- df_speeches %>%
  filter(party %in% c("SPD", "CDU/CSU") & chair == FALSE) %>%
  select(date, party, speaker, text)

df_speeches$party <- gsub("/", ".", df_speeches$party)


text = df_speeches$text

# Remove links
text <- gsub("http\\w+", "", text)


# Replace special characters
text = gsub("[^[:alnum:]]", " ",text)

# Replace @UserName

text <- gsub("@\\w+", "", text)

# Remove punctuation
text <- gsub("[[:punct:]]", "", text)

# Remove tabs
text <- gsub("[ |\t]{2,}", "", text)

# Remove blank spaces at the beginning
text <- gsub("^ ", "", text)

# Remove blank spaces at the end
text <- gsub(" $", "", text)

##Remove '
text <- gsub("'", "", text)


#Remove it?s
text<-gsub("?","",text)

##Remove numbers
text = removeNumbers(text)

df_speeches$text <- text

###Create a subset of data frames for each month. Store in list.

# do this quaterley
#df_speeches$yearMo <- as.numeric(paste0( year(df_speeches$date), quarter(df_speeches$date)))
# do this yearly
df_speeches$yearMo <- as.numeric(year(df_speeches$date))

yearMo<-unique(df_speeches$yearMo)

yearMo_subset<-function(x){
  temp_df<-filter(df_speeches,yearMo==x)
  assign(paste0("df_",str_replace(x,"-","")),temp_df)
}
list_yearModf<-lapply(yearMo,yearMo_subset)

##Store each text data frame in VOlatile Corpus and then in dfm, so far without the party labels.


list_yearModfm<-lapply(list_yearModf,function(x){
  temp_cor<-corpus(x$text, docvars=x[c("party","speaker","yearMo")])
  temp_dfm <- dfm(temp_cor,
                  tolower=TRUE,
                  verbose=TRUE,
                  removePunct = TRUE,
                  remove = stopwords("german"),
                  stem = T)
  assign(paste0(names(x),"_dfm"),temp_dfm)
})

##create dfm for whole dataset

corpus<-corpus(df_speeches$text,docvars=df_speeches[c("party","speaker","yearMo")])

dfm<-dfm(corpus,
                 tolower=TRUE,
                 verbose=TRUE,
                 removePunct=TRUE,
                 remove = stopwords("german"),
                 stem = T)

# save.image("sq.Rdata")

###How can I keep other variables in TDM?
#my_dfm$Party<-mp_tweets_complete$Party
#my_dfm$month<-mp_tweets_complete$month

#################################2.2	fixed vocabulary, drop all words not appearing in X tweets (authors used 200 speeches)###############

###in R words: remove all the variables that score == 0 in >= XXXX rows.
##How many rows do I choose?
##Anderson, Spirling: 200/3573778 speeches ==0.00005596319
##For me that would mean: 0.00005596319*18627 ==1.042426, so just 1 tweet. I first start with 20 tweets.

###Stirling Anderson dropped all the words "that do not appear in 200 speeches in the entire dataset."
###I will drop all words that do not appear in 20 Speeches across all subsetted data frames. So in the entire dataset.

# ("sq.Rdata")
###create dfm as a rule that entails all the words not "allowed" for analysis.
dfm2<-dfm_trim(dfm, min_termfreq = 1, min_docfreq = 199)

###Remove all the words in each subset dfm that are not in the major dfm.

keep_tokens<-colnames(dfm2)

dfm_select(list_yearModfm[[204]], pattern = keep_tokens, selection = "keep")

list_yearModfm2<-lapply(list_yearModfm,
                        function(x){
                          
                          if(ncol(x) > 0 | nrow(x) > 0){
                          dfm <- dfm_select(x,pattern=keep_tokens,selection="keep")
                          
                          } else {
                            
                            dfm <- x
                          }
                            
                          return(dfm)
                          })

#################################2.1 Bag of Words normalised by maximum absolute value, do not retain word-order###############

###word specific frequencies normalised by maximum absolute value. I assume this means normalise by word that appeared the most in document.

#propmax: how many times is token mentioned vs. how many times is most-often-mentioned token mentioned? tf_{ij} / \textrm{max}_j tf_{ij} 
# dfmweighted_propmax<-dfm_weight(dfm2, scheme = "propmax") 
# dfmweighted_propmax[1:5,1:17]

list_yearModfm_propmax<-lapply(list_yearModfm2,function(x){dfm_weight(x,scheme="propmax")})


#prop: how many times is token mentioned vs. how many tokens are in document? tf_{ij} / sum_j tf_{ij}
# dfmweighted_prop<-dfm_weight(dfm2, scheme = "prop")
# dfmweighted_prop[1:5,1:20]
# 
# list_yearModfm_prop<-lapply(list_yearModfm2,function(x){dfm_weight(x,scheme="prop")})


###I continue working with the propmax dfm.
rm(dfm, dfm2, list_yearModf, list_yearModfm, list_yearModfm2)

###########################################Elaborate on this later with the aid of others.##########################################################################
classify_fun <- function(dfm, train_size,
                         min_ratio_s_p){

  cat("Starting\n")
  
  #dfm <- list_yearModfm_propmax[[107]]
  
  df_yearMo2 = convert(dfm, to = "data.frame")
  
  #df_yearMo2[rowSums(is.na(df_yearMo2)) == 0,]
  
  
  # first you have a p>>s problem: many more columns than rows and thus not sufficient data. What to do?
  # note this
  # https://machinelearningmastery.com/how-to-handle-big-p-little-n-p-n-in-machine-learning/
  
  p <- ncol(df_yearMo2)
  s <- nrow(df_yearMo2)
  
  # remove_constants_fun <- function(df){
  # 
  # 
  #   list_uniquevars <- apply(df,2, function(spalte){return(length(unique(spalte)))})
  # 
  #   list_drop_one <- which(list_uniquevars <= remove_unique_values)
  # 
  #   df <- df[,-list_drop_one]
  # 
  #   }

  if(s / p <= min_ratio_s_p){
    
    list_uniquevars <- apply(df_yearMo2,2, function(spalte){return(length(unique(spalte)))})
    
    remove_unique_values <- 1
    
    while(s / p <= min_ratio_s_p){
      
      list_drop_one <- which(list_uniquevars <= remove_unique_values)
      
      
      if(length(list_drop_one) == 0){
        
        
      } else {
         
        df_yearMo2 <- df_yearMo2[,-list_drop_one]
      } 
      
      p <- ncol(df_yearMo2)
      s <- nrow(df_yearMo2)
      
      remove_unique_values <- remove_unique_values + 1
      
    }
    
  }
  
  
  Party <- docvars(dfm,"party")
  
  df_yearMo2 <- cbind(df_yearMo2[,-1],Party)
  
  df_yearMo2 <- na.omit(df_yearMo2)
  
  df_yearMo2$Party <- as.factor(df_yearMo2$Party)
  
    
  # df_yearMo2 = df_yearMo2[complete.cases(df_yearMo2),complete.cases(df_yearMo2)]
  
  # df_yearMo2 %>% filter_all(all_vars(is.na(.)))
  
  ####3.1 Choose classifiers according to the balance between strong predictive power and simplicity, reproducibility, overfitting and computational time (see Appendix page 13)##
  
  ######################################################3.2 Build classifiers with specific parameters####################################################
  
  
  
  sample_split <- sample(1:nrow(df_yearMo2), floor(nrow(df_yearMo2) * train_size))
  
  train_data <- df_yearMo2[sample_split,]
  
  test_data <- df_yearMo2[ -sample_split,]
  
  
  # df_yearMo2[,1:ncol(df_yearMo2)-1] = apply(df_yearMo2[,1:ncol(df_yearMo2)-1],2,as.numeric)
  
  #table(train_data[,ncol(train_data)], useNA = "ifany")
  
  ## Apply model weights
  model_weights <- ifelse(train_data$Party == 0,
                          (1/table(train_data$Party)[1]) * 0.5,
                          (1/table(train_data$Party)[2]) * 0.5)
  
  
  ## 10 fold cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 10,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
  
  #df_yearMo2 <- as.matrix(c(apply(df_yearMo2[,-ncol(df_yearMo2)], 2, as.numeric), df_yearMo2[, ncol(df_yearMo2)]))
  
  
  #table(df_yearMo2[, ncol(df_yearMo2)], useNA = "ifany")
  
  ## 3.2.1  Random Forest Algorithm
  
  
  start = Sys.time()
  rf_trainer <- train(
                      #x = train_data[,1:ncol(train_data)-1],
                      #y = train_data$Party,
                      Party ~ .,
                      data = train_data,
                      trControl = ctrl,
                      #weights = model_weights,
                      method = "svmLinear",
                      metric = "ROC"
                      #scale = FALSE
                      #,ntree = 75,
                      #mrty = 10
                      )
  # rf_trainer <- randomForest(Party ~ ., data=train_data, importance=TRUE,
  #              proximity=TRUE)
  start-Sys.time()
  
  
  rf_pred = predict(rf_trainer,test_data[,1:ncol(test_data)-1])

  ## generate classification accuracy
  compare <- data.frame(prediction = rf_pred,
                        data = test_data[, ncol(test_data)]) %>%
    mutate(gleich = prediction == data) %>%
    group_by(gleich) %>%
    count() %>%
    ungroup() %>%
    mutate(share = n / sum(n))
  
  accuracy <- compare$share[compare$gleich == TRUE]
  
  return(accuracy)
}

save.image("sq.RData")

load("sq.RData")



accuracy_list <- sapply(list_yearModfm_propmax, classify_fun,
                        train_size = 0.8,
                        min_ratio_s_p = 0.02)


df_accuracy <- cbind.data.frame(accuracy_list,
                                yearMo)

write_csv2(df_accuracy, "accuracy_spdcdu_year.csv")
