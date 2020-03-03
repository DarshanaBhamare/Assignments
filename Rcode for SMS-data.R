library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)        
library(caret) 
sms_raw <- read.csv(file.choose())
head(sms_raw)
table(sms_raw$type)
prop.table(table(sms_raw$type))
spam <- subset(sms_raw, type == "spam")
wordcloud(spam$text,max.words = 50,random.order = FALSE,random.color = FALSE,colors=brewer.pal(10, "BrBG"))

ham <- subset(sms_raw, type == "ham")
wordcloud(ham$text, max.words = 50, random.order = TRUE,random.color = FALSE,colors=brewer.pal(10, "BrBG"))
# VectorSource() function will create one document for each sms text message. 
#Vcorpus() function to create a volatile corpus from these individual text messages
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_dtm <- DocumentTermMatrix(sms_corpus, control = 
                                list(tolower = TRUE,removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE, stemming = TRUE))
dim(sms_dtm)
#Training & Test set
sms_dtm_train <- sms_dtm[1:3369, ]
sms_dtm_test <- sms_dtm[3370:4812, ]
#Training & Test Label
sms_train_labels <- sms_raw[1:3369, ]$type
sms_test_labels <- sms_raw[3370:4812, ]$type

#Proportion for training & test labels
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# Creating vector of most frequent words
freq_words <- findFreqTerms(x = sms_dtm, 2,3)
str(freq_words)

#Filter the DTM
sms_dtm_freq_train <- sms_dtm_train[ , freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , freq_words]


class(sms_dtm_freq_train)
#Naive Bayes Classifier Computes the conditional a-posterior probabilities of a categorical class variable,hence converting to categorical
#here non zero to yes,zero to no;to state whether a specific term is present in the document.
values <- function(x)
  {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,values)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,values)

#Creating model from the training dataset
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Make predictions on test set
sms_test_pred <- predict(sms_classifier, sms_test)
confusionMatrix(sms_test_pred,sms_test_labels)
#Confusion Matrix and Statistics

#Reference Prediction  ham spam
#                 ham  1217  182
#                 spam   23   21

#           Accuracy : 0.8579          
