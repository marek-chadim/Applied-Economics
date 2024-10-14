library(tm)
library(tidytext)
library(dplyr)
library(readr)
library(wordcloud)
library(glmnet)
library(topicmodels)
library(slam)
library(data.table)
library(e1071)
library(stringr)
library(tidyr)

rm(list = ls())
setwd('C:/Dropbox/Applied-Economics/task9')

# Read all speech-files into a corpus using the tm command VCorpus. Turn the data into a tibble with columns containing the name of file containing text, the word and row number.

indir='C:/Dropbox/Applied-Economics/task9/105'
senator_corpus = VCorpus(DirSource(indir))
senators_td = senator_corpus |>
tidy() |>
select(id, text) |>
mutate(id=str_match(id,"-(.*).txt")[,2]) |>
unnest_tokens(word, text) |>
group_by(id) |>
mutate(row=row_number()) |>
ungroup()


sen_party <- read.csv("sen105_party.csv",stringsAsFactors = FALSE)

names = sen_party |>
  mutate(word= tolower(lname)) |>
  select(word) 

states=as.data.frame(c(tolower(state.abb),tolower(state.name)))
colnames(states) = "word"

# Remove non-alphabetic characters, stopwords and other words that you find to be uninformative.

droplist=c("text","doc","docno")
senators_td2 = senators_td |>
  mutate(word=str_extract(word, "[a-z]+")) |>
  drop_na(word) |>
  filter(!(word %in% droplist)) |>
  anti_join(stop_words) |>
  anti_join(names) |>
  anti_join(states)

# Generate variables with bigrams and trigrams for each senator.

senator_bigram =senators_td2 |>
arrange(id,row) |>
group_by(id) |>
mutate(bigram = str_c(lag(word,1),word, sep=" ")) |>
filter(row==lag(row,1)+1) |>
select(-word) |>
ungroup()

senator_trigram =senators_td2 |>
arrange(id,row) |>
group_by(id) |>
mutate(trigram= str_c(lag(word,2),lag(word,1),word, sep=" ")) |>
filter(row==lag(row,1)+1 & lag(row,1)==lag(row,2)+1) |>
select(-word) |>
ungroup()

# Simple analysis
library(ggplot2)
wordlist=senators_td2 |>
count(word,sort=TRUE) |>
filter(row_number()<50) |>
mutate(word = reorder(word,n)) |>
ggplot(aes(word,n)) + geom_bar(stat="identity") + xlab(NULL) + coord_flip()

# Compute overall frequency lists for bigrams and trigrams. 
bigramlist=senator_bigram |>
count(bigram,sort=TRUE)

trigramlist=senator_trigram |>
count(trigram,sort=TRUE)

# What are the most frequent bigrams?
bigramlist
# and trigrams?
trigramlist


#Merge in party information.
used by each party
sen_party = sen_party |>
  mutate(id= paste0(tolower(lname), "-", tolower(stateab)))


# Compute frequency lists for bigrams and trigrams by party. Plot a wordcloud for the 50 words most frequently

bigramlist_p <- senator_bigram |>
inner_join(sen_party) |>
rename(word=bigram) |>
count(party,word,sort = TRUE) |>
group_by(party) |>
mutate(share=n()/sum(n()),rank=row_number()) |>
ungroup() 

trigramlist_p <- senator_trigram |>
inner_join(sen_party) |>
rename(word=trigram) |>
count(party,word,sort = TRUE) |>
group_by(party) |>
mutate(share=n()/sum(n()),rank=row_number()) |>
ungroup() 

#  Plot a wordcloud for the 50 words most frequently used by each party.
wordlist_p <- senators_td2 |>
inner_join(sen_party) |>
count(party,word,sort = TRUE) |>
group_by(party) |>
mutate(share=n()/sum(n()),rank=row_number()) |>
ungroup() 
library(reshape2)
wordlist_p |>
select(word,party,n) |>
acast(word ~ party, value.var = "n", fill = 0) |>
comparison.cloud(max.words=50) 


# Analysis

# Estimate a Lasso logit model predicting the party of the senator based on bigrams. 

wordlist_s2 <- senator_bigram |>
rename(word=bigram) |>
inner_join(sen_party) |>
count(id,party,word,sort=TRUE) |>
ungroup()

s <- wordlist_s2 |>
cast_sparse(id,word,n)
class(s)
s=s[order(rownames(s)),]
s2=wordlist_s2[order(rownames(wordlist_s2)),]

y=sen_party[order(sen_party$id),]
y<-as.matrix(y$party)
y<-as.factor(y)

library(glmnet)
set.seed(1)
train <- sample(1:nrow(s), nrow(s) * 0.8)
cv_lasso <- cv.glmnet(s[train,], y[train], alpha = 1, family = "binomial")
plot(cv_lasso)

lasso_pred <-predict(cv_lasso, s[-train,], s="lambda.min")
lasso_pred <- ifelse(lasso_pred<0,0,1)

table(predict=lasso_pred, truth=y[-train])

cv_lasso <- cv.glmnet(s, y, alpha = 1, family = "binomial")
lasso_best <- predict(cv_lasso, s = "lambda.min", type = "coefficients")
lasso_coef <- as.matrix(coef(cv_lasso, s = "lambda.min"))
coef_lasso <- data.frame(names = lasso_best@Dimnames[[1]][lasso_best@i + 1], coefficients = lasso_best@x)

# Bigrams most predictive of party
coef_lasso <- coef_lasso[coef_lasso$names != "(Intercept)", ]
coef_lasso |>
  arrange(desc(abs(coefficients))) |>
  head(10)

# LDA. 
# Estimate al LDA topic model with 5 topic based on the speeches by the senators.

senators_td3 = senators_td2[!is.na(senators_td2$word),]
senators_td3 = senators_td3 |>
mutate(d=cumsum(word=="docno")) 

droplist=c("text","doc","docno","") # 
senators_td3 = senators_td3[!(senators_td3$word %in% droplist),]
senators_td3 = senators_td3 |>
mutate(x=ifelse(d!=lag(d,1)|id!=lag(id,1),1,0) )|>
mutate(speech= cumsum(ifelse(is.na(lag(d,1)),0,x)))

wordlist_s <- senators_td3 |>
count(speech,word,sort=TRUE) |>
 ungroup()

wordlist=senators_td3 |>
count(word,sort=TRUE)
wordlist

wordlist_m50=wordlist |>
filter(n>50) |>
select(word)

wordlist_s <- wordlist_s |>
inner_join(wordlist_m50)

s <- wordlist_s |>
cast_sparse(speech,word,n)
class(s)

ap_lda10 <- LDA(s, k = 10, control = list(seed = 1234))
ap_topics <- tidy(ap_lda10, matrix = "beta")
ap_topics

# What ten words are most characteristic of each topic?

#most common
ap_top_terms <- ap_topics |>
group_by(topic) |>
slice_max(beta, n = 10) |>
ungroup() |>
arrange(topic, -beta)

ap_top_terms |>
mutate(term = reorder_within(term, beta,topic)) |>
ggplot(aes(beta,term,fill=factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free") + scale_y_reordered()

sumlogbeta <- ap_topics |>
mutate(logbeta = log(beta)) |>
group_by(term) |>
summarize(s_logbeta = sum(logbeta)) |>

#relative use measure
ap_top_terms2 <- ap_topics |>
inner_join(sumlogbeta) |>
mutate(logbeta = log(beta)) |>
mutate(term_score=beta*(log(beta)-s_logbeta/10)) |>
group_by(topic) |>
slice_max(term_score, n = 10) |>
ungroup() |>
arrange(topic, -term_score)

ap_top_terms2 |>
mutate(term = reorder_within(term, term_score,topic)) |>
ggplot(aes(beta,term,fill=factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scales = "free") + scale_y_reordered()