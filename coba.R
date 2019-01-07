install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("plyr")
install.packages("RTextTools")
install.packages("devtools")
install.packages("e1071")
install_github("sentiment140","okugami79")
install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

 require(devtools)
require(ggplot2)
library(e1071)
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(plyr)
library(RTextTools)
library(sentiment)
library(Rstem)

consumer_key <- 'ylTZG5CgPkDnw9McXdlNsPKCW'
consumer_secret <- '55rIdKXAQ4SKRz0P5i7WUcyj1LjUE5Z0Mru7lhhbOu1cm894F8'
access_token <- '2287839656-PdgW1LREWNkGI8hFd7rLGS8VTxRk748awu1cd27'
access_secret <- 'wLfOl2IHSAA6kpwClz8XDiOP5Jr9fVBX7HJjLxJ3brHRQ'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_j <- searchTwitter("#jokowi2periode", n=20,lang = "en")
tweets_p <- searchTwitter("#2019gantipresiden", n=50,lang = "en")
tweets_pil <- searchTwitter("#pilpres2019", n=50,lang = "en")

jokowi_tweets <- twListToDF(tweets_j)
prabowo_tweets <- twListToDF(tweets_p)
pil_tweets <- twListToDF(tweets_pil)

View(jokowi_tweets)
View(prabowo_tweets)
View(pil_tweets)



jokowi_text<- jokowi_tweets$text
prabowo_text<- prabowo_tweets$text
pil_text<- pil_tweets$text

#convert all text to lower case
jokowi_text<- tolower(jokowi_text)
prabowo_text<- tolower(prabowo_text)
pil_text<- tolower(pil_text)

# Replace blank space ("rt")
jokowi_text <- gsub("rt", "", jokowi_text)
prabowo_text <- gsub("rt", "", prabowo_text)
pil_text <- gsub("rt", "", pil_text)

# Replace @UserName
jokowi_text <- gsub("@\\w+", "", jokowi_text)
prabowo_text <- gsub("@\\w+", "", prabowo_text)
pil_text <- gsub("@\\w+", "", pil_text)


# Remove punctuation
jokowi_text <- gsub("[[:punct:]]", "", jokowi_text)
prabowo_text <- gsub("[[:punct:]]", "", prabowo_text)
pil_text <- gsub("[[:punct:]]", "", pil_text)


# Remove links
jokowi_text <- gsub("http\\w+", "", jokowi_text)
prabowo_text <- gsub("http\\w+", "", prabowo_text)
pil_text <- gsub("http\\w+", "", pil_text)

# Remove tabs
jokowi_text <- gsub("[ |\t]{2,}", "", jokowi_text)
prabowo_text <- gsub("[ |\t]{2,}", "", prabowo_text)
pil_text <- gsub("[ |\t]{2,}", "", pil_text)


# Remove blank spaces at the beginning
jokowi_text <- gsub("^ ", "", jokowi_text)
prabowo_text <- gsub("^ ", "", prabowo_text)
pil_text <- gsub("^ ", "", pil_text)

# Remove blank spaces at the end
jokowi_text <- gsub(" $", "", jokowi_text)
prabowo_text <- gsub(" $", "", prabowo_text)
pil_text <- gsub(" $", "", pil_text)

class_pol1 = classify_polarity(jokowi_text, algorithm="bayes")
class_pol2 = classify_polarity(prabowo_text, algorithm="bayes")
class_pol3 = classify_polarity(pil_text, algorithm="bayes")

#lihat hasilnya:
head(class_pol1, 20)
head(class_pol2, 20)
head(class_pol3, 20)

#fetch polarity category best_fit for our analysis purposes:
jokowiPol = class_pol1[,4]
prabowoPol = class_pol2[,4]
pilPol = class_pol3[,4]

#lihat hasilnya:
head(jokowiPol, 20)
head(prabowoPol, 20)
head(pilPol, 20)

#buat data frame dari hasil di atas:
jokowiSentimenDataFrame = data.frame(text=jokowi_text, polarity=jokowiPol, stringAsFactors=FALSE)
prabowoSentimentDataFrame = data.frame(text=class_pol2,polarity=prabowoPol, stringAsFactors=FALSE)
pilSentimentDataFrame = data.frame(text=class_pol3, polarity=pilPol, stringAsFactors=FALSE)

#tinjau polaritas sentimen (positif vs negatif), untuk melihat tingkat kepuasan pelanggan:
#plot distribusi polaritas tweet:
plotSentiments2 <- function(sentiment_dataframe, title)
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}
#plotting tweets polarity
plotSentiments2(jokowiSentimenDataFrame , "Polarity Analysis of Tweets on Twitter about Jokowi")
plotSentiments2(prabowoSentimentDataFrame, "Polarity Analysis of Tweets on Twitter about Prabowo")
plotSentiments2(pilSentimentDataFrame, "Polarity Analysis of Tweets on Twitter about Pilpres2019")

######


# comparison word cloud

comparison.cloud(pilSentimentDataFrame, colors = brewer.pal(nemo, "Dark2"),
                 
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
