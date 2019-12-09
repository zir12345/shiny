#-- =============================================
#-- Author: Tayfun.Duelger@nttdata.com   
#-- Create date: 01.10.2019 
#-- Last update: 11.10.2019
#-- Description: Instagram Sentiment Analysis on BMW 
#-- Version: 2.2
#-- =============================================
  
################################################### PART 1 Preprocessing Instagram Posts on BMW
##### load data
insta_post <- dbReadTable(con, "INSTA_TR_BMW_v4")

#### delete duplicates, i.e. spam
dublicates <- insta_post[duplicated(insta_post$media_text),]
insta_post <- insta_post[!duplicated(insta_post$media_text), ]

#### delete timestamps in date-column
insta_post$posted_time <- as.character(insta_post$posted_time)
insta_post$posted_time <- word(insta_post$posted_time, 1)
insta_post$posted_time <- as.Date(insta_post$posted_time, format = "%Y-%m-%d")
insta_post$media_text <- as.character(insta_post$media_text)

#### filter instagram posts for 2019
insta_post_18 <- insta_post %>% filter(posted_time >= "2018-01-01" & posted_time <= "2018-12-31")
insta_post <- insta_post %>% filter(posted_time >= "2019-01-01")

#### Get rid of special characters and symbols
clean_tweet = gsub("&amp", "", insta_post$media_text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 

#### Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*\\{8\\}","")

#### Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")

#### Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")

#### Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")  
clean_tweet <- str_remove(clean_tweet, "[newline]")

#### overwrite old column with cleaned tweets
insta_post$media_text <- clean_tweet

#### convert to corpus object
insta_post_corp <- corpus(insta_post$media_text)

#### tokenize all documents
insta_toks <- tokens(insta_post_corp, ngrams=1, remove_numbers = FALSE, remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, remove_url = TRUE)

#### stem all words
insta_toks_stemmed <- tokens_wordstem(insta_toks)

#### remove stopwords
insta_toks_stemmed_no_stopwords <- tokens_remove(insta_toks_stemmed, pattern = stopwords("en"))

#### remove other irrelevant words
list <- dbReadTable(con, "00_MA_TEXTANALYSES_REDUNDANT_WORDLIST")
insta_toks_stemmed_no_stopwords <- tokens_remove(insta_toks_stemmed_no_stopwords, pattern = list)

#insta_toks_stemmed_no_stopwords <- tokens_remove(insta_toks_stemmed_no_stopwords, pattern = c("and","the","our","that","for","are","also","more","has","must","have","should","this","with","", "NEWLINE", "advertising", "see","newline"))

#### transform into lowercase letters
insta_toks_stemmed_no_stopwords_lower <- tokens_tolower(insta_toks_stemmed_no_stopwords)

################################################### PART 2 Perform sentiment analysis
#### look up sentiment via nrc-dictionary
results_nrc <- tokens_lookup(insta_toks_stemmed_no_stopwords_lower, dictionary = data_dictionary_NRC)
results_nrc_df <- convert(dfm(results_nrc), to = "data.frame")
#dbWriteTable(con, "INSTA_TR_BMW_v4_MEDIUMRARE_MOOD",results_nrc_df, overwrite = TRUE)

#### scaling and scoring for nrc
results_nrc_df$sentiment_score <- results_nrc_df$positive - results_nrc_df$negative
results_nrc_df$sentiment_scaled <- results_nrc_df$sentiment_score / (results_nrc_df$positive + results_nrc_df$negative)
results_nrc_df$sentiment_scaled <- replace_na(tidyr::results_nrc_df$sentiment_scaled, 0)
results_nrc_df$sentiment_binary <- ifelse(results_nrc_df$sentiment_score>0, "positive", "negative")

####  replacing zeros with "neutral" for nrc
for (i in 1:3972){
  if (results_nrc_df$positive[i] - results_nrc_df$negative[i] == 0){ 
    results_nrc_df$sentiment_binary[i] <- "neutral"
  }else{
    results_nrc_df$sentiment_binary[i] <- results_nrc_df$sentiment_binary[i]
  }
}


#### look up sentiment via nrc-dictionary
results_afinn <- tokens_lookup(insta_toks_stemmed_no_stopwords_lower, dictionary =data_dictionary_AFINN)
results_afinn_df <- convert(dfm(results_afinn), to = "data.frame")
#dbWriteTable(con, "INSTA_TR_BMW_v4_MEDIUMRARE_MOOD_2",results_afinn_df, overwrite = TRUE)

#### scaling and scoring for afinn
results_afinn_df$sentiment_score <- results_afinn_df$positive - results_afinn_df$negative
results_afinn_df$sentiment_scaled <- results_afinn_df$sentiment_score / (results_afinn_df$positive + results_afinn_df$negative)
results_afinn_df$sentiment_scaled <- replace_na(results_afinn_df$sentiment_scaled, 0)
results_afinn_df$sentiment_binary <- ifelse(results_afinn_df$sentiment_score>0, "positive", "negative")

#### replacing zeros with "neutral" for afinn

for (i in 1:3972){
  if (results_afinn_df$positive[i] - results_afinn_df$negative[i] == 0){ 
    results_afinn_df$sentiment_binary[i] <- "neutral"
  }else{
    results_afinn_df$sentiment_binary[i] <- results_afinn_df$sentiment_binary[i]
  }
}

################################################### PART 2.1 Calculation of Daily Sentiment Scores with NRC-Dictionary
#### create time-series-ready dataframe with date-column
ts_df <- data.frame(date=insta_post$posted_time)
ts_df <- cbind(ts_df, positive = results_nrc_df$positive)
ts_df <- cbind(ts_df, negative = results_nrc_df$negative)

ts_df2 <- data.frame(date=insta_post$posted_time)
ts_df2 <- cbind(ts_df2, positive = results_afinn_df$positive)
ts_df2 <- cbind(ts_df2, negative = results_afinn_df$negative)

#### extract positive and negative sentiment scores and aggregate them for each day separately
agg_pos <- aggregate(ts_df$positive, by=list(date=ts_df$date), FUN=sum)
agg_neg <- aggregate(ts_df$negative, by=list(date=ts_df$date), FUN=sum)

agg_pos2 <- aggregate(ts_df2$positive, by=list(date=ts_df2$date), FUN=sum)
agg_neg2 <- aggregate(ts_df2$negative, by=list(date=ts_df2$date), FUN=sum)

#### calculate daily scores and scale them 
agg_pos_neg <- data.frame(date=agg_pos$date)
agg_pos_neg$daily_score <- (agg_pos$x - agg_neg$x)
ts_df <- cbind(agg_pos_neg, daily_scaled_score = agg_pos_neg$daily_score / (agg_pos$x + agg_neg$x))

agg_pos_neg2 <- data.frame(date=agg_pos2$date)
agg_pos_neg2$daily_score <- (agg_pos2$x - agg_neg2$x)
ts_df2 <- cbind(agg_pos_neg2, daily_scaled_score = agg_pos_neg2$daily_score / (agg_pos2$x + agg_neg2$x))

#### since 0 is not defined for division, manually transform nan-values into correct values
fix_nan <- function(x){
  x[is.nan(x)] <- 0
  x
}

ts_df$daily_scaled_score <- fix_nan(ts_df$daily_scaled_score)

fix_nan <- function(x){
  x[is.nan(x)] <- 0
  x
}

ts_df2$daily_scaled_score <- fix_nan(ts_df2$daily_scaled_score)

#### plot the time-series
ts_df$month <- month(ts_df$date)
x <- aggregate(ts_df[, 2:3], list(ts_df$month), mean)

ts_plot <- ggplot(x, aes(x=Group.1, daily_scaled_score)) %>%
  + geom_line(col="#6785C1", size=1)+ ggtitle("Average Positive vs. Negative Posts on Monthly Basis (NRC), 2019")+ xlab("Month (2019)") + ylab("scaled_score") + scale_x_continuous(breaks=seq(1, 12, 1))
ts_plot

ts_df2$month <- month(ts_df2$date)
x2 <- aggregate(ts_df2[, 2:3], list(ts_df2$month), mean)

ts_plot2 <- ggplot(x2, aes(x=Group.1, daily_scaled_score)) %>%
  + geom_line(col="#6785C1", size=1) + ggtitle("Average Positive vs. Negative Posts on Monthly Basis (AFINN), 2019") + xlab("Month (2019)") + ylab("scaled_score") + scale_x_continuous(breaks=seq(1, 12, 1))
ts_plot2

################################################### PART 2.1 Calculation of Daily Sentiment Scores with NRC-Dictionary

# Step 3: Calculation of Granger Causality #

############################################################################
############################################################################

## Create time-series

# time-series must be stationary 

options(warn=-1)
adf.test(ts_df_month$transformed) # you can reject the null hypothesis that your time-series is non-stationary
# two series to be tested for granger causality (whether a causes b or vice versa)

############################################################################

# do the same for the bmw stock prices

bmw <- read.csv("~/projects/gumshoos/Insta_Sentiment/BMW.DE.csv", sep = ",")
bmw$Date <- as.Date(bmw$Date, format = "%Y-%m-%d")

ggplot(bmw, aes(Date, Close)) + geom_line() + ggtitle("BMW Adjusted Closing Prices") + xlab("") + ylab("Adj. Closing Price")

############################################################################

# test for stationary time-series

adf.test(bmw$Adj.Close) # you canNOT reject the null hypothesis that your time-series is non-stationary
adf.test(diff(bmw$Adj.Close)) # is stationary
ndiffs(bmw$Adj.Close, test="adf") # only 1 difference is necessary to become stationary

############################################################################

# take last month's adj.closing price as the monthly price (BMW)

bmw_ts <- setDT(bmw)[order(Date), .(Adj.Close[which.max(Date)], Date[which.max(Date)]), by = .(year(Date), month(Date))]
bmw_ts$year <- NULL
bmw_ts$month <- NULL
colnames(bmw_ts) <- c("Close", "Date")
bmw_ts <- bmw_ts[,c(2,1)]
bmw_ts$Date <- as.character(bmw_ts$Date)
bmw_ts$Date <- substr(bmw_ts$Date,1,nchar(bmw_ts$Date)-3)
bmw_ts <- data.frame(bmw_ts)

############################################################################

# merge sentiment scores into one table

insta_nrc <- as.numeric(ts_df_month$transformed)
insta_afinn <- as.numeric(ts_df_month2$transformed)
ts_granger <- data.frame(date=agg_pos_neg_month$date, nrc=insta_nrc, afinn=insta_afinn)
ts_granger <- ts_granger[-c(1),]

# diff() and log() the closing prices to make the time-series stationary and clean

ts_granger$bmw_stock <- cbind((diff(log(bmw_ts$Close))))

############################################################################

# z-standardize all scores to make them comparable 

ts_granger$nrc <- scale(ts_granger$nrc, center = TRUE, scale = TRUE)
ts_granger$afinn <- scale(ts_granger$afinn, center = TRUE, scale = TRUE)
ts_granger$bmw_stock <- scale(ts_granger$bmw_stock, center = TRUE, scale = TRUE)

############################################################################

# all time-series overlayed in one plot

ggplot(ts_granger, aes(date, bmw_stock, show.legend = TRUE)) + geom_line() + geom_point() + geom_line(data=ts_granger,aes(date, afinn), color="red", show.legend = TRUE) + geom_line(data=ts_granger,aes(date, nrc), color="blue") + ggtitle("Sentiments and BMW Stock Prices") + ylab("bmw_scaled_diff_log") + theme(legend.justification = "top",  legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6)) + scale_colour_manual(name = 'Legend', guide = 'legend', values = c('bmw_stock' = 'black','afinn' = 'red', "nrc" ="blue"), labels = c("BMW Stock","AFINN","NRC"))


#(25.April-3.Mai BMW-Open; 27.Mai neuer 1er; 31.Mai Strafzölle von China(und schwache Konjunktur) vs. USA; 3. Juni neues Werk in Mexiko und Strafzölle USA vs. Mexiko;
#05.Juni Koop. mit Jaguar Land Rover E-Autos, 18. Juni neuer Chef Oliver Zipse)

############################################################################

# all time-series in separate plots

ggplot(bmw, aes(Date, Close)) + geom_line() + geom_point() + ggtitle("BMW Adjusted Closing Prices") + xlab("") + ylab("scaled_adj_closing_price_log") + scale_y_continuous(trans=log2_trans())
ggplot(ts_df_month, aes(date, transformed)) + geom_line(color="blue") + geom_point() + ggtitle("Positive vs. Negative Posts (NRC)") + xlab("") + ylab("scaled_score_month_log")
ggplot(ts_df_month2, aes(date, transformed)) + geom_line(color="red") + geom_point() + ggtitle("Positive vs. Negative Posts (AFINN)") + xlab("") + ylab("scaled_score_month_log")

############################################################################

#####date format is causing problems

# create data-frames for VAR() to check optimal number of lags

afinn_stock <- ts_granger
afinn_stock$nrc <- NULL
afinn_stock$date <- NULL

nrc_stock <- ts_granger
nrc_stock$afinn <- NULL
nrc_stock$date <- NULL

############################################################################

# calculate optimal number of lags

VARselect(ts(afinn_stock,start = c(2019,2),frequency = 12), lag.max = 2)
VAR_afinn <- VAR(afinn_stock, p=2, type="const", lag.max = 6 , ic="AIC")

VARselect(ts(nrc_stock,start = c(2019,2),frequency = 12), lag.max = 2)
VAR_nrc <- VAR(nrc_stock, p=2, type="const", lag.max = 6 , ic="AIC")

############################################################################

# calculate causality: Option 1

causality(VAR_afinn, cause="afinn")$Granger #afinn does not Granger-cause bmw_stock
causality(VAR_afinn, cause="bmw_stock")$Granger

causality(VAR_nrc, cause="nrc")$Granger
causality(VAR_nrc, cause="bmw_stock")$Granger

# calculate causality: Option 2

grangertest(bmw_stock~afinn, order=1, data=ts_granger)
grangertest(afinn~bmw_stock, order=1, data=ts_granger)

grangertest(bmw_stock~nrc, order=1, data=ts_granger)
grangertest(nrc~bmw_stock, order=1, data=ts_granger)