##--------------------------------------------
##
## Course Project: UFO Sighting data analysis
##
## Class: PCE Data Science Methods Class
##
## Name: John Senft
##
## Due Date: March 14, 2017
##
## Data Sources: 
## https://www.census.gov/library/publications/2011/compendia/statab/131ed/population.html
## https://www.kaggle.com/NUFORC/ufo-sightings
## http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
## Twitter post search via twitter api
##--------------------------------------------

# Load libraries
library(ggplot2)
library(twitteR)
library(maps)
library(readxl)
library(stringr)
library(tm)
library(dplyr)
library(futile.logger)

# Load functions
trim.trailing <- function (x) sub("\\s+$", "", x)

load_my_csv <- function(filename) {
  stopifnot(!is.null(filename))
  data = read.csv(filename, stringsAsFactors = FALSE)
  flog.info("Data has loaded from: %s", filename, name="fp9")
  return(data)
}

add_tweets <- function(x, y) {
  setup_twitter_oauth(TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)
  tweets <- searchTwitter('ufo -filter:retweets', n=x, since=y, lang='en')
  tw_df <- twListToDF(tweets)
  return(tw_df)
}

# Tests if non-null output is returned using sample data
test_return_val = function(){
  one_pass_outcome = add_tweets(1, '2017-03-10')
  stopifnot(!is.null(one_pass_outcome))
  flog.info("Unit test passed!", name = "fp9")
}

if(interactive()){
  
  setwd("~/code/uw_dsci/DS350/Homework/Project")
  
  # Setup Logging
  log_file_name = "final_proj_log.log"
  flog.threshold(INFO, name = "fp9")
  flog.appender(appender.file(log_file_name), name="fp9")
  flog.info("Script created by: John Senft", name = "fp9")
  
  # Run unit test
  test_return_val()
  
  ## --------------------------- Process ufo sighting data from NUFORC ------------------------------
  
  # Read in kaggle UFO database
  ufo_db <- load_my_csv("src/scrubbed.csv")
  state_abv <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", "id", "il", "in", "ia", "ks",
                 "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc",
                 "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tx", "tn", "ut", "vt", "va", "wa", "wv", "wi",
                 "wy")
  ufo_db_us <- filter(ufo_db, state %in% state_abv)
  ufo_db_us <- data.table(ufo_db_us)
  
  # Find counts per state in table, add state names for mapping using external excel file
  state_counts <- ufo_db_us[, .(count = .N), by = state]
  state_counts$state <- toupper(state_counts$state)
  state_counts$Abbreviation <- state_counts$state
  state_counts$state <- NULL
  state_names <- load_my_csv("src/states.csv")
  state_counts <- merge(state_counts, state_names, by="Abbreviation")
  state_counts$State <- tolower(state_counts$State)
  state_counts$Abbreviation <- NULL
  state_counts$region <- state_counts$State
  state_counts$State <- NULL
  ufo_barplot <- barplot(state_counts$count, las = 2, names.arg = state_counts$region, col = c(2, 3),
          ylab = "Number of Sightings")
  text(ufo_barplot, state_counts$count, labels = format(state_counts$count, 4),
       pos = 3, cex = .4)
  
  # Plot map of states lit up by sightings per state
  us_map <- map_data("state")
  counts_map <- merge(us_map, state_counts, by="region")
  gg <- ggplot()
  gg <- gg + geom_map(data=us_map, map=us_map,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  gg <- gg + geom_map(data=counts_map, map=us_map,
                      aes(fill=counts_map$count, map_id=region),
                      color="#ffffff", size=0.15)
  gg <- gg + scale_fill_continuous(low='#d6f1fc', high='darkblue', guide='colorbar')
  gg <- gg +theme_bw()  + labs(fill = "Sightings per state", x="", y="")
  gg + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())
  
  # Read in 2010 census population data and get total state populations
  pop2010_totals <- read_excel("src/12s0016.xls", sheet = 1, skip = 6, col_names = FALSE)
  pop2010_totals <- data.table(pop2010_totals)
  pop2010_totals <- pop2010_totals[, .(X1, X2 * 1000)]
  names(pop2010_totals) <- c("region", "population")
  pop2010_totals <- pop2010_totals[complete.cases(pop2010_totals),]
  pop2010_totals <- pop2010_totals[c(6:13, 15:56)] # Remove non-state rows
  pop2010_totals$region <- trim.trailing(pop2010_totals$region)
  pop2010_totals$region <- tolower(pop2010_totals$region)
  
  # Want sightings as a percentage of state population since 2010
  ufo_db_us$datetime <- as.Date(ufo_db_us$datetime, format="%m/%d/%Y")
  ufo_db_us_2010 <- subset(ufo_db_us, ufo_db_us$datetime >= "2010-01-01")
  ufo_db_us_2010 <- ufo_db_us_2010[, .(count = .N), by = state]
  ufo_db_us_2010$state <- toupper(ufo_db_us_2010$state)
  ufo_db_us_2010$Abbreviation <- ufo_db_us_2010$state
  ufo_db_us_2010$state <- NULL
  ufo_db_us_2010 <- merge(ufo_db_us_2010, state_names, by="Abbreviation")
  ufo_db_us_2010$State <- tolower(ufo_db_us_2010$State)
  ufo_db_us_2010$Abbreviation <- NULL
  ufo_db_us_2010$region <- ufo_db_us_2010$State
  ufo_db_us_2010$State <- NULL
  ufo_pop_merge <- merge(ufo_db_us_2010, pop2010_totals, by="region")
  ufo_pop_merge <- ufo_pop_merge[, .(pct_pop = count/population * 100), by = region]
  
  # Map sightings as a percentage of state population since 2010
  pctpop_2010_map <- merge(us_map, ufo_pop_merge, by="region")
  gg2010 <- ggplot()
  gg2010 <- gg2010 + geom_map(data=us_map, map=us_map,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  gg2010 <- gg2010 + geom_map(data=pctpop_2010_map, map=us_map,
                      aes(fill=pctpop_2010_map$pct_pop, map_id=region),
                      color="#ffffff", size=0.15)
  gg2010 <- gg2010 + scale_fill_continuous(low='#F5D9D0', high='darkred', guide='colorbar')
  gg2010 <- gg2010 +theme_bw()  + labs(fill = "Per Capita Sightings", x="", y="")
  gg2010 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())
  
  ## -------------------------------- Process twitter data ----------------------------------------
  
  # Load Twitter credentials
  tw_creds <- load_my_csv('creds/tw_creds_JJS.csv')
  TWITTER_ACCESS_TOKEN <- tw_creds$TWITTER_ACCESS_TOKEN
  TWITTER_ACCESS_SECRET <- tw_creds$TWITTER_ACCESS_SECRET
  
  # Grab new tweets and append to aggregate, clean up
  tw_df_ufo_more <- add_tweets(2000, '2017-03-12')
  tw_df_ufo_more$text <- sapply(tw_df_ufo_more$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  tw_full_df <- rbind(tw_full_df, tw_df_ufo_more)
  tw_full_df <- subset(tw_full_df, !duplicated(tw_full_df$text))
  
  # Replace state abbr with state names, then remove the space in the 10 names that have one
  i <- 1
  for(i in seq(tw_full_df$text)){
    tw_full_df$text[[i]] <- gsub(" ME ", " maine ", tw_full_df$text[[i]])
    tw_full_df$text[[i]] <- gsub(" IN ", " indiana ", tw_full_df$text[[i]])    
    tw_full_df$text[[i]] <- gsub(" OH ", " ohio ", tw_full_df$text[[i]])
    tw_full_df$text[[i]] <- gsub(" OK ", " oklahoma ", tw_full_df$text[[i]])
    tw_full_df$text[[i]] <- gsub(" OR ", " oregon ", tw_full_df$text[[i]])
  }
  
  # Replace state abbr with state names and remove the space in the 10 names that have one
  j <- 1
  for(j in seq(tw_full_df$text)){
    tw_full_df$text[[j]] <- gsub('([[:upper:]])', '\\L\\1', tw_full_df$text[[j]], perl = TRUE)
    tw_full_df$text[[j]] <- gsub("north carolina", " northcarolina ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("north dakota", " northdakota ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("new hampshire", " newhampshire ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("new jersey", " newjersey ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("new mexico", " newmexico ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("new york", " newyork ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("rhode island", " rhodeisland ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("south carolina", " southcarolina ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("south dakota", " southdakota ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("west virginia", " westvirginia ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nc ", " northcarolina ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nd ", " northdakota ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nh ", " newhampshire ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nj ", " newjersey ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nm ", " newmexico ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ny ", " newyork ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ri ", " rhodeisland ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" sc ", " southcarolina ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" sd ", " southdakota ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" wv ", " westvirginia ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" al ", " alabama ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ak ", " alaska ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" az ", " arizona ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ar ", " arkansas ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ca ", " california ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" co ", " colorado ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ct ", " connecticut ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" de ", " delaware ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" fl ", " florida ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ga ", " georgia ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" hi ", " hawaii ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ia ", " iowa ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" id ", " idaho ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" il ", " illinois ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ks ", " kansas ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ky ", " kentucky ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" la ", " louisiana ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ma ", " massachusetts ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" md ", " maryland ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" mi ", " michigan ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" mn ", " minnesota ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" mo ", " missouri ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ms ", " mississippi ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" mt ", " montana ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ne ", " nebraska ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" nv ", " nevada ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" pa ", " pennsylvania ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" tn ", " tennessee ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" tx ", " texas ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ut ", " utah ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" va ", " virginia ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" wa ", " washington ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" wi ", " wisconsin ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" wy ", " wyoming ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)\\s", " ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("http://t.co/[a-z,A-Z,0-9]*{8}", " ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("https://t.co/[a-z,A-Z,0-9]*{8}", " ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("@\\w+", " ", tw_full_df$text[[j]])
    tw_full_df$text[[j]] <- gsub("#\\w+", " ", tw_full_df$text[[j]])
  }
  
  # Convert tweet df to word corpus, corpus preprocessing
  tw_full_corpus <- Corpus(VectorSource(tw_full_df$text))
  myStopwords <- c("ufo", "sighting", stopwords("english"))
  tw_full_corpus <- tm_map(tw_full_corpus, removeWords, myStopwords)
  tw_full_corpus <- tm_map(tw_full_corpus, content_transformer(tolower))
  tw_full_corpus <- tm_map(tw_full_corpus, removePunctuation)
  tw_full_corpus <- tm_map(tw_full_corpus, removeNumbers)
  tw_full_corpus <- tm_map(tw_full_corpus, stripWhitespace) 
  
  # Create document term matrix, convert to data frame
  dtm <- DocumentTermMatrix(tw_full_corpus)
  tw_corpus_matrix = as.matrix(dtm)
  tw_words_df = as.data.frame(tw_corpus_matrix)
  
  # Search term matrix for state name matches
  search_states = c()
  search_states = gsub("[[:space:]]", "", states$region)
  tw_freq_words = colSums(tw_corpus_matrix)
  tw_freq_words = data.frame(tw_freq_words)
  tw_freq_words <- data.table(tw_freq_words, keep.rownames = TRUE)
  names(tw_freq_words) = c("region", "count")
  words_states <- tw_freq_words[region %in% search_states, .(region, count)]
  
  # Modify state names by adding space back into the 10 names needing it for mapping
  k <- 1
  for (k in seq(words_states$region)){
    words_states$region[k] <- gsub("northcarolina", "north carolina", words_states$region[k])
    words_states$region[k] <- gsub("northdakota", "north dakota", words_states$region[k])
    words_states$region[k] <- gsub("newhampshire", "new hampshire", words_states$region[k])
    words_states$region[k] <- gsub("newjersey", "new jersey", words_states$region[k])
    words_states$region[k] <- gsub("newmexico", "new mexico", words_states$region[k])
    words_states$region[k] <- gsub("newyork", "new york", words_states$region[k])
    words_states$region[k] <- gsub("rhodeisland", "rhode island", words_states$region[k])
    words_states$region[k] <- gsub("southcarolina", "south carolina", words_states$region[k])
    words_states$region[k] <- gsub("southdakota", "south dakota", words_states$region[k])
    words_states$region[k] <- gsub("westvirginia", "west virginia", words_states$region[k])
  }
  
  # Want to merge in ratio of sightings from total sightings for each state
  ufo_pct_count <- ufo_db_us_2010[, .(pct_count = count/sum(ufo_db_us_2010$count)), by = region]
  words_states <- merge(words_states, ufo_pct_count, by="region")
  
  # Look at twitter count barplot
  tw_barplot <- barplot(words_states$count, las = 2, names.arg = words_states$region, col = c(2, 3),
          ylab = "Number of Sightings")
  text(tw_barplot, words_states$count, labels = format(words_states$count, 4),
       pos = 3, cex = .4)
  
  # Look at twitter count on us map graphic
  tweet_map <- merge(us_map, words_states, by="region")
  ggtw <- ggplot()
  ggtw <- ggtw + geom_map(data=us_map, map=us_map,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  ggtw <- ggtw + geom_map(data=tweet_map, map=us_map,
                      aes(fill=tweet_map$count, map_id=region),
                      color="#ffffff", size=0.15)
  ggtw <- ggtw + scale_fill_continuous(low='#c5f9d0', high='darkgreen', guide='colorbar')
  ggtw <- ggtw +theme_bw()  + labs(fill = "Sightings per state", x="", y="")
  ggtw + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())
  
  # Look at basic stats
  ufo_mean <- mean(ufo_db_us_2010$count)
  tw_mean <- mean(words_states$count)
  
  # Look at chi-sq stats
  # H0: NUFORC sightings are a pridictor of twitter ufo posts.
  # Ha: NUFORC sightings are not a pridictor of twitter ufo posts.
  ufo_cs_test <- chisq.test(words_states$count, p = words_states$pct_count)
  
  # Log results
  flog.info("UPDATED Chi-Squared test results are as follows:", ufo_cs_test, capture=T, name="fp9")
}
##--------------------------------End of Script----------------------------------------