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
##--------------------------------------------

# Load libraries
library(ggplot2)
library(twitteR)
library(maps)
library(readxl)
library(stringr)
library(tm)
library(RTextTools)
library(SnowballC)
library(dplyr)

setwd("~/code/uw_dsci/DS350/Homework/Project")

# Load functions
trim.trailing <- function (x) sub("\\s+$", "", x)

# Setup Twitter OAuth
tw_creds <- read.csv('tw_creds_JJS.csv', stringsAsFactors = FALSE)
TWITTER_ACCESS_TOKEN <- tw_creds$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET <- tw_creds$TWITTER_ACCESS_SECRET

# Build Twitter api session
setup_twitter_oauth(TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)

# Acquire tweets and return dataframe
add_tweets <- function(x) {
  tweets <- searchTwitter('ufo -filter:retweets', n=2000, since=x, lang='en')
  tw_df <- twListToDF(tweets)
  return(tw_df)
}

# Grab new tweets and append to aggregate, clean up
tw_df_ufo_more <- add_tweets('2017-03-08')
tw_df_ufo_more$text <- sapply(tw_df_ufo_more$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
tw_full_df <- rbind(tw_full_df, tw_df_ufo_more)
tw_full_df <- subset(tw_full_df, !duplicated(tw_full_df$text))

# Read in UFO database
ufo_db <- read.csv("scrubbed.csv", stringsAsFactors=FALSE)
state_abv <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", "id", "il", "in", "ia", "ks",
               "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc",
               "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tx", "tn", "ut", "vt", "va", "wa", "wv", "wi",
               "wy")
ufo_db_us <- filter(ufo_db, state %in% state_abv)
ufo_table <- data.table(ufo_db_us)

# Find counts per state in table, add state names for mapping
state_counts <- ufo_table[, .(count = .N), by = state]
state_counts$state <- toupper(state_counts$state)
state_counts$Abbreviation <- state_counts$state
state_counts$state <- NULL
state_names <- read.csv("states.csv", header = TRUE, stringsAsFactors = FALSE)
state_names <- data.table(state_names)
states <- merge(state_counts, state_names, by="Abbreviation")
states$State <- tolower(states$State)
states$Abbreviation <- NULL
states$region <- states$State
states$State <- NULL
barplot(states$count, las = 2, names.arg = states$region, col = c(2, 3))

# Plot map of states lit up by sightings per state
us_map <- map_data("state")
counts_map <- merge(us_map, states, by="region")

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=counts_map, map=us_map,
                    aes(fill=counts_map$count, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='#d6f1fc', high='darkblue', 
                                 guide='colorbar')
gg <- gg +theme_bw()  + labs(fill = "Sightings per state"
                              ,title = "Heatmap of UFO Sightings Reported since 1949", x="", y="")
gg + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())

# Read in population data and clean up data
excel_sheets("12s0016.xls")
pop2010 <- read_excel("12s0016.xls", sheet = 1, skip = 6, col_names = FALSE)
pop2010 <- data.table(pop2010)
cols <- c("region", "population")
pop2010_totals <- pop2010[, .(X1, X2 * 1000)]
names(pop2010_totals) <- cols
pop2010_totals <- pop2010_totals[complete.cases(pop2010_totals),]
pop2010_totals <- pop2010_totals[c(6:13, 15:56)] # Remove all but states
pop2010_totals$region <- trim.trailing(pop2010_totals$region)
pop2010_totals$region <- tolower(pop2010_totals$region)

# Want sightings as a percentage of state population since 2010
ufo_table$datetime <- as.Date(ufo_table$datetime, format="%m/%d/%Y")
ufo_table_2010 <- subset(ufo_table, ufo_table$datetime >= "2010-01-01")
state_counts_2010 <- ufo_table_2010[, .(count = .N), by = state]
state_counts_2010$state <- toupper(state_counts_2010$state)
state_counts_2010$Abbreviation <- state_counts_2010$state
state_counts_2010$state <- NULL
states_2010 <- merge(state_counts_2010, state_names, by="Abbreviation")
states_2010$State <- tolower(states_2010$State)
states_2010$Abbreviation <- NULL
states_2010$region <- states_2010$State
states_2010$State <- NULL
ufo_pop_merge <- merge(states_2010, pop2010_totals, by="region")
ufo_pctpop_2010 <- ufo_pop_merge[, .(pct_pop = count/population * 100), by = region]

# Map sightings as a percentage of state population since 2010
pctpop_2010_map <- merge(us_map, ufo_pctpop_2010, by="region")
gg2010 <- ggplot()
gg2010 <- gg2010 + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg2010 <- gg2010 + geom_map(data=pctpop_2010_map, map=us_map,
                    aes(fill=pctpop_2010_map$pct_pop, map_id=region),
                    color="#ffffff", size=0.15)
gg2010 <- gg2010 + scale_fill_continuous(low='#F5D9D0', high='darkred', 
                                 guide='colorbar')
gg2010 <- gg2010 +theme_bw()  + labs(fill = "Per Capita Sightings"
                             ,title = "Heatmap of UFO Sightings as Percent of Population since 2010"
                             , x="", y="")
gg2010 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c())
                                        + theme(panel.border = element_blank())


# Replace state abbr & names with spaces
j <- 1
for(j in seq(tw_full_df$text)){   
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
  tw_full_df$text[[j]] <- gsub(" In ", " indiana ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ks ", " kansas ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ky ", " kentucky ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" la ", " louisiana ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ma ", " massachusetts ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" md ", " maryland ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" Me ", " maine ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" mi ", " michigan ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" mn ", " minnesota ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" mo ", " missouri ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ms ", " mississippi ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" mt ", " montana ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ne ", " nebraska ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" nv ", " nevada ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" oh ", " ohio ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" ok ", " oklahoma ", tw_full_df$text[[j]])
  tw_full_df$text[[j]] <- gsub(" Or ", " oregon ", tw_full_df$text[[j]])
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
 
tw_full_df$text[[1]]

# Convert tweet df to word corpus, corpus preprocessing
tw_full_corpus <- Corpus(VectorSource(tw_full_df$text))
myStopwords <- c("ufo", "sighting", stopwords("english"))
tw_full_corpus <- tm_map(tw_full_corpus, removeWords, myStopwords)
tw_full_corpus <- tm_map(tw_full_corpus, content_transformer(tolower))
tw_full_corpus <- tm_map(tw_full_corpus, removePunctuation)
tw_full_corpus <- tm_map(tw_full_corpus, removeNumbers)
tw_full_corpus <- tm_map(tw_full_corpus, stripWhitespace) 

inspect(tw_full_corpus[[3]])

# Create document term matrix
dtm <- DocumentTermMatrix(tw_full_corpus)

tw_corpus_matrix = as.matrix(dtm)
tw_words_df = as.data.frame(tw_corpus_matrix)

# Search tweets for state matches and sum counts for each state
search_states = states$region
search_states$region = gsub("[[:space:]]", "", states$region)
tw_freq_words = colSums(tw_corpus_matrix)
tw_freq_words = data.frame(tw_freq_words)
tw_freq_words <- data.table(tw_freq_words, keep.rownames = TRUE)
names(tw_freq_words) = c("state", "count")
words_states <- tw_freq_words[word %in% search_states$region, .(word, count)]


# Plot hist of states and superimpose line of tweets

# Quote correlation stats, r-squared, and error

## Use qqplot for project plots

## Data Sources: 
# https://www.census.gov/library/publications/2011/compendia/statab/131ed/population.html
# https://www.kaggle.com/NUFORC/ufo-sightings
# http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/
