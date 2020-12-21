# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(sentimentr)
library(tidytext)
library(stringr)
library(lubridate)
library(data.table)

# whatever name you assigned to your created app
appname <- "######"

## api key (example below is not a real key)
key <- "######"

## api secret (example below is not a real key)
secret <- "##########"

access_token = '####-#'
access_secret = '#########'

# create token named "twitter_token"
twitter_token <- create_token(
   app = appname,
   consumer_key = key,
   consumer_secret = secret,
   access_token = access_token,
   access_secret = access_secret)

#This adds a few extra stop words
stop_words2 = rbind(stop_words,
                    data.frame(word = c('im', 'dont', 'hes'), lexicon = NA))

#this is the cohort of screennames to loop through
user_names = c('martinonyc', 'NYPost_Mets', 'mikemayerMMO', 'TimBritton', 'Miss_Met', 'AnthonyDiComo', 'PSLToFlushing', 'timbhealey')
mets_tweets = c()

#Loops through each users timeline to get their most recent tweets (I think it maxes around 3200-3300)
for(i in user_names){
   timeline = get_timeline(i, n=3500)
   
   mets_tweets = rbind(mets_tweets, timeline)
   print(i)
}

#Filters out tweets not related to anything "Mets" or to any of its main officers
mets_tweets = mets_tweets %>%
   filter(str_detect(text, c('(mets)|(cohen)|(wilpon)|(sandy)|(brodie)|(wagenen)|(porter)|(rojas)|(rodriguez)|(arod)|(a-rod)|(alderson)|(Mets)|(Cohen)|(Wilpon)|(Sandy)|(Brodie)|(Wagenen)|(Porter)|(Rojas)|(Rodriguez)|(Arod)|(A-rod)|(Alderson)|(nym)|(NYM)'))) %>%
   mutate(month = month(created_at), #creates month and year variables
          year = year(created_at)) %>%
   #This gets rid of all columns that are lists, which would otherwise prevent this from being able to be saved as a CSV and are a bunch of
   #useless columns anyway
   select_if(Negate(is.list))

write.csv(mets_tweets, 'Mets_Tweets_2020_Data.csv', row.names = F)

#Get a count of each user's tweets after filtering. Looking for them all to still be contributing
mets_tweets %>%
   count(screen_name) %>%
   arrange(-n)

#Drills down from tweets down to individual words
mets_words = mets_tweets %>%
   #Converts to tokens. The token = 'tweets' gets rid of hyperlinks. Could change to 'ngrams' with n=2 to evaluate word pairs
   unnest_tokens(text, output = 'word', token = 'tweets', format='text', to_lower = T, collapse=F, drop = F) %>%
   #Gets rid of all stop words
   anti_join(stop_words2) %>%
   #Filters out retweets
   filter(is_retweet==F) %>%
   #This line converts any number strings to numbers to make them easy to get rid of (all characters will convert to NA)
   mutate(word.num = as.numeric(word)) %>%
   #now filters out numbers, usernames, hashtags, dollars, and "about" symbols
   filter(is.na(word.num)!=F, 
          substr(word, start = 0, 1) != '@',
          substr(word, start = 0, 1) != '#',
          substr(word, start = 0, 1) != '$',
          substr(word, start = 0, 1) != '~'
   ) %>%
   select(status_id, screen_name, created_at, month, year, word, favorite_count, retweet_count)
          







#####This section has a list of all relevant proper name ########################
names = c('Brodie Van Wagenen',  'Jared Porter', 'Luis Rojas', 'Carlos Beltran',
          'Jeff Wilpon', 'Fred Wilpon', 'Steve Cohen',  'Sandy Alderson', 'Wilpons', 'Cohens',
          'Alex Rodriguez', 'Jennifer Lopez', 'J-Lo', "A-Rod", "J-Rod",
          'George Springer', 'Trevor Bauer', 'JT Realmuto', 'DJ Lemahieu', 'Francisco Lindor', 'Nolan Arenado', 'James Mccann', 'Trevor May',
          'Jeff Mcneil', 'Brandon Nimmo', 'Michael Conforto', 'Mike Conforto', 'Pete Alonso', 'Dom Smith', 'Dominic Smith', 'Robinson Cano', 'canó',
          'andrés giménez', 'JD Davis', 'Jacob Degrom', 'Noah Syndergaard', 'Marcus Stroman', 'David Peterson', 'Edwin Diaz', 'Steven Matz',
          'Seth Lugo', 'Rick Porcello', 'Robert Gsellman', 'Michael Wacha', 'Yoenis Cespedes', 'Todd Frazier',  'Luis Guillorme', 'Amed Rosario', 
          'Wilson Ramos', 'Dellin Betances', 'Jeurys Familia', 'Billy Hamilton', 'Brad Brach', 'Franklyn Kilome', 'Drew Smith', 'Zach Wheeler', 'Jed Lowrie')

#This loops every name to try to get it into case when form. However, this proved to be a pain after the loop was done in terms 
#of fixing it up perfectly. So after the loop, I copied results into Excel and finished the line from there. 
#The case_when form is in the next chunk
all_text =c()
for(nm in 1:length(names)){
   split = strsplit(names[nm], " ")
   
   if(nm != length(names)){
      text = paste("name %in% c('", names[nm], "', '", split[[1]][1], "', '", split[[1]][2], "') ~ ",  substr(names[nm],0,1), split[[1]][2], ",", sep='' )
   } else{
      text = paste(
               paste("name %in% c('", names[nm], "', '", split[[1]][1], "', '", split[[1]][2], "') ~ ",  substr(names[nm],0,1), split[[1]][2], ",", sep=""),
               " TRUE ~ NA", sep="\n" )
   }
   
   all_text = rbind(all_text, text)
}
###########################################

mets_words =
   mets_words %>%
   #Trying to group together possible references to a single person, usually first or last name
      mutate(word2 = case_when(
         word %in% c('brodie van wagenen', 'brodie', 'wagenen', 'bvw') ~ 'bwagenen',
         word %in% c('jared porter', 'jared', 'porter') ~ 'jporter',
         word %in% c('luis rojas', 'luis', 'rojas') ~ 'lrojas',
         word %in% c('carlos beltran', 'carlos', 'beltran') ~ 'cbeltran',
         word %in% c('jeff wilpon', 'jeff', 'wilpon', 'fred', 'fred wilpon', 'wilpons') ~ 'fjwilpon',
         word %in% c('steve cohen', 'steve', 'cohen', 'cohens', 'alex cohen') ~ 'scohen',
         word %in% c('sandy alderson', 'sandy', 'alderson') ~ 'salderson',
         word %in% c('alex rodriguez', 'alex', 'rodriguez', 'jennifer lopez', 'jennifer', 'lopez', 'j-lo', 'a-rod', 'j-rod', 'arod', 'jlo', 'jrod') ~ 'arodriguez',
         word %in% c('george springer', 'george', 'springer') ~ 'gspringer',
         word %in% c('trevor bauer', 'trevor', 'bauer') ~ 'tbauer',
         word %in% c('jt realmuto', 'jt', 'realmuto', 'j.t.') ~ 'jrealmuto',
         word %in% c('dj lemahieu', 'd.j.', 'dj', 'lemahieu') ~ 'dlemahieu',
         word %in% c('francisco lindor', 'francisco', 'lindor') ~ 'flindor',
         word %in% c('nolan arenado', 'nolan', 'arenado') ~ 'narenado',
         word%in% c('james mccann', 'james', 'mccann') ~ 'jmccann',
         word%in% c('trevor may', 'trevor', 'may') ~ 'tmay',
         word%in% c('jeff mcneil', 'jeff', 'mcneil') ~ 'jmcneil',
         word%in% c('brandon nimmo', 'brandon', 'nimmo') ~ 'bnimmo',
         word%in% c('michael conforto', 'mike conforto', 'michael', 'conforto') ~ 'mconforto',
         word%in% c('pete alonso', 'pete', 'alonso') ~ 'palonso',
         word%in% c('dom smith', 'dom', 'dominic', 'dominic smith', 'smith') ~ 'dsmith',
         word%in% c('robinson cano', 'robinson', 'cano', 'canó') ~ 'rcano',
         word%in% c('andrés giménez', 'andrés', 'giménez') ~ 'agiménez',
         word%in% c('jd davis', 'jd', 'davis') ~ 'jdavis',
         word%in% c('jacob degrom', 'jacob', 'degrom') ~ 'jdegrom',
         word%in% c('noah syndergaard', 'noah', 'syndergaard') ~ 'nsyndergaard',
         word%in% c('marcus stroman', 'marcus', 'stroman') ~ 'mstroman',
         word%in% c('david peterson', 'david', 'peterson') ~ 'dpeterson',
         word%in% c('edwin diaz', 'edwin', 'diaz') ~ 'ediaz',
         word%in% c('steven matz', 'steven', 'matz') ~ 'smatz',
         word%in% c('seth lugo', 'seth', 'lugo') ~ 'slugo',
         word%in% c('rick porcello', 'rick', 'porcello') ~ 'rporcello',
         word%in% c('robert gsellman', 'robert', 'gsellman') ~ 'rgsellman',
         word%in% c('michael wacha', 'wacha') ~ 'mwacha',
         word%in% c('yoenis cespedes', 'yoenis', 'cespedes', 'céspedes') ~ 'ycespedes',
         word%in% c('todd frazier', 'todd', 'frazier') ~ 'tfrazier',
         word%in% c('luis guillorme', 'luis', 'guillorme') ~ 'lguillorme',
         word%in% c('amed rosario', 'amed', 'rosario') ~ 'arosario',
         word%in% c('wilson ramos', 'wilson', 'ramos') ~ 'wramos',
         word%in% c('dellin betances', 'dellin', 'betances') ~ 'dbetances',
         word%in% c('jeurys familia', 'jeurys', 'familia') ~ 'jfamilia',
         word%in% c('billy hamilton', 'hamilton') ~ 'bhamilton',
         word%in% c('brad brach', 'brad', 'brach') ~ 'bbrach',
         word%in% c('franklyn kilome', 'franklyn', 'kilome') ~ 'fkilome',
         word%in% c('drew smith', 'drew') ~ 'dsmith',
         word%in% c('zach wheeler', 'zach', 'wheeler') ~ 'zwheeler',
         word%in% c('jed lowrie', 'jed', 'lowrie') ~ 'jlowrie', 
                   TRUE ~ 'NA'
       ),
       is.name = case_when(word2 == 'NA' ~ 'word', TRUE ~ 'name'),   #this line makes a logical to delineate the proper names from regular words
       word3 = case_when(word2 == 'NA' ~ word, TRUE ~ word2) #this is the final check to bring in the combined names with the regular words to one column
      )
      
#This makes a regex approved string to lookup all possible combos of someone's name from the names list earlier
pat <-  str_c(gsub(" ", "\\b|\\b", str_c("\\b", tolower(names), "\\b"),
                   fixed = TRUE), collapse="|")

#This filters out all words from tweets that are proper names from the names list made earlier. Leaves only non-name words
mets_words_nonames = mets_words %>% 
   filter(str_detect(word, pat)==F) %>%
   #I guess I wanted to group ownership terms. There are probably others like this too but "ownership" is a hot topic
   mutate(word = case_when(word %in% c('owner', 'owners', 'ownership') ~ 'owner', TRUE ~ word)) %>%
   #converts this to counts of each word in the tweets
   count(word) %>%
   arrange(-n)

#Makes the bar plot of the top "no_name" words from the tweets
ggplot(mets_words_nonames %>% filter(n>175, word != 'mets') %>% mutate(word = reorder(word,n)), 
       aes(word, n, fill=word)) +
   geom_col() +
   coord_flip() +
   xlab('Count') +
   ylab('Word') +
   ggtitle('Mets Twitter Most Popular Words (no player names)') +
   theme(legend.position = "none") 

#Makes the bar plot of the top words incl names from the tweets
ggplot(mets_words %>% count(word3) %>% filter(n>200, word3 != c( 'mets', 'van')) %>% mutate(word3 = reorder(word3,n)), 
       aes(word3, n, fill=word3)) +
   geom_col() +
   coord_flip() +
   xlab('Count') +
   ylab('Word') +
   ggtitle('Mets Twitter Most Popular Words (incl player names)') +
   theme(legend.position = "none") 

#Word cloud of no_names
mets_words_nonames %>% 
   filter(word != 'mets') %>%
   with(wordcloud(words = word, freq = n,max.words = 70)) 

#Word cloud of all words
mets_words %>% 
   filter(word3 != 'mets', word3 != 'van') %>%
   count(word3) %>%
   with(wordcloud(words = word3, freq = n,max.words = 70))

#This gets the sentiment score by grouping the sentiment of all words from each tweet
mets_sentiments = sentiment_by(text.var = mets_words$word3, by = mets_words$status_id) %>%
   #Joins on info about the specific tweet
   full_join(mets_tweets %>% select(screen_name, status_id, text, month, year , favorite_count, retweet_count), 
             by=c('status_id' = 'status_id')) %>%
   #Filters only tweets since March 2020
   filter(year == 2020, month >= 3)

#Graphs the sentiment and positive tweet percentage by month   
mets_sentiments %>%
   #Gets rid of NAs and 0 sentiments, since they likely had no emotive language
   filter(is.na(ave_sentiment)==F, ave_sentiment != 0) %>%
   #Denotes if it is a positive sentiment tweet
   mutate(pos.sent = ifelse(ave_sentiment > 0, 1, 0)) %>%
   #Gets the average sentiment and percentage of positive tweets by month
   group_by(month) %>%
   summarize(avg.sentiment = mean(ave_sentiment), 
             n = n(), 
             pos.sent = mean(pos.sent)) %>%
   #Plots the avg sentiment by month
   ggplot(aes(month, avg.sentiment)) +
   geom_col() +
   #Fills in the columns with the percentage of positive tweets (scaled to the size of the column that month)
   geom_col(aes(month, pos.sent*avg.sentiment), 
            fill = 'green', alpha=.5) +
   #Labels the percentage of positive tweets
   geom_label(aes(month, pos.sent*avg.sentiment, 
                  label = paste(round(pos.sent*100,1),'%',sep=''))) +
   scale_x_continuous(breaks = seq(3,12,1)) +
   scale_y_continuous(breaks = seq(0,.18,.02)) +
   xlab('Month') +
   ylab('Average Sentiment') +
   ggtitle('Mets Twitter Sentiment & Percentage of Positive Tweets by Month') 


#This line makes the most/least positive words graph. You need to change 2 lines (filter is.name and arrange) to switch between the 2
mets_sentiments %>%
   #Joins sentiment tweet data down to the word level
   full_join(mets_words %>% select(status_id, is.name, word3), 
             by=c('status_id' = 'status_id')) %>%
   filter(year == 2020, month >= 3, 
          is.name!= 'name') %>%  ############ Use this line if you only want to use 'names' or 'words' only ################
   #Gets the average sentiment and count by word
   group_by(word3) %>%
   summarise(avg = mean(ave_sentiment), 
             n= n()) %>%
   #Only uses words with at least 20 occurrences
   filter(n > 20) %>%
   arrange(-avg) %>%  ######## Use this line to switch between the top and bottom rankings #################
   #Take the top n from the dataset to make the chart only show those words
   slice(1:10) %>%   
   #Reorders the word factor for the chart to show them in order of highest/lowest avg sentiment
   mutate(word3 = reorder(word3, avg)) %>%
   #Plots the average sentiment by the top or bottom words selected in the data steps before it
   ggplot(aes(word3, avg)) +
   geom_col(aes(fill=word3)) +
   coord_flip() +
   theme(legend.position = "none") +
   xlab("Word") +
   ylab('Average Sentiment') +
   ggtitle('Words with the Most Positive Sentiment on Mets Twitter')



#Dummy variable stuff for regression that I didn't get to yet
mets_words %>%
   mutate()
select(word2, favorite_count, retweet_count) %>%
   slice(1:250) %>%
   fastDummies::dummy_cols(remove_first_dummy = TRUE)

dummy.words = unique(mets_words %>% filter(is.name == 'name') %>% select(word = word3)) %>%
   bind_rows(mets_words_nonames %>% slice(2:301) %>% select(word)) %>%
   filter(word != 'céspedes', word!= 'nym')
