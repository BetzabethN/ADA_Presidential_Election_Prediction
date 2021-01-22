library(twitteR)
library(ROAuth)
library(base64enc)
library(xlsx)

# API Authorizations
consumerKey <- "-----"
consumerSecret <- "-----"
accessToken <- "------"
accessTokenSecret <- "-------"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)


#Get Tweets for Trump
pro_trump <- searchTwitter("#Trump2020", n=5000, lang="en")

anti_trump <- searchTwitter("#DumpTrump", n=5000, lang="en") 


#Get Tweets for Biden
pro_biden <- searchTwitter("#Biden2020", n=5000, lang="en")

anti_biden <- searchTwitter("#CorruptJoeBiden", n=5000, lang="en")


#Make Data Frame 
pro_trump_df <- do.call("rbind", lapply(pro_trump, as.data.frame))
anti_trump_df <- do.call("rbind", lapply(anti_trump, as.data.frame))

pro_biden_df <- do.call("rbind", lapply(pro_biden, as.data.frame))
anti_biden_df <- do.call("rbind", lapply(anti_biden, as.data.frame))


#Save Data
write.csv(pro_trump_df,file="pro_trump_tdata.csv")
write.csv(anti_trump_df,file="anti_trump_tdata.csv")

write.csv(pro_biden_df,file="pro_biden_tdata.csv")
write.csv(anti_biden_df,file="anti_biden_tdata.csv")