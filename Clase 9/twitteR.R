library(twitteR)
library(dplyr)
library(readr)
consumer_key <- "kD8PbIF27SHDz4yoxu4YYkykV"
consumer_secret <- "oes5yH27am6MmsSGM12hezyz1YPfYhQE2O3Dgo2yVk1P1Rt5tc"
access_token <- "879189137173225472-JLUVWoE8Om8WiKa8qWmEzBotAGINC3h"
access_secret <- "AbCc9XYEe8w3gWS3k3dcEkQJH4ajZ9xUuVTGes39ovNGa"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tw <- twitteR::searchTwitter('#RiseofSkywalker', n = 2e4, since = '2019-10-20', retryOnRateLimit = 1e3)

df <- twitteR::twListToDF(tw)
df %>% head() %>% View()


write_csv(df,"tweetsRoS.csv")
