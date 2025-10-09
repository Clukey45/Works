path <- "C:/Users/chris/OneDrive/Desktop/youtube-top-100-songs-2025.csv"
tibble <- read.csv(path) %>% arrange(channel, desc(view_count))
tibble <- tibble %>% mutate(rank = rank(-view_count))
avgviewsperchannel <- tibble %>% group_by(channel) %>% summarise (avgviews = mean(view_count), count = n(), channel_follower_count = first(channel_follower_count))
joined <- inner_join(tibble,avgviewsperchannel, by = "channel")


scatter <- ggplot(avgviewsperchannel, aes(x = channel_follower_count, y = avgviews)) + geom_point()
linearmodel <- lm(avgviews ~ channel_follower_count, data = avgviewsperchannel)
summary(linearmodel)
