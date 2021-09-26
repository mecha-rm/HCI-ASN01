# Names: Adam Kahil, Eric Aivaliotis, Hao Tian Guan, and Roderick "R.J." Montague
# Date: 09/25/2021
# Description:
# References:
# - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rep

# INFR 4350U - Human-Computer Interactions for Games - Assignment 1

# 1.Grouped Bar Chart
# compare scores for Positive Experience, Negative Experience, Tiredness, and Returning to Reality.
set.seed(0)
q1a = floor(rnorm(10,2,1))
q1b = floor(rnorm(10,4,1))

q2a = floor(rnorm(10,3,1))
q2b = floor(rnorm(10,2,1))

q3a = floor(rnorm(10,2,1))
q3b = floor(rnorm(10,4,1))

q4a = floor(rnorm(10,0,1))
q4b = floor(rnorm(10,0,1))

q5a = floor(rnorm(10,2,1))
q5b = floor(rnorm(10,3,1))

q6a = floor(rnorm(10,2,1))
q6b = floor(rnorm(10,0,1))

q7a = floor(rnorm(10,3,1))
q7b = floor(rnorm(10,4,1))

q8a = floor(rnorm(10,3,1))
q8b = floor(rnorm(10,4,1))

q9a = floor(rnorm(10,1,1))
q9b = floor(rnorm(10,0,1))

q10a = floor(rnorm(10,2,1))
q10b = floor(rnorm(10,0,1))

q11a = floor(rnorm(10,2,1))
q11b = floor(rnorm(10,0,1))

q12a = floor(rnorm(10,3,1))
q12b = floor(rnorm(10,4,1))

q13a = floor(rnorm(10,2,1))
q13b = floor(rnorm(10,0,1))

q14a = floor(rnorm(10,1,1))
q14b = floor(rnorm(10,0,1))

q15a = floor(rnorm(10,0,1))
q15b = floor(rnorm(10,0,1))

q16a = floor(rnorm(10,2,1))
q16b = floor(rnorm(10,4,1))

q17a = floor(rnorm(10,3,1))
q17b = floor(rnorm(10,4,1))

my_data <- data.frame(player = gl(10, 1, 340),
                      Q = rep(c("01. I felt revived", 
                                "02. I felt bad",
                                "03. I found it hard to get back to reality",
                                "04. I felt guilty",
                                "05. It felt like a victory",
                                "06. I found it a waste of time",
                                "07. I felt energised",
                                "08. I felt satisfied",
                                "09. I felt disoriented",
                                "10. I felt exhausted",
                                "11. I felt that I could have done more useful things",
                                "12. I felt powerful",
                                "13. I felt weary",
                                "14. I felt regret",
                                "15. I felt ashamed",
                                "16. I felt proud",
                                "17. I had a sense that I had returned from a journey"
                      ),
                      each = 20),
                      
                      game = rep(c("Demon's Souls 2009", "Demon's Souls 2020"), each = 10),
                      rank = c(q1a,q1b,
                               q2a,q2b,
                               q3a,q3b,
                               q4a,q4b,
                               q5a,q5b,
                               q6a,q6b,
                               q7a,q7b,
                               q8a,q8b,
                               q9a,q9b,
                               q10a,q10b,
                               q11a,q11b,
                               q12a,q12b,
                               q13a,q13b,
                               q14a,q14b,
                               q15a,q15b,
                               q16a,q16b,
                               q17a,q17b		   
                      )
)
my_data$rank[my_data$rank > 4 ] <- 4
my_data$rank[my_data$rank < 0 ] <- 0    

#convert to wide

library(reshape) #for cast

#bring it to the wide format
wideData<-cast(my_data, player + game ~ Q, value = "rank")
wideData

#Scoring guidelines GEQ Post-game Module
#The post-game Module consists of four components; the items for each are listed below.
#Component scores are computed as the average value of its items.
#Positive Experience: Items 1, 5, 7, 8, 12, 16.
#Negative experience: Items 2, 4, 6, 11, 14, 15.
#Tiredness: Items 10, 13.
#Returning to Reality: Items 3, 9, and 17.

positive <- c(1,5,7,8,12,16)
positive <- positive + 2
wideData$pos_avg <- rowSums(wideData[,positive] / length(positive))

negative <- c(2,4,6,11,14,15)
negative <- negative + 2
wideData$neg_avg <- rowSums(wideData[,negative] /  length(negative))

tiredness <- c(10,13)
tiredness <- tiredness + 2
wideData$tiredness_avg <- rowSums(wideData[,tiredness] / length(tiredness))

reality <- c(3,9,17)
reality <- reality + 2
wideData$reality_avg <- rowSums(wideData[,reality] / length(reality))

summaries <- wideData[,c(1,2,20,21,22,23)]

aggregate(summaries$pos_avg, by=list(Category=summaries$game), FUN=mean)
aggregate(summaries$neg_avg, by=list(Category=summaries$game), FUN=mean)
aggregate(summaries$tiredness_avg, by=list(Category=summaries$game), FUN=mean)
aggregate(summaries$reality_avg, by=list(Category=summaries$game), FUN=mean)

# installing ggplot
if (!require(ggplot2)) install.packages(ggplot2)
if (!require(reshape2)) install.packages(reshape2)

library(ggplot2)
library(reshape2)

# QUESTION 1 - NEW CODE
summaries$pos_avg 
summaries$neg_avg
summaries$tiredness_avg
summaries$reality_avg

# TODO: fix
# charting
# the rep() function is being used instead of directly plugging in the values (e.g. 'positive' = rep(...) instead of 'positive' = positive)
# this is so that the data is copied instead of reused.

# question required to be in a data frame, so cbind was not used.
exp_all <- data.frame(
  'positive' = rep(x = positive, times = 1),
  'negative' = rep(x = negative, times = 1),
  'tiredness' = rep(x = tiredness, times = 1),
  'reality' = rep(x = reality, times = 1)
  )

bp_cols = c("red", "green", "blue", "cyan", "yellow")
barplot(as.matrix(exp_all), main = "Dark Souls Experience Chart", xlab = "Experience", ylab = "Amount", beside = TRUE, col = bp_cols)

####################################################
# QUESTION 2 - Plotting Questions
# Original Code (lines repeated from part 1 have been taken out)

#convert to wide
if (!require(likert)) install.packages(likert) # new line
library(likert)
# library(reshape) #for cast; already implemented.

#bring it to the wide format
# wideData<-cast(my_data, player + game ~ Q, value = "rank")
wideData

game = c("Demon's Souls 2009", "Demon's Souls 2020")

#Note: displays only the first 5 out of 17 questions
n_questions_display <- 5
n_questions_display <- n_questions_display + 2

if (!require(plyr)) install.packages("plyr")
wideData[3:n_questions_display] <- lapply(wideData[3:n_questions_display], factor, levels=0:4)
#create likert
likt <- likert::likert(wideData[,c(3:n_questions_display)], grouping = wideData$game)#, group.order = game)

#define better colors
cs <- c("#e9505a","#f6b9bd","gray88","#cfcfe8","#7474b0")
#define order in which the levels of the group variable will appear
order <- c("Demon's Souls 2009", "Demon's Souls 2020")
#plot
plot(likt,  plot.percents=TRUE, colors = cs, group.order = order)




###########
#Boxplot
###########

#Diverging stacked bar chart is the best way to display likert data
#We can also use a box plot, although it's less optimal
#Let's do it anyway as an exercise
#we will do it for the first question only

#make sure to run this again to clear the factor operations we performed above
wideData<-cast(my_data, player + game ~ Q, value = "rank")

#1. first isolate first three cols
subWideData <- wideData[,c(1,2,3)]
#rename last column to simply 'rank'
names(subWideData)[names(subWideData) == colnames(subWideData)[3]] <- 'rank'


if (!require(ggpubr)) install.packages("ggpubr")
library("ggpubr")
ggboxplot(subWideData, x = "game", y = "rank")

#why there is no 1st quartile?
#subWideData2020 = subWideData[game == "Demon's Souls 2020",]
#quantile(subWideData2020$rank)


###########
#Histogram
###########

library(ggplot2)        # for generating visualizations
ggplot(subWideData, aes(x = rank)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(game~.)

# The bins are not specified. To specify the bins, add "bins =" to the geom line above, and give it a number.
# If you don't, you will get a warning.
# geom_histogram(fill = "white", colour = "black", bins = int)


# Question 2 - New Code
# diverging stacked box chart

#libaries already set.

# widedata and game variables have already been set.
# recasts instead of copying since it already had its likert set up from before.
# dsbc_wideData = rep(x = wideData, times = 1)

dsbc_wideData<-cast(my_data, player + game ~ Q, value = "rank")

# copying segments (inclusive, and starts at 1) - check environment to see if copied successfully.
dsbc_wd_p1 = rep(x = wideData[1:8], times = 1) # Entries 1 - 6
dsbc_wd_p2 = rep(x = wideData[9:14], times = 1) # Entries 7 - 12
dsbc_wd_p3 = rep(x = wideData[15:19], times = 1) # Entries 13 - 17

# print if you want to check.

# games
dsbc_game = c("Demon's Souls 2009", "Demon's Souls 2020") # needed?

# these were moved above the rest since the other lines will be run multiple times.
# defining the colours using hexcodes
likdsbc_cols <- c("#ffc7c7","#cdffc7","#c7f8ff","#ffff99","#fce3ff")

#define order in which the levels of the group variable will appear
dsbc_order <- c("Demon's Souls 2009", "Demon's Souls 2020")

#questions to display
q_dis <- 6 # originally set to 5.
q_dis <- q_dis + 2 # makes space for other 2 (e.g. if set to 5, only 3 would show up without this)
# n_questions_display <- n_questions_display + 2 - change amount

# Questions 1 - 5
# player already installed.
dsbc_wideData[3:q_dis] <- lapply(dsbc_wideData[3:q_dis], factor, levels = 0:4)

#create new likert
likdsbc <- likert::likert(dsbc_wideData[,c(3:q_dis)], grouping = dsbc_wideData$game)

#plot
plot(likdsbc, plot.percents = TRUE, colors = likdsbc_cols, group.order = dsbc_order)

