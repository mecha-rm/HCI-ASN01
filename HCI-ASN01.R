# Names: Adam Kahil, Eric Aivaliotis, Hao Tian Guan, and Roderick "R.J." Montague
# Description:
# References:

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

barplot(sort(table(positive), beside = T), xlab = "H", ylab = "V")
barplot(sort(table(negative), beside = T), xlab = "H", ylab = "V")
barplot(sort(table(tiredness), beside = T), xlab = "H", ylab = "V")
barplot(sort(table(reality), beside = T), xlab = "H", ylab = "V")


