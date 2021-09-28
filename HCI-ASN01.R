# Names: Adam Kahil, Eric Aivaliotis, Hao Tian Guan, and Roderick "R.J." Montague
# Date: 09/26/2021
# Description: assignment 1 for human-computer interaction course.
# References:
# - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rep
# - https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
# - https://stackoverflow.com/questions/28592729/how-to-save-plots-inside-a-folder
# - https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5/topics/ggsave
# - https://www.rdocumentation.org/packages/ggplot2/versions/0.9.0/topics/ggsave
# - https://www.stat.berkeley.edu/~s133/saving.html
# - https://www.datamentor.io/r-programming/saving-plot/
# - https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/dev2
# - https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html
# - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/split
# - https://stackoverflow.com/questions/3302356/how-to-split-a-data-frame
# - https://stackoverflow.com/questions/5142842/export-a-graph-to-eps-file-with-r
# - https://statisticsglobe.com/export-plot-to-eps-file-in-r

# INFR 4350U - Human-Computer Interaction for Games - Assignment 1

# EXPORT SETTINGS #
# if 'true', the files are exported. If false, the contents are not exported.
auto_export <- FALSE

# setting the folder path. Not that it automatically does it relative to the working directory.
# as such, only the folder needs to be provided. The second example shows how you would the full path.

# folder
export_path <- "exports"

# for giving full path (unneeded since it adds the working directory by default)
# export_path <- paste(getwd(), sep = "", "/exports") # working directory + folder path

auto_export
export_path

# QUESTION #
ques <- "Question 1"
print(ques)

# QUESTION 1 - Grouped Bar Chart

### ORIGINAL CODE (ACTIVITY 1) ###

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


### QUESTION 1 - NEW CODE ###

# this is the data that must be put into a graph
# x is the values, Category is the labels
agg_pos <- aggregate(summaries$pos_avg, by=list(Category=summaries$game), FUN=mean)
agg_neg <- aggregate(summaries$neg_avg, by=list(Category=summaries$game), FUN=mean)
agg_tir <- aggregate(summaries$tiredness_avg, by=list(Category=summaries$game), FUN=mean)
agg_rea <- aggregate(summaries$reality_avg, by=list(Category=summaries$game), FUN=mean)

# TODO: fix
# charting
# the rep() function is being used instead of directly plugging in the values (e.g. 'positive' = rep(...) instead of 'positive' = positive)
# this is so that the data is copied instead of reused.

# question required to be in a data frame, so cbind was not used.

# all experience values
# exp_all <- data.frame(
#  'positive' = agg_pos,
#  'negative' = agg_neg,
#  'tiredness' = agg_tir,
#  'reality' = agg_rea
#)

# passed numbers only
exp_vals <- data.frame(
  'positive' = agg_pos$x,
  'negative' = agg_neg$x,
  'tiredness' = agg_tir$x,
  'reality' = agg_rea$x
)

exp_vals

# length of the y-axis (highest value rounded up, plus 1)
bp_ylim = c(0, ceiling(max(exp_vals)) + 1)

# uncomment themed ver. if using basic
bp_cols = c("red", "blue") # basic
bp_cols = c("#2854b5", "#e0ca19") # themed

# display bar plot
barplot(as.matrix(exp_vals), main = "Demon Souls Experience Chart", xlab = "Experience", ylab = "Average",
        legend.text = agg_pos$Category, beside = TRUE, col = bp_cols, ylim = bp_ylim)

# if the bar plot should be exported.
if(auto_export) {
  # both an absolute path and relative path works. This just shows the two ways of doing it.
  
  # png
  f = paste(getwd(), export_path, "hci-asn01_q1.png", sep = "/")
  dev.copy(png, f)
  dev.off()
  
  # eps (requires a different setup)
  setEPS()
  f = paste(export_path, "hci-asn01_q1.eps", sep = "/")
  postscript(f)
  barplot(as.matrix(exp_vals), main = "Demon Souls Experience Chart", xlab = "Experience", ylab = "Average",
          legend.text = agg_pos$Category, beside = TRUE, col = bp_cols, ylim = bp_ylim)
  dev.off()
  
}

####################################################

### QUESTION 2 - Plotting Questions ###
ques <- "Question 2"
print(ques)

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

# the wide data
dsbc_wideData<-cast(my_data, player + game ~ Q, value = "rank")

# print if you want to check.

# games
dsbc_game = c("Demon's Souls 2009", "Demon's Souls 2020") # needed?

# these were moved above the rest since the other lines will be run multiple times.
# defining the colours using hexcodes
likdsbc_cols <- c("#ffc7c7","#cdffc7","#c7f8ff","#ffff99","#fce3ff")

#define order in which the levels of the group variable will appear
dsbc_order <- c("Demon's Souls 2009", "Demon's Souls 2020")

# question variables
# new - controls start of list
q_start <- 3

#questions to display
# TODO: remove
# e.g. prints the first six (q_start = 3, q_count = 6)
q_count <- 6 # originally set to 5.
q_count <- q_count + 2 # makes space for other 2 (e.g. if set to 5, only 3 would show up without this)
# n_questions_display <- n_questions_display + 2 - change amount


# setup
# player already installed.

# creating new likert, and plotting

###
# 1 - 6 #
# just redid this for the  sake of consistency.
q_start <- 3
q_count <- 8

# applying data
dsbc_wideData[q_start:q_count] <- lapply(dsbc_wideData[q_start:q_count], factor, levels = 0:4)

# create new likert
likdsbc <- likert::likert(dsbc_wideData[,c(q_start:q_count)], grouping = dsbc_wideData$game)

# plot
plot(likdsbc, plot.percents = TRUE, colors = likdsbc_cols, group.order = dsbc_order)

# if files should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn01_q2-01-06.png", path = export_path)
  ggsave(filename = "hci-asn01_q2-01-06.eps", path = export_path)
  
  }


###
# 7 - 12 #
q_start <- 9
q_count <- 14

# reset data
dsbc_wideData<-cast(my_data, player + game ~ Q, value = "rank")

# applying data
dsbc_wideData[q_start:q_count] <- lapply(dsbc_wideData[q_start:q_count], factor, levels = 0:4)

# create new likert
likdsbc <- likert::likert(dsbc_wideData[,c(q_start:q_count)], grouping = dsbc_wideData$game)

# plot
plot(likdsbc, plot.percents = TRUE, colors = likdsbc_cols, group.order = dsbc_order)

# if files should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn01_q2-07-12.png", path = export_path)
  ggsave(filename = "hci-asn01_q2-07-12.eps", path = export_path)
}


###
# 13 - 17 #
q_start <- 15
q_count <- 19

# reset data
dsbc_wideData<-cast(my_data, player + game ~ Q, value = "rank")

# applying data
dsbc_wideData[q_start:q_count] <- lapply(dsbc_wideData[q_start:q_count], factor, levels = 0:4)

# create new likert
likdsbc <- likert::likert(dsbc_wideData[,c(q_start:q_count)], grouping = dsbc_wideData$game)

# plot
plot(likdsbc, plot.percents = TRUE, colors = likdsbc_cols, group.order = dsbc_order)

# if files should be automatically exported.
if(auto_export) {
  ggsave(filename = "hci-asn01_q2-13-17.png", path = export_path)
  ggsave(filename = "hci-asn01_q2-13-17.eps", path = export_path)
}

###
# 1 - 17 (ALL)
# this is too big to see everything, so maybe take it out.
q_start <- 3
q_count <- 19

# reset data
dsbc_wideData<-cast(my_data, player + game ~ Q, value = "rank")

# applying data
dsbc_wideData[q_start:q_count] <- lapply(dsbc_wideData[q_start:q_count], factor, levels = 0:4)

# create new likert
likdsbc <- likert::likert(dsbc_wideData[,c(q_start:q_count)], grouping = dsbc_wideData$game)

# plot
plot(likdsbc, plot.percents = TRUE, colors = likdsbc_cols, group.order = dsbc_order)

# if files should be automatically exported.
if(auto_export) {
  # scale is used to zoom out the graph, as it's too big by default otherwise. It also changes the final image size.
  ggsave(filename = "hci-asn01_q2-all.png", path = export_path,  width = 544, height = 1032, units = "px", scale = 3.8)
  ggsave(filename = "hci-asn01_q2-all.eps", path = export_path, width = 544, height = 1032, units = "px", scale = 3.8)
}

###########################################
# QUESTION 3 - Grouped Bar Chart
# you can use read to read the data directly.
ques <- "Question 3"
print(ques)

# import data from file
re7_import <- read.csv("imports/RE7.csv")
re7_import


# Step 1 - Box Plots

# highest heart rate
hhr <- max(re7_import$avgHeartRate)
hhr <- hhr + 100

 
# both versions work.
# going to check and see if ggplot has to be used, or if this one is fine.
# formula = y ~ grp (valus ~ grouping)
# ver 1
boxplot(formula = avgHeartRate ~ ï..game, data = re7_import, main = "Resident Evil 7 Heart Rates", 
        xlab = "Version", ylab = "Heart Rate", ylim = c(0, hhr))

# ver 2
if (!require(ggpubr)) install.packages("ggpubr")
library("ggpubr")
ggboxplot(re7_import, x = 'ï..game', y = 'avgHeartRate', main = "Resident Evil 7 Heart Rates", 
          xlab = "Version", ylab = "Heart Rate", ylim = c(0, hhr))

# Step 2 - Outliers (IQR Method)

# splits the data into the television and vr data
re7_split<-split(re7_import, re7_import$ï..game)
re7_tv<-re7_split$RE7_TV
re7_vr<-re7_split$RE7_VR

# mean
re7_avg = mean(re7_import$avgHeartRate)

# IQRs (Q3 - Q1)
re7_iqr<-IQR(re7_import$avgHeartRate)
re7_tv_iqr<-IQR(re7_tv$avgHeartRate)
re7_vr_iqr<-IQR(re7_vr$avgHeartRate)

# quantiles
re7_q <- quantile(re7_import$avgHeartRate)
re7_q

# lower and upper bounds/q1 and q3 bounds to find outliers (Q1 - 1.5 * IQR > x < Q3 + 1.5 * IQR )
re7_lb<-re7_q[2] - 1.5 * re7_iqr # 25%
re7_ub<-re7_q[4] + 1.5 * re7_iqr # 75%

# Step 3 - Updated Box Plots

# re7 data no outliers (re7_dno)
# range: [Q1 - 1.5 * IQR, Q3 + 1.5 * IQR]
re7_dno = rep(re7_import, times = 1) # not needed.
re7_dno<-subset(re7_import, (re7_import$avgHeartRate > re7_lb) & (re7_import$avgHeartRate < re7_ub))

ggboxplot(re7_dno, x = 'ï..game', y = 'avgHeartRate', main = "Resident Evil 7 Heart Rates", 
          xlab = "Version", ylab = "Heart Rate", ylim = c(0, hhr))

# Step 4 - Bar Chart for Means
# mean with outliers
re7_tv_avg <- mean(re7_split$RE7_TV$avgHeartRate)
re7_vr_avg <- mean(re7_split$RE7_VR$avgHeartRate)

# mean without outliers
re7_dno_split<-split(re7_dno, re7_dno$ï..game)
re7_tv_dno_avg <- mean(re7_dno_split$RE7_TV$avgHeartRate)
re7_vr_dno_avg <- mean(re7_dno_split$RE7_VR$avgHeartRate)

# averages (with outliers)
re7_avg_df <- data.frame(
  'RE7_TV' = rep(re7_tv_avg, times = 1),
  'RE7_VR' = rep(re7_vr_avg, times = 1)
)
re7_avg_df

# averages (without outliers)
re7_dno_avg_df <- data.frame(
  'RE7_TV' = rep(re7_tv_dno_avg, times = 1),
  'RE7_VR' = rep(re7_vr_dno_avg, times = 1)
)
re7_dno_avg_df

# standard deviations
re7_tv_sd <- sd(re7_split$RE7_TV$avgHeartRate)
re7_vr_sd <- sd(re7_split$RE7_VR$avgHeartRate)
re7_tv_dno_sd <- sd(re7_dno_split$RE7_TV$avgHeartRate)
re7_vr_dno_sd <- sd(re7_dno_split$RE7_VR$avgHeartRate)

# bar chart values
re7_bp_cols = c("red", "blue")
re7_ylim = ceiling(max(re7_avg_df, re7_dno_avg_df)) + 50

# sd colors (line, text)
re7_sd_cols = c("light green", "green")

# sd text locx (single bars only)
re7_sd_tls = c(1.5, 3.5)

# sd text size (big)
re7_sd_tsb = 1.09

# sd text size (small)
re7_sd_tss = 0.8

# with outliers
barplot(as.matrix(re7_avg_df), main = "Resident Evil 7 Averages (With Outliers)", xlab = "Version", ylab = "Average", 
        beside = TRUE, col = re7_bp_cols[1], ylim = c(0, re7_ylim))

# RE7_TV
# sd line
abline(h = re7_tv_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = re7_sd_tls[1],
     y = re7_tv_sd * 1.1,
     paste("TV_SD = ~", round(re7_tv_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tsb)

# RE7_VR
# sd line
abline(h = re7_vr_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = re7_sd_tls[2],
     y = re7_vr_sd * 1.14,
     paste("VR_SD = ~", round(re7_vr_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tsb)

##

# without outliers
barplot(as.matrix(re7_dno_avg_df), main = "Resident Evil 7 Averages (No Outliers)", xlab = "Version", ylab = "Average", 
        beside = TRUE, col = re7_bp_cols[2], ylim = c(0, re7_ylim))

# RE7_TV
# sd line
abline(h = re7_tv_dno_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = re7_sd_tls[1],
     y = re7_tv_dno_sd * 1.2,
     paste("TV_SD = ~", round(re7_tv_dno_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tsb)

# RE7_VR
# sd line
abline(h = re7_vr_dno_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = re7_sd_tls[2],
     y = re7_vr_dno_sd * 1.35,
     paste("VR_SD = ~", round(re7_vr_dno_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tsb)

##

# both
re7_avg_comp_df <- data.frame(
  'RE7_TV' = c(re7_tv_avg, re7_tv_dno_avg),
  'RE7_VR' = c(re7_vr_avg, re7_vr_dno_avg)
)

barplot(as.matrix(re7_avg_comp_df), main = "Resident Evil 7 Averages", xlab = "Version", ylab = "Average", 
        beside = TRUE, legend.text = c('With Outliers', 'No Outliers'), col = re7_bp_cols, ylim = c(0, re7_ylim + 50))

# OUTLIERS
# RE7_TV
# sd line
abline(h = re7_tv_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = 1.5,
     y = re7_tv_sd * 1.1,
     paste("TV_SD = ~", round(re7_tv_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tss)

# RE7_VR
# sd line
abline(h = re7_vr_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = 3.5,
     y = re7_vr_sd * 1.14,
     paste("VR_SD = ~", round(re7_vr_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tss)

##

# NO OUTLIERS
# RE7_TV_DNO
# sd line
abline(h = re7_tv_dno_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = 1.5,
     y = re7_tv_dno_sd * 1.2,
     paste("TV_SD = ~", round(re7_tv_dno_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tss)

# RE7_VR_DNO
# sd line
abline(h = re7_vr_dno_sd, col = re7_sd_cols[1], lwd = 1)

# sd text
text(x = 3.5,
     y = re7_vr_dno_sd * 1.35,
     paste("VR_SD = ~", round(re7_vr_dno_sd, 3)),
     col = re7_sd_cols[2],
     cex = re7_sd_tss)

