#Michael Dennis Jack Kroll
#Group 6
#Statistical Analysis On Typing Speed and Accuracy Across Different Passage Types and Keyboard Types


library("ggplot2")
library("lme4")
library("moments")
library("ggeffects")
typing<-read.csv("typing.csv", stringsAsFactors = TRUE)


#Part 1 Basic Information from data
View(typing)

#User 1 Total Average WPM
user1_data<- subset(typing, Subject == 'A')
user2_data<- subset(typing, Subject == 'B')


#Combined WPM
mean(typing$WPM) #72.85556
median(typing$WPM) #73
sd(typing$WPM) #8.524
moments::skewness(typing$WPM)#-0.332
quantile(typing$WPM, probs = c(0.25,0.75))

#Combined Score
mean(typing$Accuracy) #94..4444
median(typing$Accuracy) #95
sd(typing$Accuracy) #2.022
moments::skewness(typing$Accuracy)#-0.4388
quantile(typing$Accuracy, probs = c(0.25,0.75)) #93-96


#Histogram of the data
hist(typing$WPM, col = 'red', main = "Frequency of WPM",
     xlab = "Words Per Minute")

#For User 1
mean(user1_data$WPM) #72.022
median(user1_data$WPM) #72
sd(user1_data$WPM) #5.82
moments::skewness(user1_data$WPM)#0.76
quantile(user1_data$WPM, probs = c(0.25,0.75)) #69-75

mean(user1_data$Accuracy) #94.11
median(user1_data$Accuracy) #94
sd(user1_data$Accuracy) #1.92
moments::skewness(user1_data$Accuracy)#-0.80
quantile(user1_data$Accuracy, probs = c(0.25,0.75)) #93-95



#For User 2
mean(user2_data$WPM) #73.69
median(user2_data$WPM) #78
sd(user2_data$WPM) #10.57
moments::skewness(user2_data$WPM)#-0.65
quantile(user2_data$WPM, probs = c(0.25,0.75)) #66-81

mean(user2_data$Accuracy) #94.78
median(user2_data$Accuracy) #95
sd(user2_data$Accuracy) #2.09
moments::skewness(user2_data$Accuracy)#-0.26
quantile(user2_data$Accuracy, probs = c(0.25,0.75)) #93-96



#2 Comparing the 2 users
#Is there a meaningful difference in typing speed?

#Based on the mean not really (they are about 1 apart well within std)
#Not used in project but was good to look at
boxplot(WPM ~ Subject, data = typing, main = "WPM in typing between two typers",
        xlab = "Typer",
        ylab = "Words Per Minute")




#This is a much more significant difference
#Yet neither of the data is particularly normal
qqnorm(user1_data$WPM, main = "Normal Q-Q Plot of Subject A WPM")
qqline(user1_data$WPM)

qqnorm(user2_data$WPM, main = "Normal Q-Q Plot of Subject B WPM")
qqline(user2_data$WPM)

#This confirms that data is not normal
#But it not horribly off and 
#with the number of samples it is okay
#to proceed
nrow(user1_data) #45
nrow(user2_data) #45


#H0 No difference between mean typing speed between users
#HA There is a difference in mean typing speed between users
t.test(WPM ~ Subject, typing, conf.level = 0.9)

#With a p value of 0.3573 the null is not rejected in favor of the alternative


#There is a problem, the distributions
#are pretty different so it is difficult
#to not violate assumptions
#Googling online I found the 
#KS test which will
#compare distribtuions and if
#they differ

#Assumptions
#Independent Yes
#Continous Sorta - but robust where there are a lot of "bins"
#Representative

#So We can run this!
#H0 the distributions in WPM are not different
#H1 the distribtuions in WPM are different between the two users
#Sig level 0.9
ks.test(user1_data$WPM, user2_data$WPM)

#p=val - 0.0007917
#This is a very signifcant result
#so the alternative in favored compared to the null




#What about Accuracy?
boxplot(Accuracy ~ Subject, data = typing, main = "Accuracy in Typing Between Two Typist",
        xlab = "Typist",
        ylab = "AccuracyRate (%)",
        col =c(5,3))

#There seems to be a potentially larger
#difference specially due to the smaller std

#Is the data normal?
qqnorm(user1_data$Accuracy, main = "Normal Q-Q Plot of Subject A Typing Accuracy")
qqline(user1_data$Accuracy)

qqnorm(user2_data$Accuracy, main = "Normal Q-Q Plot of Subject B Typing Accuracy")
qqline(user2_data$Accuracy)

#There is a bit of a skew for user 1
#It looks pretty normal for user 2
#Due to sample size though Welch's
#test is robust and can be done
#At 90%
#H0 There is no difference in accuracy between subjects
#HA There is a different in accuracy between subjects
t.test(Accuracy ~ Subject, typing, conf.level = 0.9)

#P-val of 0.1186 border line but H0 is not rejected
#In favor of the alternative

#Part 2 correlation
#There are 3 numerical values
#WPM, Accuracy, and NonAZ Char
#How do they correlate?

cor(typing[, c("WPM", "Accuracy", "NonAZChar")], use = 'complete.obs')
pairs(typing[, c("WPM", "Accuracy", "NonAZChar")])



#There is actually a correlation t test I found online
#Using spearmen as it better fits the data

#Relationship should be either positive or negative
#And data is independent
#This is true for both

cor.test(typing$WPM, typing$Accuracy, method= "spearman")

#H0 there is no relationship between Accuracy and WPM
#HA there is a relationship between Accuracy and WPM
#Got p = 1.921e-07
#This is extremely significant and makes sense
#The typing test does not include inaccuracies
#as part of your WPM. So if a significant amount of
#what somebody typed was inaccurate, it would not be included
#in the final WPM calculation making it lower

#What about NonAZChar and WPM at 10%
#H0 There is no relationship between typing speed and what percent of characters was NonAZ
#HA There is a relationship between typing speed and what percent of characters was NonAZ
cor.test(typing$WPM, typing$NonAZChar, method= "spearman")

#p-val is 0.03613
#This is also significant, HA is accepted in favor of the null
#This also makes sense as participants may not be as accustomed to typing
#keys that are not A-Z



#To be thorough lets do NonAZChar and Accuracy
#H0 Is that there is no relationship between Accuracy and NonAZChar
#And HA is that there is a relationship
cor.test(typing$Accuracy, typing$NonAZChar, method= "spearman")

#p-val of 0.6415 not significant and H0 is not rejected
#in favor of the alternative


#While the mean typing speed is not significantly different, the participants
#do type differently. Yet it would be good to consider what impact the other
#factors have (WPM, Accuracy, Passage, and Keyboard)

library(lmerTest)

modelWPM <- lmer(
  WPM ~ Passage * Keyboard + (1 | Subject),
  data = typing
)


#The STD between the subjects 9s 0.4993 while residual std is 7.1610 
#This confirms that the users difference from eachother is less
#than the differences between the users in general

library(ggplot2)


#Used Gen AI to help generate this chart although I edited it by changing
#theming and geom + sizes to look better along with axeds to fit better
ggplot(typing, aes(x = Keyboard, y = WPM, color = Passage, group = Passage)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "Interaction: Passage × Keyboard on WPM",
       y = "Mean WPM",
       x = "Keyboard Typed")


#This chart shows the WPM between the passage type and keyboard.
#It seems based on this chart that all passages respond similarly to
#the different type of keyboard, in fact the ordering is the same for all of them
#(Kids always fastest, then news, then old English).

#Also it seems pretty apparent that typing seems to be slower for mobile keyboards
#and although low profile seems to be a bit faster than clunky, the difference
#is much less significant (and reversed in the case of OldEnglish passages)


#Note Gen AI was used to develop
#table in paper from this data
#all numbers were verified for accuracy
summary(modelWPM)


#With a significance of 0.1 only one of the effects is statistically significant
#and that is that Mobile keyboards are slower than the other keyboards
#p_val = 0.0253. The only other that potentially raises eyebrows is 
#keyboard low profile being faster than the other keyboards with a p value of 0.1224

#Based on this there seems to be a significantly significant difference on the keyboard
#Typed with especially with mobile keyboard. Passage type did not have a significantly different difference
#between the two groups
library(emmeans)


#When searching online I found emmeans
#which will compare Keyboard types
#from the model 
emmeans(modelWPM, pairwise ~ Keyboard)

#This confirms what was found earlier
#p_val is very low between Mobile and the other two keyboard
#types which confirms what was found earlier and low profile and clunky
#are found to not have a significant difference between them

emmeans(modelWPM, pairwise ~ Passage)
# At a significance level of 10%, there is nothing deemed "significant",
# however the difference observed from a kids passage to an Old English one
# is 0.06, which could warrant further investigation



#Now lets do the same for Accuracy
modelAcc <- lmer(
  Accuracy ~ Passage * Keyboard + (1 | Subject),
  data = typing
)




# Nothing particularly notable, accuracy does not seem to have a significant difference 
# in either category

ggplot(typing, aes(x = Keyboard, y = Accuracy, color = Passage, group = Passage)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "Interaction: Passage × Keyboard on Accuracy",
       y = "Mean Accuracy",
       x = "Keyboard Typed With")

#At a quick glance it is very difficult to notice any difference in accuracy
#between either keyboard type or passage so expectation is no significant results
summary(modelAcc)

emmeans(modelAcc, pairwise ~ Keyboard)
emmeans(modelAcc, pairwise ~ Passage)


#Interestingly with a significance level of a = 0.10 there seems to be two
#significant findings

#This is that Both news and Old English struggle more compared to kids articles
#with a mobile keyboard (p = 0.0296 and 0.0435 respectively)
#theory is potentially this is due to the count of non-az characters
#which may be difficult to type due
#to being on a different page

#Lets do a bar plot to do that
passage_split <- split(typing$NonAZChar, typing$Passage)


barplot(sapply(passage_split, mean), col = c(1,2,3), main = "Barplot of Article Type vs Percent not AZ Character",
  xlab = "Article Type", ylab = "Percentage non A-Z")


#Looking at this this DOES NOT seem to be the case. Actually kids articles have the highest
#percentage non-AZ (mostly likely due to quotation marks). So this does not explain the lower error
#rate


#Another theory is that it is potentially due to having more difficult and long spellings
#and while that can be assumed true (kids books are graded due to the difficult
#spellings)

modelFull <- lmer(
  WPM ~ Passage * Keyboard + NonAZChar + (1 | Subject),
  data = typing
)

summary(modelFull)

# Model indicates that a higher NonAZCharacter count does have an notable impact, but 
# it isn't incredibly impactful compared to other factors. 
ggDF<-ggpredict(modelFull, terms = "NonAZChar")
ggpredict(modelFull, terms = c("NonAZChar", "Keyboard")) |>  plot() + labs(
  title="Effect of Non-AZ Characters on WPM",
  x = "Percent of passage Non-A-Z characters"
)
