#Scipt for the behavioral data analysis for DRES paper
#Last Run Date: July 23 2019 
#File has been adapted to use deidentified data. 
##This script will reproduce plots and results 

#Import Libraries
library(readxl)
library(ggplot2)
library(psych)

#Import data file
data1 <- read.csv("/Users/jennygilbert/Documents/bevel_DRES_analysis/data/deid_bevel.csv")
names(data1)
#crop off extra empty rows (not sure why this happens, this is my brute fix)
df <- data1[-c(91,92), ]

#Make DRES Categories
## What are the tertiles? 
quantile(df$DRES, c(.33, .66)) 

## Split data into catergories based on the tertiles 
attach(df)
df$DREScat[DRES > 2.5] <- "high_restraint"
df$DREScat[DRES > 1.837 & DRES <= 2.5] <- "mid_restraint"
df$DREScat[DRES <= 1.837] <- "low_restraint"
detach(df)
print(df$DREScat)

## Select top tertile = high restraint; bottom tertile = low restraint
df_new <- df[ which(df$DREScat=='high_restraint' | df$DREScat =="low_restraint"), ]

#Generate Table 1: Sample Statistics 


#Test for significant differences in sample statistics between groups
t.test(BMI ~ DREScat, data = df_new)
t.test(age ~ DREScat, data = df_new)
chisq.test(df_new$DREScat, df_new$sex, correct=FALSE)
t.test(DRES ~ DREScat, data = df_new)
t.test(emotional_eating ~ DREScat, data = df_new)
t.test(external_eating ~ DREScat, data = df_new)
t.test(bis ~ DREScat, data = df_new)
t.test(bas ~ DREScat, data = df_new)
t.test(TOTAL_KCAL ~ DREScat, data = df_new)
t.test(PORTIONS_SSB ~ DREScat, data = df_new)
t.test(nback_accuracy ~ DREScat, data = df_new)
chisq.test(df_new$DREScat, df_new$sweetstim_level, correct=FALSE)
t.test(sweetstim_pleasent ~ DREScat, data = df_new)
chisq.test(df_new$DREScat, df_new$bitterstim_level, correct=FALSE)
t.test(bitterstim_pleasent ~ DREScat, data = df_new)
t.test(sensitivity_reward ~ DREScat, data = df_new)
t.test(sensitivity_punish ~ DREScat, data = df_new)
chisq.test(df_new$DREScat, df_new$test_result_group, correct=FALSE)




# Plot Differences between groups
DRES_Palette <- c("#f6daff", "#c5f0ff")
p <- ggplot(df_new, aes(x=reorder(DREScat,BMI, FUN = median), y=BMI, fill=DREScat)) 
p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +xlab("Dietary Restraint Groups") +
  ylab("Body Mass Index") + theme_bw(base_size = 14) + scale_fill_discrete(name="Dietary Restraint Groups",
                                                                           breaks=c("high_restraint", "low_restraint"),
                                                                           labels=c("High Dietary Restraint", "Low Dietary Restraint"))


tbl_learner <- with(df_new, table(test_result_group, DREScat))
ggplot(as.data.frame(tbl_learner), aes(factor(test_result_group), Freq, fill = DREScat)) + scale_x_discrete(limits=c("learner", "reward_learner","punish_learner", "nonlearner"), labels=c("choose reward and avoid punishment", "choose reward","avoid punishment","no learning")) +
  geom_col(position = "dodge", colour="black") + theme_minimal() + scale_fill_manual(values=c(DRES_Palette), name = "Dietary Restraint Groups", labels = c("Low", "High")) +
  labs(y = "Number of Participants", x = "PST Posttest Performance") + ylim(0,13)



################################################DIRTY CODE TO FIX################################################

###### Continuous Analysis #######
reg1 <- lm(sensitivity_reward ~ DRES + BMI, data=df)
summary(reg1)

reg2 <- lm(sensitivity_punish ~ DRES + BMI, data=df)
summary(reg2)

ggplot(data=df, aes(df$DRES)) + geom_histogram(binwidth = 0.25, col="black", fill=I("grey"), alpha = .7) + theme_minimal() + labs(x="DRES Score", y="Count")

colors <- c(rep("#c5f0ff",4), rep("grey",3), rep("#f6daff",8))
ggplot(data=df, aes(df$DRES)) + geom_histogram(binwidth = 0.25, col="black", fill=(colors), alpha = .7) + theme_minimal() + labs(x="DRES Score", y="Count")

#test for group differences
describeBy(df_new$BMI, df_new$DREScat)
t.test(BMI ~ DREScat, data = df_new)

describeBy(df_new$age, df_new$DREScat)
t.test(age ~ DREScat, data = df_new)

table(df_new$DREScat, df_new$sex)
chisq.test(df_new$DREScat, df_new$sex, correct=FALSE)

describeBy(df_new$DRES, df_new$DREScat)
t.test(DRES ~ DREScat, data = df_new)

describeBy(df_new$external_eating, df_new$DREScat)
t.test(external_eating ~ DREScat, data = df_new)

describeBy(df_new$emotional_eating, df_new$DREScat)
t.test(emotional_eating ~ DREScat, data = df_new)

describeBy(df_new$bis, df_new$DREScat)
t.test(bis ~ DREScat, data = df_new)

describeBy(df_new$bas, df_new$DREScat)
t.test(bas ~ DREScat, data = df_new)

describeBy(df_new$bas_drive, df_new$DREScat)
t.test(bas_drive ~ DREScat, data = df_new)

describeBy(df_new$TOTAL_KCAL, df_new$DREScat)
t.test(TOTAL_KCAL ~ DREScat, data = df_new)

describeBy(df_new$PORTIONS_SSB, df_new$DREScat)
t.test(PORTIONS_SSB ~ DREScat, data = df_new)

describeBy(df_new$nback_accuracy, df_new$DREScat)
t.test(nback_accuracy ~ DREScat, data = df_new)

t<- table(df_new$DREScat, df_new$sweetstim_level)
t
chisq.test(df_new$DREScat, df_new$sweetstim_level, correct=FALSE)

describeBy(df_new$sweetstim_pleasent, df_new$DREScat)
t.test(sweetstim_pleasent ~ DREScat, data = df_new)

describeBy(df_new$bitterstim_pleasent, df_new$DREScat)
t.test(bitterstim_pleasent ~ DREScat, data = df_new)

t<-table(df_new$DREScat, df_new$bitterstim_level)
prop.table(t)
chisq.test(df_new$DREScat, df_new$bitterstim_level, correct=FALSE)

describeBy(df_new$sweetstim_pleasent, df_new$DREScat)
t.test(sweetstim_pleasent ~ DREScat, data = df_new)

describeBy(df_new$sweetstim_desire, df_new$DREScat)
t.test(sweetstim_desire ~ DREScat, data = df_new)

t<- table(df_new$DREScat, df_new$bitterstim_level)
prop.table(t)
chisq.test(df_new$DREScat, df_new$bitterstim_level, correct=FALSE)

describeBy(df_new$scan_._correct, df_new$DREScat)
t.test(scan_._correct ~ DREScat, data = df_new)

describeBy(df_new$sensitivity_punish, df_new$DREScat)
t.test(sensitivity_punish ~ DREScat, data = df_new)

describeBy(df_new$sensitivity_reward, df_new$DREScat)
t.test(sensitivity_reward ~ DREScat, data = df_new)

t<- table(df_new$DREScat, df_new$test_result_group)
prop.table(t)
chisq.test(df_new$DREScat, df_new$test_result_group, correct=FALSE)


#PLOTS
p <- ggplot(df_new, aes(x=reorder(DREScat,BMI, FUN = median), y=BMI, fill=DREScat)) 
p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +xlab("Dietary Restraint Groups") +
  ylab("Body Mass Index") + theme_bw(base_size = 14) + scale_fill_discrete(name="Dietary Restraint Groups",
                                                                           breaks=c("high_restraint", "low_restraint"),
                                                                           labels=c("High Dietary Restraint", "Low Dietary Restraint"))

p <- ggplot(df_new, aes(x=reorder(DREScat,DRES, FUN = median), y=DRES, fill=DREScat)) 
p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +xlab("Dietary Restraint Groups") +
  ylab("DRES score") + theme_bw(base_size = 14)

p <- ggplot(df_new, aes(x=reorder(DREScat,nback_accuracy, FUN = median, desc = FALSE), y=nback_accuracy, fill=DREScat)) 
p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +xlab("Dietary Restraint Groups") +
  ylab("N-Back Accuracy") + theme_bw(base_size = 14)

#Bar plots
tbl <- with(df_new, table(DREScat, test_result_group))
barplot(tbl, beside = TRUE, legend = TRUE)

tbl <- with(df_new, table(DREScat, sweetstim_level))
barplot(tbl, beside = TRUE, legend = FALSE)

tbl <- with(df_new, table(DREScat, bitterstim_level))
barplot(tbl, beside = TRUE, legend = FALSE)

df_new$RT_avg <- (df_new$scan_RT_ab + df_new$scan_RT_cd + df_new$scan_RT_ef)/3
describeBy(df_new$df_new$RT_avg, df_new$DREScat)
t.test(df_new$RT_avg ~ DREScat, data = df_new)

#Controlling for BMI
reg1 <- lm(emotional_eating ~ DREScat + BMI, data=df_new)
summary(reg1)

reg2 <- lm(bas_drive ~ DREScat + BMI, data=df_new)
summary(reg2)

reg3 <- lm(TOTAL_KCAL ~ DREScat + BMI, data=df_new)
summary(reg3)

reg4 <- lm(PORTIONS_SSB ~ DREScat + BMI, data=df_new)
summary(reg4)

reg5 <- lm(nback_accuracy ~ DREScat + BMI, data=df_new)
summary(reg5)

write.table(df_new, "~/Desktop/mydata.txt", sep="\t")


########################## FOR SSIB ########################################

aov_cont<- aov(df_new$BMI ~ df_new$test_result_group)
summary(aov_cont)

################## EXPLORING SENSITIVITY TO REWARD AND PUNISHMENT #########################
#MANOVA
res.man <- manova(cbind(sensitivity_punish, sensitivity_reward) ~ DRES, data = df)
summary(res.man)
summary.aov(res.man)

#MANCOVA (controling for BMI)
Y <- cbind(df$sensitivity_punish, df$sensitivity_reward)
fit <- manova(Y ~ df$DRES + df$BMI)
summary.aov(fit)

Z <- cbind(df_new$sweetstim_level, df_new$bitterstim_level)
fit <- manova(Z ~ df_new$DREScat + df_new$BMI)
summary.aov(fit)

L <- cbind(df_new$sensitivity_punish, df_new$sensitivity_reward)
fit <- manova(L ~ df_new$DREScat + df_new$nback_accuracy)
summary.aov(fit)

t <- table(df_new$DREScat, df_new$race1)
chisq.test(df_new$DREScat, df_new$race1, correct=FALSE)

t <- table(df_new$DREScat, df_new$hispanic)
chisq.test(df_new$DREScat, df_new$hispanic, correct=FALSE)

#DENSITY PLOTS 
cbPalette <- c("orchid1", "deepskyblue")

ggplot(df_new, aes(x=sensitivity_punish, fill=DREScat)) + geom_density(alpha=.3) + scale_fill_manual(values=cbPalette) + theme_minimal() 
ggplot(df_new, aes(x=sensitivity_reward, fill=DREScat)) + geom_density(alpha=.3) + scale_fill_manual(values=cbPalette) + theme_minimal()

#BAR PLOT
sweet_Palette <- c("palegreen", "palegreen2", "seagreen3","springgreen4")
bitter_Palette <- c("indianred1", "orangered2", "firebrick3","red4")

tbl <- with(df_new, table(sweetstim_level, DREScat))
ggplot(as.data.frame(tbl), aes(factor(sweetstim_level), Freq, fill = DREScat)) +     
  geom_col(position = 'dodge', colour="black") + theme_minimal() + scale_fill_manual(values=l_Palette)

tbl2 <- with(df_new, table(bitterstim_level, DREScat))
ggplot(as.data.frame(tbl2), aes(factor(bitterstim_level), Freq, fill = DREScat)) +     
  geom_col(position = 'dodge', colour="black") + theme_minimal() + scale_fill_manual(values=l_Palette)

l_Palette <- c("#f6daff", "#c5f0ff")

tbl3 <- with(df_new, table(test_result_group, DREScat))
ggplot(as.data.frame(tbl3), aes(factor(test_result_group), Freq, fill = DREScat)) + scale_x_discrete(limits=c("learner", "reward_learner","punish_learner", "nonlearner")) +
  geom_col(position = "dodge", colour="black") + theme_minimal() + scale_fill_manual(values=c(l_Palette), name = "Dietary Restraint Groups", labels = c("Low", "High")) +
  labs(y = "Number of Participants", x = "Post Test Performance") + ylim(0,13)

#EXPLORATORY ANALYSIS WITH BIGGER SAMPLES
df <- read.csv("/Users/jennygilbert/Google Drive/NIBL/Projects/18-0417 BeveL/data/clean_data/dres_plus5.csv")
names(df)

describe(df$DRES)
attach(df)
df$DREScat[DRES > 2.1] <- "high_restraint"
df$DREScat[DRES <= 2.1] <- "low_restraint"
detach(df)
table(df$DREScat)

t<- table(df$DREScat, df$test_result_group)
print(t)
chisq.test(df$DREScat, df$test_result_group, correct=FALSE)

t<- table(df$DREScat, df$sweetstim_level)
t
chisq.test(df$DREScat, df$sweetstim_level, correct=FALSE)

l_Palette <- c("#f6daff", "#c5f0ff")

tbl3 <- with(df, table(test_result_group, DREScat))
ggplot(as.data.frame(tbl3), aes(factor(test_result_group), Freq, fill = DREScat)) + scale_x_discrete(limits=c("learner", "reward_learner","punish_learner", "nonlearner")) +
  geom_col(position = "dodge", colour="black") + theme_minimal() + scale_fill_manual(values=c(l_Palette), name = "Dietary Restraint Groups", labels = c("Low", "High")) +
  labs(y = "Number of Participants", x = "Post Test Performance") + ylim(0,13)

####EXPLORATORY ANALYSIS WITH DISINHIBITION
df$disinhibition_score <- df$emotional_eating + df$external_eating
df_new$disinhibition_score

describeBy(df_new$disinhibition_score, df_new$DREScat)
t.test(disinhibition_score ~ DREScat, data = df_new)

reg1 <- lm(disinhibition_score ~ test_result_group, data=df)
summary(reg1)

aov<- aov(df$disinhibition_score ~ df$test_result_group)
summary(aov)


reg1 <- lm(disinhibition_score ~ sensitivity_punish + sensitivity_reward, data=df_new)
summary(reg1)

