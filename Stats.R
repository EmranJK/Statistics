############################################################
#
# Statistics Assignment (Stats and Probability)
#
# Please run the commands seperately
#
# R commands 
#
# q1: paired t.test
# q3: anova
# less than 0.05: null hypthesys is rejected.
# greater than 0.05: null hypothesys is not rejected.

# Shapiro test must be performed to check the assumption of normality after doing the t.test when p value is less than 0.05
# HZ.test must be performed to check the assumptions after the correlation test when p value is less than 0.05
# Boxplot for t.test
# Plot for correlation
# Plot for Chi-Square test

setwd("C://Users//USER//Desktop//Statistics CA/")
Emran = read.table("Sabbagh_Emran_20088634.csv", sep= ",", header = TRUE)
attach(Emran)
view(Emran)
head(Emran)
names(Emran)


tapply(q4_spend, q1_sex, mean, na.rm=T)
tapply(q4_spend, q1_sex, sd, na.rm=T)
tapply(q4_spend, q1_sex, length)
shapiro.test(q4_spend[q1_sex==1])
ks.test(weight)
t.test(weight1, weight2, paired=T)
t.test(weight1 ~ weight2)
cor.test(q2_age, q2_spend, use="pairwise.complete")
HZ.test(data.frame(age, spend))
tmp = !is.na(q4_spend) & !is.na(q2_age)
HZ.test(data.frame(q4_spend[tmp], q2_age[tmp]))
boxplot(q4_spend ~ q1_sex, main="Spend by Sex", xlab="Sex")
plot(q4_spend ~ q1_sex, xlab="Age", pch=20, bty="L")
!is.na(q4_spend)
is.na(q4_spend)
summary(aov(q4_spend~as.factor(q3_goods)))
plot(as.factor(q4_spend), as.factor(q1_sex))
chisq.test(as.factor(q4_spend), as.factor(q1_sex))
t.test(examScore, midTermScore, paired=T)



############################################################

# Q1 #######################################################
tapply(examScore, stud.sex, mean, na.rm=T)
tapply(examScore, stud.sex, sd, na.rm=T)
tapply(examScore, stud.sex, length)
boxplot(examScore ~ as.factor(stud.sex), main="Score by Sex", ylab="Sex", xlab="Score", horizontal=T)
t.test(examScore ~ stud.sex)
# 0.7

# Q2 #######################################################
summary(examScore)
summary(midTermScore)
plot(examScore ~ midTermScore, xlab="Mid Term Exam", ylab="Final Exam", pch=20, bty="L")
t.test(examScore, midTermScore, paired=T)
#0.02

tmp = !is.na(examScore) & !is.na(midTermScore)
HZ.test(data.frame(examScore[tmp], midTermScore[tmp]))
#0.34

# Q3 #######################################################
plot(as.factor(Disp) ~ as.factor(stud.sex), xlab="Sex", ylab="Disciplines", pch=20, bty="L")
table(as.factor(Disp), as.factor(stud.sex))
chisq.test(as.factor(Disp), as.factor(stud.sex))
#0.007 

# Q4 #######################################################
summary(examScore)
summary(lcPoints)
plot(lcPoints ~ examScore, xlab="Final Exam Score", ylab="LC Points", pch=20, bty="L")
cor.test(lcPoints, examScore)
# 0.7562

# Q5 #######################################################
tapply(examScore, Disp, mean, na.rm=T)
tapply(examScore, Disp, sd, na.rm=T)
tapply(examScore, Disp, length)
boxplot(examScore ~ as.factor(Disp), xlab="Final Exam Score", ylab="Disciplines", main="Score by Discipline", horizontal=T)
summary(aov(examScore ~ as.factor(Disp)))
# 0.107

# Q6 #######################################################
tapply(examScore, stud.group, mean, na.rm=T)
tapply(examScore, stud.group, sd, na.rm=T)
tapply(examScore, stud.group, length)
boxplot(examScore ~ as.factor(stud.group), xlab="Final Exam Score", ylab="Lecturer Groups", main="Score by Lecturer Groups", horizontal=T)
summary(aov(examScore ~ as.factor(stud.group)))
shapiro.test(examScore[stud.group==1])
#  0.9802
shapiro.test(examScore[stud.group==2])
# 0.1468
shapiro.test(examScore[stud.group==3])
# 0.3766
shapiro.test(examScore[stud.group==4])
# 0.5328

# Q7 #######################################################
summary(examScore)
summary(studIQ)
plot(examScore ~ studIQ, main="Final Exam Score and IQ Relation", xlab="IQ", ylab="Final Exam Score", pch=20, bty="L")
cor.test(examScore, studIQ)
# 2.2e-16 
tmp = !is.na(examScore) & !is.na(studIQ)
HZ.test(data.frame(examScore[tmp], studIQ[tmp]))
# 0.4253441 

# Q8 #######################################################
plot(as.factor(Disp) ~ as.factor(stud.group), xlab="Group", ylab="Disciplines", pch=20, bty="L")
table(as.factor(Disp), as.factor(stud.group))
chisq.test(as.factor(Disp), as.factor(stud.group))
#0.2858

# Q9 #######################################################
summary(examScore)
summary(studyHours)
plot(examScore ~ studyHours, main="Final Exam Score and Study Hours Relation", xlab="Study Hours", ylab="Final Exam Score", pch=20, bty="L")
cor.test(examScore, studyHours)
# 0.00012 
tmp = !is.na(examScore) & !is.na(studyHours)
HZ.test(data.frame(examScore[tmp], studyHours[tmp]))
# 0.4253441 
