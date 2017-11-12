# Survival Analysis is used to determine the time before certain things happen
# before customer buy the first purchase
# a person probability of risk over a period of time
# so this helps whenever you have to anlayse the Time Factor
# but it doesn't seems to be predictive in nature , this is a descriptive anlaysis sort of as of now


require(survival)
require(survminer)
library(survminer)

# cool we are trying to do a survival function 
#https://www.r-exercises.com/2017/11/10/survival-ggplot-exercises1/


data1 = lung
head(data1)
dim(data1)

# Simple statement to see the data type of each column before I start
str(data1)
head(data1$status)
table(data1$status)
data1$status = as.factor(data1$status)
str(data1)
cat("Censored: ") + cat(table(data1$status)[1])
print("censored")

# why always forget the basic stuff to do any sort of filter
# I think you should stick with one and master that 
# basically say you are used to of tidyr then use it multiple times

library(sqldf)
data.frame(table(data1$age,c= 1))
sur = survfit(Surv(time,status == 2)~1,data = lung)
# this gives me some sort of ranges 
plot(sur)

# having  a better plot for survival analysis
library(survminer)
ggsurvplot(sur,risk.table = TRUE)

# now I want to see how many will survive over time with a starta of 
# input variables
#starting with sex
sur = survfit(Surv(time,status == 2)~(age),data = lung)
ggsurvplot(sur,risk.table = TRUE)


# why i forget the basic things
# ok so in this I want to see the survival analysis w.r.t to age
# can i convert them into bin 
# how to convert a continuous variable into bin

library(binr)
data1$age_fact = cut(data1$age,b = 5)
table(data1$age_fact)

sur = survfit(Surv(time,status == 2)~age_fact,data = data1)
ggsurvplot(sur,risk.table = TRUE)

# Considering a hypothesis there is no difference between sex = 1 and sex = 2
sur = survdiff(Surv(time,status == 2)~(sex),data = lung)
sur
View(lung)