# Tooth Growth Analysis
# Statistical Inference Class
# Mark Culp
# 3/22/2017

## Overview:

# This analysis examines the effects of vitamin C on 
# tooth growth in guinea pigs.  Each of the 60 guinea 
# pigs studied received one of three doses of vitamin
# C per day: 0.5, 1.0, or 2.0 mg/day.  There were two 
# delivery methods: orange juice (OJ) and ascorbic 
# acid (VC).  The response measures the growth in 
# odontoblasts (cells responsible for tooth growth).

library(datasets)
data("ToothGrowth")

options(scipen = 999)

## ----------------------------
## Exploratory analysis

# First we conduct an exploratory analysis of 
# the "ToothGrowth" dataset.  We need to 
# understand the state of our data.

# So this appears to be a very tidy data set: 
# 30 guinea pigs were give orange juice and 30 
# were given ascorbic acid.  10 from each set 
# of 30 guinea pigs were given 0.5, 1.0, or 2.0 
# milligrams/day of one of the two supplements.

# List length of rows / columns
dim(ToothGrowth)

# Determine column data types
str(ToothGrowth)

# Range of tooth growth
range(ToothGrowth$len)

# Levels and counts of delivery methods/supplements
table(ToothGrowth$supp)

# Levels and counts of dosages
table(ToothGrowth$dose, ToothGrowth$supp)

## ----------------------------
## Data summary 

# Let's look at our 30 guinea pigs given orange
# juice and ascorbic acid
oj <- ToothGrowth[ToothGrowth$supp == "OJ",]
oj05 <- oj[oj$dose == 0.5,]
oj10 <- oj[oj$dose == 1.0,]
oj20 <- oj[oj$dose == 2.0,]

# Plot tooth odontoblast length in relation
# to increasing dosages of orange juice. 
boxplot(len ~ dose, data = oj,
        boxwex = 0.25, at = 1:3 - 0.2,
        col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Orange juice dosage in milligrams",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")


# ------------------------------------------

# Subset the guinea pigs given ascorbic acid,
# overall and by dosages
vc <- ToothGrowth[ToothGrowth$supp == "VC",]
vc05 <- vc[vc$dose == 0.5,]
vc10 <- vc[vc$dose == 1.0,]
vc20 <- vc[vc$dose == 2.0,]

# Plot tooth odontoblast length in relation
# to increasing dosages of ascorbic acid. 
boxplot(len ~ dose, data = vc,
        boxwex = 0.25, at = 1:3 - 0.2,
        col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Ascorbic Acid dosage in milligrams",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")


# ------------------------------------------

## -----------------------------------------
## Comparison of tooth growth by supp and dose

# So lets compare tooth growth by supplement by 
# dosage levels

# Means and standard error for the orange
# juice samples 
mnsOJ <- rbind(
   c("0.5",mean(oj05$len),round(sd(oj05$len),3),round(var(oj05$len),3)),
   c("1.0",mean(oj10$len),round(sd(oj10$len),3),round(var(oj10$len),3)),
   c("2.0",mean(oj20$len),round(sd(oj20$len),3),round(var(oj20$len),3))
)
mnsOJ

# ---------------------------------------------

# Means and standard error for the ascorbic
# acid samples 
mnsVC <- rbind(
  c("0.5",mean(vc05$len),round(sd(vc05$len),3),round(var(vc05$len),3)),
  c("1.0",mean(vc10$len),round(sd(vc10$len),3),round(var(vc10$len),3)),
  c("2.0",mean(vc20$len),round(sd(vc20$len),3),round(var(vc20$len),3))
)
mnsVC

# ---------------------------------------------

# So we conduct a t-test on the orange 
# juice sample at the 0.5 mg/day dosage level.
# This shows we can reject the null hypothesis
# that the orange juice had no impact on the 
# guinea pigs' tooth growth.
t.test(oj05$len)[4]
# t = 9.3811, df = 9, p-value = 0.000006074
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  10.03972 16.42028
#  sample estimates:
#  mean of x 
#  13.23

# ------------------------------------------

# Our means and standard error for the ascorbic
# acid samples at the 0.5 mg/day dosage level
mean(vc05$len)  # 7.98
sd(vc05$len)    # 2.746634

# So we conduct a t-test on the ascorbic 
# acid sample at the 0.5 mg/daydosage level.
# This shows we can reject the null hypothesis
# that the ascorbic acid had no impact on the 
# guinea pigs' tooth growth.
t.test(vc05$len)
# t = 9.1876, df = 9, p-value = 0.00000721
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  6.015176 9.944824
#  sample estimates:
#  mean of x 
#  7.98 

# --------------------------------------------

# We now compare the guinea pigs tooth growth 
# under both supplements: 
# ----------------------
# At the 0.5 mg/daydosage level
tg05 <- ToothGrowth[ToothGrowth$dose == 0.5,] 
t.test(len ~ supp, paired = F, var.equal = F, data = tg05)
#  t = 3.1697, df = 14.969, p-value = 0.006359
#  alternative hypothesis: true difference in means is not equal to 0
#  95 percent confidence interval:
#   1.719057 8.780943
#   sample estimates:
#   mean in group OJ mean in group VC 
#              13.23             7.98

# At the 1.0 mg/daydosage level
tg10 <- ToothGrowth[ToothGrowth$dose == 1.0,] 
t.test(len ~ supp, paired = F, var.equal = F, data = tg10)
#  t = 4.0328, df = 15.358, p-value = 0.001038
#  alternative hypothesis: true difference in means is not equal to 0
#  95 percent confidence interval:
#    2.802148 9.057852
#  sample estimates:
#  mean in group OJ mean in group VC 
#             22.70            16.77 

# At the 2.0 mg/daydosage level
tg20 <- ToothGrowth[ToothGrowth$dose == 2.0,] 
t.test(len ~ supp, paired = F, var.equal = F, data = tg20)
#  t = -0.046136, df = 14.04, p-value = 0.9639
#  alternative hypothesis: true difference in means is not equal to 0
#  95 percent confidence interval:
#    -3.79807  3.63807
#  sample estimates:
#    mean in group OJ mean in group VC 
#               26.06            26.14 

# The power of the supplement sample comparisons at the
# 0.5 mg/day level are:
power.t.test(n=10,delta = 13.23,sd= 4.459709, type="two.sample", alt = "one.sided")$power

## ----------------------------
## Conclusions and assumptions