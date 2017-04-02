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

## ----------------------------
## Exploratory analysis

# List length of rows / columns
dim(ToothGrowth)

# Determine column data types
str(ToothGrowth)

# Range of tooth growth
range(ToothGrowth$len)

# Levels and counts of delivery methods/supplements
table(ToothGrowth$supp)

# Levels and counts of dosages
table(ToothGrowth$dose)

## ----------------------------
## Data summary 

# Summarize columns 
summary(ToothGrowth)

# Let's look at our 30 guinea pigs given orange
# juice and ascorbic acid
oj <- ToothGrowth[ToothGrowth$supp == "OJ",]
vc <- ToothGrowth[ToothGrowth$supp == "VC",]

# A histogram of tooth odontoblast length
# appears to have a positive slope possibly 
# due to increasing dosages of orange juice. 
hist(oj$len)


# The histogram of the tooth growth for guinea
# pigs given ascorbic acid looks almost normal.
hist(vc$len)


## ----------------------------
## Comparison of tooth growth by supp and dose

# So lets compare tooth growth by supplement by 
# dosage levels

# Given an 0.5 milligram/day dosage, we have
# 10 samples of guinea pigs given orange juice
# and 10 samples of guinea pigs given ascorbic acid
oj05 <- ToothGrowth[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "OJ",]
vc05 <- ToothGrowth[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "VC",]
dim(oj05)
dim(vc05)

# Our means and standard error for the orange
# juice samples at the 0.5 mg/day level 
mean(oj05$len)  # 13.23
sd(oj05$len)    # 4.459709

# So we conduct a t-test on the orange 
# juice sample at the 0.5 mg/day dosage level.
# This shows we can reject the null hypothesis
# that the orange juice had no impact on the 
# guinea pigs' tooth growth.
t.test(oj05$len)
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