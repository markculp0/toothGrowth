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

## ----------------------------
## Comparison of tooth growth by supp and dose

## ----------------------------
## Conclusions and assumptions