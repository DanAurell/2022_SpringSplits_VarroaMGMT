---
title: "Analysis - Colony development"
author: "Dan Aurell"
date: "2024-01-07"
output:   
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.1     ✔ purrr   1.0.1
## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
## ✔ readr   2.1.4     ✔ forcats 1.0.0
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(lme4)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```


# Read in data


```r
temp <- read.csv("./data_prepared/an_dat.csv")
```

# Prep data for analysis


```r
temp <- temp %>% 
  rename(
    fob_44 = ch44_fob,
    fob_58 = ch58_fob,
    fob_72 = ch72_fob,
    fob_105 = post_quick_fob
  )
```



## Analyze day 77 Colony strength


```r
m.bees <- lm(frames_bees~trt_label, data = temp) # Next run should include a yard level random effect
m.bees.2 <- lmer(frames_bees~trt_label + (1 | new_yard), data = temp)
m.bees.3 <- lmer(frames_bees~ 1 + (1 | new_yard), data = temp)
anova(m.bees.2, m.bees.3) # No improvement to include a treatment effect (p=1)
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: temp
## Models:
## m.bees.3: frames_bees ~ 1 + (1 | new_yard)
## m.bees.2: frames_bees ~ trt_label + (1 | new_yard)
##          npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
## m.bees.3    3 532.80 541.49 -263.40   526.80                     
## m.bees.2    9 544.55 570.63 -263.27   526.55 0.2515  6     0.9997
```

```r
# Still make predictions based on m2, but no statistical detected effect of treatment (p=1)
```

## Analyze data for colony weight post honey production


```r
m.weight <- lm(post_lbs_C~trt_label, data = temp)
m.weight.2 <- lmer(post_lbs_C ~ trt_label + (1 | new_yard), data = temp)
m.weight.3 <- lmer(post_lbs_C ~ 1 + (1 | new_yard), data = temp)
anova(m.weight.2, m.weight.3) # No effect of treatment (p=0.77)
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: temp
## Models:
## m.weight.3: post_lbs_C ~ 1 + (1 | new_yard)
## m.weight.2: post_lbs_C ~ trt_label + (1 | new_yard)
##            npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
## m.weight.3    3 1196.0 1204.4 -595.00   1190.0                    
## m.weight.2    9 1204.7 1230.0 -593.35   1186.7 3.292  6     0.7714
```

```r
# Still graph predictions based on m2 to estimate effect size
```

