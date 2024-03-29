---
title: "Split-Treat Descriptive Stats"
author: "Dan Aurell"
date: "2023-10-27"
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
an_dat <- read.csv("./data_prepared/an_dat.csv")
```


# Descriptive stats - including some plotting
Which colony issues were present in how many numbers per group


```r
# Failure to mate per treatment - within non-EFB colonies
loss_summary <- an_dat %>% 
  filter(efb == 0) %>% 
  group_by(trt_label) %>% 
  summarize(n = n(),

            failed_num = sum(failed),
            failed.p = failed_num/n,
            
            weak_num = sum(weak),
            weak.p = weak_num/n,
            
            QL_num = sum(QL),
            QL.p = QL_num/n,
            
            DL_num = sum(DL),
            DL.p = DL_num/n,
            
            died_num = sum(died),
            died.p = died_num/n,
            
            survived_num = sum(survived),
            survived.p = survived_num/n
          )
```



Prep failure type table for graphing

```r
loss_summary_pivoted <- loss_summary %>% 
  pivot_longer(c(failed.p, weak.p, QL.p, DL.p), names_to = "reason", values_to = "proportion") 


loss_summary_pivoted <- loss_summary_pivoted %>% 
  mutate(
    reason_label = if_else(reason == "weak.p", "Weak",
                           if_else(reason == "failed.p", "Failed to mate",
                                   if_else(reason == "DL.p", "Drone layer",
                                           if_else(reason == "QL.p", "Queenless", "NA"
                           )
    
    
    ))))

# Relevel factors for plotting

loss_summary_pivoted <- loss_summary_pivoted %>%
     mutate(trt_label = fct_relevel(trt_label, "Control", "Apivar", "Amitraz E.C.", "OA Dribble", "5x OA Dribble", "OA Vapor", "HopGuard"),
            reason_label = fct_relevel(reason_label,"Weak", "Drone layer", "Queenless", "Failed to mate")
            )
```


## To be able to pull out colonies that had certain issues, should I make vectors of colonies with those issues?

- No, I can filter based on the EFB column to make an analytical dataset


```r
# Which colonies did I identify as having each issue - make vectors

# EFB

# Weak
weak_colonies <- an_dat %>% 
  filter(weak == 1)


weak_colonies$split_ids # "22-003" "22-050" "22-058" "22-142" "22-155"
```

```
## NULL
```

```r
# "22-003" - missing
# "22-050" - missing
# "22-058" - missing
# "22-142" - exists but is quite low 
# "22-155" - missing
```





# Plotting survival and issues


```r
# With "weak"

loss_summary_pivoted %>% 
  ggplot(aes(x = trt_label, y = proportion, fill = reason_label)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(limits = c(0,1)) + 
  ylab("Proportion of colonies") +
  theme_classic(base_size = 30) + 
  theme(axis.text.x=element_text(angle=45,hjust=1), axis.title.x=element_blank(), legend.title = element_blank())
```

![](2_1-Descriptive_Stats_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# ggsave("./outputs/2023-11-04 CoD v3.png", width = 12, height = 8.625, units = "in")


# Without "weak"

loss_summary_pivoted %>% 
  filter(reason != "weak.p") %>% 
  ggplot(aes(x = trt_label, y = proportion, fill = reason_label)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(limits = c(0,1)) + 
  ylab("Proportion of colonies") +
  theme_classic(base_size = 30) + 
  theme(axis.text.x=element_text(angle=45,hjust=1), axis.title.x=element_blank(), legend.title = element_blank())
```

![](2_1-Descriptive_Stats_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# ggsave("./outputs/2023-11-04 CoD v5.png", width = 12, height = 8.625, units = "in")
```


Only "weak" colonies


```r
loss_summary %>% 
  ggplot(aes(x = trt_label, y = weak_num)) +
  geom_col() +
  ylab(NULL) +
  scale_y_continuous(breaks = c(0,1,2)) +
  theme_classic(base_size = 30) + 
  theme(axis.text.x=element_text(angle=45,hjust=1), axis.title.x=element_blank(), legend.title = element_blank())
```

![](2_1-Descriptive_Stats_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
# ggsave("./outputs/2023-11-05 Count of weak v1.png", width = 12, height = 4, units = "in")
```



