---
title: "Split-Treat Data Preparation"
author: "Dan Aurell"
date: "2023-10-27"
output:   
  html_document: 
    keep_md: yes
---


# Workflow 

## What data do I have from the app?

App data - d77
-
Shaw 5/24/2022
Wire 5/31/2022
Shooting Range 6/7/2022
Turfgrass 6/11/2022

Post-harvest - no app data
-
Shaw 7/1/2022
Wire 7/2/2022
Shooting Range 7/2/2022
Turfgrass 7/2/2022




# Setup


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

```r
library(stringr)
```


# Read in data

## Read csv

```r
paper <- read.csv("./data/manual_merge_paper_2023-05-23 v2.csv")
colony <- read.csv("./data/2022-06-14_125633__techteam__all_sample_export.csv", skip = 9, na.strings = c("", " ", "NA"))
frame <- read.csv("./data/2022-06-14_125650__techteam__frame_side_export.csv")
```

## Pare down and rename columns

Paper data

```r
paper <- paper %>% 
  rename(colony_num = split_ids)
```

Frame-level data

```r
frame2 <- frame %>% 
  rename(
    project = frame__box__sample__sample_event__project,  
    colony_num = frame__box__sample__colony_num,
    date = frame__box__sample__sample_event__collection_date,
    box_num = frame__box__box_number,
    box_size = frame__box__size,
    frame_num = frame__frame_number,
    side = side_name,
    bees = bee_coverage,
    cap_wk = capped_worker_brood_coverage,
    cap_dr = capped_drone_brood_coverage,
    honey = honey_coverage,
    pollen = pollen_coverage,
    extra_bees = frame__box__bee_coverage,
    inspection_order = frame__box__sample__inspection_order
         )


frame2 <- frame2 %>% 
  select(
        project,
    colony_num,
    date,
    box_num,
    box_size,
    frame_num,
    side,
    bees,
    cap_wk,
    cap_dr,
    honey,
    pollen,
    extra_bees,
    inspection_order,
         id,
         frame,
         notes
         )

frame2 <- frame2 %>% 
  filter(date %in% c("2022-05-24", "2022-05-31", "2022-06-07", "2022-06-11")) 


# Quality control
unique(frame2$colony_num)
```

```
##   [1] "22-018" "22-016" "22-005" "22-022" "22-021" "22-010" "22-009" "22-011"
##   [9] "22-012" "22-035" "22-036" "22-037" "22-031" "21-034" "22-033" "22-007"
##  [17] "22-040" "22-042" "22-013" "22-001" "22-002" "22-027" "22-030" "22-026"
##  [25] "22-014" "22-032" "22-017" "22-004" "22-019" "22-020" "22-008" "22-061"
##  [33] "22-060" "22-059" "22-068" "22-069" "22-086" "22-053" "22-072" "22-071"
##  [41] "22-070" "22-074" "22-077" "22-076" "22-075" "22-049" "22-048" "22-047"
##  [49] "22-046" "22-062" "22-063" "22-065" "22-064" "22-056" "22-054" "22-057"
##  [57] "22-044" "22-043" "22-045" "22-078" "22-084" "22-079" "22-085" "22-080"
##  [65] "22-100" "22-099" "22-098" "22-097" "22-115" "22-116" "22-113" "22-114"
##  [73] "22-087" "22-089" "22-093" "22-081" "22-109" ""       "22-104" "22-096"
##  [81] "22-094" "22-095" "22-120" "22-108" "22-091" "22-092" "22-122" "22-121"
##  [89] "22-126" "22-125" "22-123" "22-127" "22-124" "22-119" "22-112" "22-101"
##  [97] "22-103" "22-162" "22-165" "22-163" "22-154" "22-157" "22-161" "22-160"
## [105] "22-144" "22-147" "22-146" "22-145" "22-176" "22-174" "22-168" "22-166"
## [113] "22-169" "22-167" "22-136" "22-139" "22-137" "22-138" "22-172" "22-173"
## [121] "22-171" "22-135" "22-134" "22-132" "22-129" "22-130" "22-131" "22-140"
## [129] "22-141" "22-142" "22-143" "22-148" "22-149" "22-151" "22-152" "23-153"
## [137] "22-170"
```

```r
 frame2 %>% 
  group_by(date, colony_num) %>% 
  summarize(n = n()) %>% 
   arrange(desc(n))
```

```
## `summarise()` has grouped output by 'date'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 138 × 3
## # Groups:   date [4]
##    date       colony_num     n
##    <chr>      <chr>      <int>
##  1 2022-05-24 "22-005"      72
##  2 2022-06-07 ""            72
##  3 2022-06-11 "22-149"      72
##  4 2022-05-24 "22-031"      58
##  5 2022-05-24 "22-004"      56
##  6 2022-05-24 "21-034"      54
##  7 2022-05-24 "22-009"      54
##  8 2022-05-24 "22-010"      54
##  9 2022-05-24 "22-011"      54
## 10 2022-05-24 "22-012"      54
## # … with 128 more rows
```
## Deal with "extra bees" so that it is counted only once per box

```r
frame2 %>% 
  group_by(date, colony_num) %>% 
  summarize(n = n()) # There are 36-72 rows per colony and date
```

```
## `summarise()` has grouped output by 'date'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 138 × 3
## # Groups:   date [4]
##    date       colony_num     n
##    <chr>      <chr>      <int>
##  1 2022-05-24 21-034        54
##  2 2022-05-24 22-001        36
##  3 2022-05-24 22-002        36
##  4 2022-05-24 22-004        56
##  5 2022-05-24 22-005        72
##  6 2022-05-24 22-007        36
##  7 2022-05-24 22-008        38
##  8 2022-05-24 22-009        54
##  9 2022-05-24 22-010        54
## 10 2022-05-24 22-011        54
## # … with 128 more rows
```

```r
frame2$extra_bees_temp <- frame2$extra_bees # clone column
frame2$extra_bees <- NA # Erase the values in original column

frame2 <- frame2 %>% 
  mutate(extra_bees = ifelse(frame_num == 1 & side == "A", extra_bees_temp, NA)) %>% 
  select(-extra_bees_temp)

# Replace NA with 0
frame2$bees[is.na(frame2$bees)] <- 0
frame2$extra_bees[is.na(frame2$extra_bees)] <- 0

# Replace 2520 with 25
frame2$bees[frame2$bees == 2520] <- 0

frame2 <- frame2 %>% 
  mutate(bees_total = bees + extra_bees)
```


Correct resource coverages for different box/frame sizes - express all as percentage of one side of a Deep frame

```r
# Convert to Deep-frame equivalents
frame2 <- frame2 %>% mutate(
  conversion = ifelse(box_size == "Medium", 0.64, 1)
)

names(frame2)
```

```
##  [1] "project"          "colony_num"       "date"             "box_num"         
##  [5] "box_size"         "frame_num"        "side"             "bees"            
##  [9] "cap_wk"           "cap_dr"           "honey"            "pollen"          
## [13] "extra_bees"       "inspection_order" "id"               "frame"           
## [17] "notes"            "bees_total"       "conversion"
```

```r
# append "_C" to a column to mean converted
frame2 <- frame2 %>% 
  mutate(
    bees_C = bees*conversion,
    cap_wk_C = cap_wk*conversion,
    cap_dr_C = cap_dr*conversion,
    honey_C = honey*conversion,
    pollen_C = pollen*conversion
  )
```

## Summarize into a "resources" data frame that sums resources for each colony, since the colony is the experimental unit

Also convert from percentages of frame-sides to frames of [resource]


```r
resources <- frame2 %>% 
  group_by(colony_num) %>% 
  # 100 would mean that there is one complete side of bees
  
  # Divide by 100 to get "sides"
  # Then divide by 2 to get "frames"

  # In total, divide by 200
  
  summarise(
    frames_bees = sum(bees_C, na.rm = TRUE)/200,
    frames_wbr = sum(cap_wk_C, na.rm = TRUE)/200,
    frames_dbr = sum(cap_dr_C, na.rm = TRUE)/200,
    frames_honey = sum(honey_C, na.rm = TRUE)/200
    )
```


Colony-level data

```r
names(colony)
```

```
##   [1] "X"                                            
##   [2] "sample_event__beekeeper__id"                  
##   [3] "sample_event__beekeeper__tech_team_code"      
##   [4] "sample_event__beekeeper__tech_team_membership"
##   [5] "sample_event__visit__id"                      
##   [6] "sample_event__visit__date"                    
##   [7] "sample_event__visit__merits_discount"         
##   [8] "sample_event__visit__billable_days"           
##   [9] "sample_event__visit__number_of_specialists"   
##  [10] "sample_event__visit__tt1_name"                
##  [11] "sample_event__visit__tt2_name"                
##  [12] "sample_event__visit__tt3_name"                
##  [13] "sample_event__visit__tt1_hours"               
##  [14] "sample_event__visit__tt2_hours"               
##  [15] "sample_event__visit__tt3_hours"               
##  [16] "sample_event__visit__notes"                   
##  [17] "sample_event__visit__visit_type"              
##  [18] "sample_event__visit__date_entered"            
##  [19] "sample_event__location__location_name"        
##  [20] "sample_event__location__latitude"             
##  [21] "sample_event__location__longitude"            
##  [22] "sample_event__location__state"                
##  [23] "sample_event__location__county"               
##  [24] "sample_event__location__area_fips"            
##  [25] "sample_event__location__street_address"       
##  [26] "sample_event__location__city"                 
##  [27] "sample_event__location__zipcode"              
##  [28] "sample_event__location__is_dummy"             
##  [29] "sample_event__location__date_entered"         
##  [30] "sample_event__beekeeper_id"                   
##  [31] "sample_event__original_id"                    
##  [32] "sample_event__project"                        
##  [33] "sample_event__rtdl_code"                      
##  [34] "sample_event__collection_date"                
##  [35] "sample_event__sample_month"                   
##  [36] "sample_event__sample_sheet_label"             
##  [37] "sample_event__location_id"                    
##  [38] "sample_event__tech_team"                      
##  [39] "sample_event__sampler"                        
##  [40] "sample_event__event_notes"                    
##  [41] "sample_event__report_sent"                    
##  [42] "sample_event__total_colonies"                 
##  [43] "sample_event__healthy_colonies"               
##  [44] "sample_event__nonhealthy_colonies"            
##  [45] "sample_event__dead_colonies"                  
##  [46] "sample_event__pallet_count"                   
##  [47] "sample_event__pallet_type"                    
##  [48] "sample_event__pallet_colonies_missing"        
##  [49] "sample_event__seasonal_info"                  
##  [50] "sample_event__reason_for_sampling"            
##  [51] "sample_event__previous_locations"             
##  [52] "sample_event__management_notes"               
##  [53] "sample_event__migrated_to_ushbs"              
##  [54] "sample_event__nhbs_kit_id"                    
##  [55] "sample_event__apiary_migratory_operation"     
##  [56] "sample_event__apiary_primary_function"        
##  [57] "sample_event__sampled_colonies"               
##  [58] "sample_event__sampler_address"                
##  [59] "sample_event__sampler_phone"                  
##  [60] "sample_event__state_origin"                   
##  [61] "sample_event__longitudinal"                   
##  [62] "sample_event__is_dummy"                       
##  [63] "sample_event__date_entered"                   
##  [64] "id"                                           
##  [65] "sample_event_id"                              
##  [66] "original_id"                                  
##  [67] "colony_num"                                   
##  [68] "bottle_id"                                    
##  [69] "yard_group"                                   
##  [70] "colony_status"                                
##  [71] "colony_type"                                  
##  [72] "eightortenframes"                             
##  [73] "deeps_num"                                    
##  [74] "threequarters_num"                            
##  [75] "mediums_num"                                  
##  [76] "shallows_num"                                 
##  [77] "supers_num"                                   
##  [78] "super_weight"                                 
##  [79] "nuc"                                          
##  [80] "colony_health"                                
##  [81] "queen_status"                                 
##  [82] "frames_of_bees"                               
##  [83] "brood_pattern"                                
##  [84] "temperament"                                  
##  [85] "weight"                                       
##  [86] "qualitative_weight"                           
##  [87] "color"                                        
##  [88] "notes"                                        
##  [89] "bee_type"                                     
##  [90] "colony_id"                                    
##  [91] "cdb_observed"                                 
##  [92] "chalk_observed"                               
##  [93] "entomb_observed"                              
##  [94] "varroa_observed"                              
##  [95] "dwv_observed"                                 
##  [96] "pms_observed"                                 
##  [97] "afb_observed"                                 
##  [98] "efb_observed"                                 
##  [99] "shiny_observed"                               
## [100] "sbv_observed"                                 
## [101] "waxmoth_observed"                             
## [102] "shbl_observed"                                
## [103] "shba_observed"                                
## [104] "cbpv_observed"                                
## [105] "bald_brood_observed"                          
## [106] "mdl_observed"                                 
## [107] "queen_cells_observed"                         
## [108] "hygienic_sample"                              
## [109] "varroa_sample"                                
## [110] "composite_varroa_sample_group"                
## [111] "nosema_sample"                                
## [112] "composite_nosema_sample_group"                
## [113] "virus_sample"                                 
## [114] "composite_virus_sample_group"                 
## [115] "composite_pesticide_sample_group"             
## [116] "protein_sample"                               
## [117] "pollen_sample"                                
## [118] "afb_efb_sample"                               
## [119] "tracheal_mite_sample"                         
## [120] "empty_0_hrs"                                  
## [121] "sealed_24_hrs"                                
## [122] "partials_24_hrs"                              
## [123] "uncapped_removing"                            
## [124] "percent_removed"                              
## [125] "cell_count_time_start"                        
## [126] "cell_count_time_end"                          
## [127] "pollen_trap_installed"                        
## [128] "hive_scale_installed"                         
## [129] "recent_requeen"                               
## [130] "recent_move"                                  
## [131] "recent_feeding"                               
## [132] "recent_treatment"                             
## [133] "recent_management"                            
## [134] "varroa_archived"                              
## [135] "bees_archived"                                
## [136] "archived_bottle_id"                           
## [137] "archived_box_id"                              
## [138] "lab_notes"                                    
## [139] "varroa_in_field"                              
## [140] "varroa_in_sample"                             
## [141] "bee_weight_100"                               
## [142] "bee_weight_remaining"                         
## [143] "total_bee_mass"                               
## [144] "bee_count_estimated"                          
## [145] "bee_count_if_less_100"                        
## [146] "drones_in_sample"                             
## [147] "queens_in_sample"                             
## [148] "varroa_per_100_bees"                          
## [149] "total_nosema_spores"                          
## [150] "million_spores_per_bee"                       
## [151] "nosema_spores_categorical"                    
## [152] "tracheal_bees_tested"                         
## [153] "tracheal_bees_positive"                       
## [154] "afb_efb_test_type"                            
## [155] "afb_test_result"                              
## [156] "efb_test_result"                              
## [157] "afb_field_tests_used"                         
## [158] "efb_field_tests_used"                         
## [159] "terramycinreszone"                            
## [160] "tylanreszone"                                 
## [161] "live_bee_sample"                              
## [162] "alcohol_bee_sample"                           
## [163] "frozen_bee_sample"                            
## [164] "open_honey_sample"                            
## [165] "capped_honey_sample"                          
## [166] "fresh_pollen_sample"                          
## [167] "bee_bread_sample"                             
## [168] "corbicular_pellets_sample"                    
## [169] "trapped_pellets_sample"                       
## [170] "wax_sample"                                   
## [171] "propolis_sample"                              
## [172] "royal_jelly_sample"                           
## [173] "eggs_seen"                                    
## [174] "emergency_cells_observed"                     
## [175] "swarm_cells_observed"                         
## [176] "supersedure_cells_observed"                   
## [177] "uncategorized_cells_observed"                 
## [178] "youngest_worker_brood_age"                    
## [179] "oldest_worker_brood_age"                      
## [180] "sample_time"                                  
## [181] "colony_grade"                                 
## [182] "inspection_order"                             
## [183] "colony_weight"                                
## [184] "weight_unit"                                  
## [185] "viral_prevalence_phase"                       
## [186] "brood_area"                                   
## [187] "date_entered"
```

```r
colony2 <- colony %>% 
  select(sample_event__project,
         sample_event__collection_date,
         colony_num,
         deeps_num,
         mediums_num,
         threequarters_num,
         shallows_num,
         supers_num,
         queen_status,
         frames_of_bees,
         efb_observed
         ) %>% 
  rename(project = sample_event__project,
         collection_date = sample_event__collection_date
         )
```



# Understand data and how to analyze it

How to identify just relevant app data for day 77
"colony" and "frame" are the colony and frame level app data

```r
unique(colony$project)
```

```
## NULL
```

```r
pattern <- "date"
  grep(pattern, names(colony), value=TRUE)  
```

```
## [1] "sample_event__visit__date"           
## [2] "sample_event__visit__date_entered"   
## [3] "sample_event__location__date_entered"
## [4] "sample_event__collection_date"       
## [5] "sample_event__date_entered"          
## [6] "date_entered"
```

```r
pattern <- "coverage"
  grep(pattern, names(frame), value=TRUE)  
```

```
## [1] "frame__box__bee_coverage"     "bee_coverage"                
## [3] "capped_drone_brood_coverage"  "capped_worker_brood_coverage"
## [5] "larvae_eggs_coverage"         "honey_coverage"              
## [7] "pollen_coverage"
```

```r
unique(colony$collection_date)
```

```
## NULL
```

# Prep data 

## Prep colony level app data

```r
# Colony2 is just selected and renamed

colony2 %>% 
  filter(collection_date %in% c("2022-05-24", "2022-05-31", "2022-06-07", "2022-06-11")) %>% 
  group_by(collection_date, colony_num) %>% 
  summarize(n())
```

```
## `summarise()` has grouped output by 'collection_date'. You can override using
## the `.groups` argument.
```

```
## # A tibble: 143 × 3
## # Groups:   collection_date [4]
##    collection_date colony_num `n()`
##    <chr>           <chr>      <int>
##  1 2022-05-24      21-034         3
##  2 2022-05-24      22-001         2
##  3 2022-05-24      22-002         2
##  4 2022-05-24      22-004         1
##  5 2022-05-24      22-005         3
##  6 2022-05-24      22-007         3
##  7 2022-05-24      22-008         2
##  8 2022-05-24      22-009         2
##  9 2022-05-24      22-010         2
## 10 2022-05-24      22-011         2
## # … with 133 more rows
```

```r
# colony3 is filtered to the data of interest
colony3 <- colony2 %>% 
  filter(collection_date %in% c("2022-05-24", "2022-05-31", "2022-06-07", "2022-06-11")) %>% 
  filter(!is.na(queen_status)) %>% # removes some duplicates introduced by app
  arrange(colony_num)

unique(colony3$colony_num) #138 including the blank space
```

```
##   [1] "21-034" "22-001" "22-002" "22-004" "22-005" "22-007" "22-008" "22-009"
##   [9] "22-010" "22-011" "22-012" "22-013" "22-014" "22-016" "22-017" "22-018"
##  [17] "22-019" "22-020" "22-021" "22-022" "22-026" "22-027" "22-030" "22-031"
##  [25] "22-032" "22-033" "22-035" "22-036" "22-037" "22-040" "22-042" "22-043"
##  [33] "22-044" "22-045" "22-046" "22-047" "22-048" "22-049" "22-053" "22-054"
##  [41] "22-056" "22-057" "22-059" "22-060" "22-061" "22-062" "22-063" "22-064"
##  [49] "22-065" "22-068" "22-069" "22-070" "22-071" "22-072" "22-074" "22-075"
##  [57] "22-076" "22-077" "22-078" "22-079" "22-080" "22-081" "22-084" "22-085"
##  [65] "22-086" "22-087" "22-089" "22-091" "22-092" "22-093" "22-094" "22-095"
##  [73] "22-096" "22-097" "22-098" "22-099" "22-100" "22-101" "22-103" "22-104"
##  [81] "22-108" "22-109" "22-110" "22-111" "22-112" "22-113" "22-114" "22-115"
##  [89] "22-116" "22-119" "22-120" "22-121" "22-122" "22-123" "22-124" "22-125"
##  [97] "22-126" "22-127" "22-129" "22-130" "22-131" "22-132" "22-134" "22-135"
## [105] "22-136" "22-137" "22-138" "22-139" "22-140" "22-141" "22-142" "22-143"
## [113] "22-144" "22-145" "22-146" "22-147" "22-148" "22-149" "22-151" "22-152"
## [121] "22-154" "22-157" "22-160" "22-161" "22-162" "22-163" "22-165" "22-166"
## [129] "22-167" "22-168" "22-169" "22-170" "22-171" "22-172" "22-173" "22-174"
## [137] "22-176" "23-153"
```

```r
# But a few duplicated colonies exist

# Replace 21-043 with 22-043
colony3$colony_num[colony3$colony_num == "21-034"] <- "22-034"

# focus on queen status and efb columns
colony4 <- colony3 %>% 
  select(colony_num, queen_status, efb_observed)

# Rename queen status and efb columns
colony4 <- colony4 %>% 
  rename(ch77_qstatus = queen_status,
         ch77_efb = efb_observed
         )


# Remove duplicate rows introduced by the app
colony4 <- colony4[!duplicated(colony4[, 1]), ]

# "pa" means "paper_app"
pa <- left_join(paper, colony4)
```

```
## Joining with `by = join_by(colony_num)`
```

```r
pa <- pa %>% 
  mutate(
    trt_label = if_else(trt == "1-Control", "Control",
              if_else(trt == "2-Apivar", "Apivar",
                    if_else(trt == "4-Dribble18", "OA Dribble", 
                            if_else(trt == "6-ManyDribble", "5x OA Dribble",
                                   if_else(trt == "5-Vapor18", "OA Vapor",
                                           if_else(trt == "7-HopGuard", "HopGuard",
                                                   if_else(trt == "3-Taktic", "Amitraz E.C.", "NA"
                                                           )
                                                   )))))))

pa <- pa %>%
     mutate(trt_label = fct_relevel(trt_label, "Control", "Apivar", "Amitraz E.C.", "OA Dribble", "5x OA Dribble", "OA Vapor", "HopGuard"))
```

## Prep pa data

Tag colonies that had EFB by day 77

```r
pa$efb_columns <- paste(
  pa$reason_removal, 
  pa$ch77_efb # This did not add any additional ones
  )

pa$efb <- ifelse(grepl("EFB", pa$efb_columns), 1, 0)



# Number of EFB colonies per treatment
pa %>%
  filter(efb == 1) %>% 
  group_by(trt) %>% 
  summarize(n = n()) # Very even across groups
```

```
## # A tibble: 6 × 2
##   trt             n
##   <chr>       <int>
## 1 1-Control       3
## 2 2-Apivar        3
## 3 3-Taktic        3
## 4 4-Dribble18     3
## 5 5-Vapor18       3
## 6 7-HopGuard      4
```

Impute bees in alcohol wash for 3 colonies on day 77

```r
pa %>% 
  filter(ch77_wash_workers == "Missing")
```

```
##   colony_num       new_yard split_date parent_yard parent_id varroa_5cup
## 1     22-078 Shooting Range    3/22/22     Bee Lab    21-243           9
## 2     22-108 Shooting Range    3/22/22        Kiwi    21-191           1
## 3     22-124 Shooting Range    3/22/22     Raleigh    21-108           8
##   splits_made stratum       trt trt_numeric notes ch2_date ch2_var_tray
## 1           3       2 5-Vapor18           5        3/24/22            2
## 2           3       8  3-Taktic           3        3/24/22           NA
## 3           2       3  3-Taktic           3        3/24/22            6
##   ch2_var_wash ch2_var_total ch2_var_total2 ch2_var_total_check
## 1            6             8              8                   0
## 2           NA             0              0                   0
## 3            8            14             14                   0
##   ch2_wash_workers ch2_wash_drones ch2_wash_queens ch18_date ch18_qstatus
## 1              287               2               0    4/9/22          QNS
## 2              309               6               0    4/9/22           QS
## 3              284              26               0    4/9/22          VQS
##                                   ch18_notes ch18_eggs ch18_ageoldest ch18_age
## 1                        Polished brood nest         N           <NA>       NA
## 2                                                    Y             L1        4
## 3 Small VQ seen, and a sealed emergency cell         N           <NA>       NA
##   ch30_date ch30_eggs ch30_qstatus ch30_notes ch30_ageoldest ch30_age  lay_18
## 1   4/21/22         Y           QR                   Prepupa       11 #VALUE!
## 2   4/21/22         Y           QR                    Capped       NA      14
## 3   4/21/22         Y           QR                   Prepupa       11 #VALUE!
##    lay_30 start_layday ch44_date ch44_eggs ch44_qstatus ch44_fob ch44_notes
## 1      19           19    5/5/22         Y           QR       10           
## 2 #VALUE!           14    5/5/22         Y           QR       11           
## 3      19           19    5/5/22         Y           QR        8           
##   ch58_date ch58_eggs ch58_qstatus ch58_fob ch58_notes ch72_date ch72_eggs
## 1   5/19/22         Y           QR       12               6/2/22         Y
## 2   5/19/22         Y           QR       11               6/2/22         Y
## 3   5/19/22         Y           QR        6               6/2/22         Y
##   ch72_qstatus ch72_fob           ch72_notes ch72_supered ch77_var_total
## 1           QR     14.0                                               11
## 2           QR     12.0                                                2
## 3           QR      8.5 CDB and possible EFB                           8
##   ch77_var_total2 ch77_var_total_check ch77_wash_workers ch77_wash_drones
## 1              11                    0           Missing          Missing
## 2               2                    0           Missing          Missing
## 3               8                    0           Missing          Missing
##   ch77_wash_queens ch77_lbs ch77_lbs2 ch77_lbs_check bottom_board
## 1          Missing    129.4     129.4              0       Normal
## 2          Missing    135.6     135.6              0       Normal
## 3          Missing    107.8     107.8              0       Normal
##   supers_supering_sheet_dontuse chpost_date post_supers_use post_lbs
## 1                             2     6/30/22               2    171.2
## 2                             1     6/30/22               1    153.6
## 3                             1        <NA>              NA       NA
##   post_q_status post_quick_fob  post_notes chpost_var_total chpost_var_total2
## 1            QR             17 cdb w mites               14                14
## 2            QR             11 bad pattern                5                 5
## 3          <NA>             NA                           NA                NA
##   chpost_var_total_check chpost_wash_workers chpost_wash_drones
## 1                      0                 310                  9
## 2                      0                 346                 25
## 3                     NA                  NA                 NA
##   chpost_wash_queens date_decided_remove exp_day_decided_remove
## 1                  0                                         NA
## 2                  0                                         NA
## 3                 NA              6/7/22                     77
##        reason_removal ch77_qstatus ch77_efb    trt_label
## 1                               QR     <NA>     OA Vapor
## 2                               QS     <NA> Amitraz E.C.
## 3 d77 EFB, likely sac           QR Observed Amitraz E.C.
##                    efb_columns efb
## 1                           NA   0
## 2                           NA   0
## 3 d77 EFB, likely sac Observed   1
```

```r
# 22-078
# 22-108
# 22-124

wash77_vec <- pa$ch77_wash_workers
wash77_vec <- as.numeric(wash77_vec[!(wash77_vec == "Missing")])
mean(wash77_vec, na.rm = TRUE) # 358.68
```

```
## [1] 358.6815
```

```r
wash77_vec <- pa$ch77_wash_drones
wash77_vec <- as.numeric(wash77_vec[!(wash77_vec == "Missing")])
mean(wash77_vec, na.rm = TRUE) # 3.59
```

```
## [1] 3.592593
```

```r
# To impute

issues <- c("22-078", "22-108", "22-124")

pa <- pa %>% 
  mutate(
    ch77_wash_workers = ifelse(colony_num %in% issues, 359, ch77_wash_workers),
    ch77_wash_drones = ifelse(colony_num %in% issues, 4, ch77_wash_drones),
    ch77_wash_queens = ifelse(colony_num %in% issues, 0, ch77_wash_queens),
    )
```





Determine queen issues including day 77

```r
# Survival to day 77 - assess based on absence of other issues

# Number of queen events per treatment up to and including day 72

# failed to mate
  # look in column reason_removal
# drone layer
# queenless
  # If QL or DL present in columns ch44_qstatus, ch58_qstatus, ch72_qstatus, 


pa$qstatuses <- paste(
  pa$ch44_qstatus, 
  pa$ch58_qstatus, 
  pa$ch72_qstatus, 
  pa$ch77_qstatus,
  pa$reason_removal
  )

pa$failed <- ifelse(grepl("Failed", pa$reason_removal), 1, 0)
pa$weak <- ifelse(grepl("Weak|Small", pa$reason_removal), 1, 0)
pa$QL <- ifelse(grepl("QL|VS|VQS|VQ", pa$qstatuses), 1, 0)
pa$DL <- ifelse(grepl("DL", pa$qstatuses), 1, 0)

pa <- pa %>% 
  mutate(died = ifelse(failed + weak + QL + DL > 0, 1, 0),
         survived = ifelse(failed + weak + QL + DL == 0, 1, 0)
         )
```

## Add resources data to pa data

```r
pa <- left_join(pa, resources)
```

```
## Joining with `by = join_by(colony_num)`
```

Correct the hive weight based on the number of supers and 12 lbs per super
12.005 was mean (min-max: 11.1-12.8) of 20 supers weighed before putting them on


```r
pa <- pa %>% 
  mutate(post_lbs_C = post_lbs - 12*(post_supers_use-1))
```



# Write an_dat for use in future steps

```r
an_dat <- pa

write.csv(an_dat, "../2_Analysis/data_prepared/an_dat.csv")
```


