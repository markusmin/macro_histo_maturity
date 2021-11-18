# Cross-read tables - maturity codes

library(tidyverse)
library(here)
library(readxl)
library(janitor)

#####----Sablefish----####

# load data
SABL.mat<-read_excel(here("data", "2015_2016 ODFW Sablefish maturity_updated.xlsx"))
SABL.mat <- clean_names(SABL.mat)
SABL.cert<-subset(SABL.mat,certainty==1) #subset only certain samples and those that were staged macroscopically#
SABL.cert <- subset(SABL.cert, !(is.na(maturity_code)))
SABL.cert <- clean_names(SABL.cert)

# Move spent to stage 11 and resting/recovering/regenerating to stage 12
SABL.cert %>% 
  dplyr::rename("x11" = "spent_post_spawn", "x12" = "regenerating_recovering") %>% 
  # rename maturity_code as macro_maturity_code
  dplyr::rename(macro_maturity_code = maturity_code) -> SABL.cert

# Create new field to store most advanced histological stage
SABL.cert %>% 
  mutate(., histo_stage = ifelse(x12 == "Y", "12",
                                 ifelse(x11 == "Y", "11",
                                        # x10 is all NAs so we can ignore
                                        # ifelse(x10 == "Y", "10",
                                               ifelse(x9 == "Y", "9",
                                 ifelse(x8 == "Y", "8",
                                        ifelse(x7 == "Y", "7",
                                               ifelse(x6 == "Y", "6",
                                                      ifelse(x5 == "Y", "5",
                                                             ifelse(x4_2 == "Y", "4_2",
                                                                    ifelse(x4_1 == "Y", "4_1",
                                                                           ifelse(x3 == "Y", "3",
                                                             ifelse(x2 == "Y", "2",
                                                                    ifelse(x1 == "Y", "1", NA))))))))))))) -> SABL.cert

# # Check accuracy by port sampler

# Import experience (from email form Sheryl Flores)
portbio_exp <- data.frame(port_biologist_initials = c("SF", "LG", "CG", "KL", "CR", "NW", "JL", "JSM"), 
                          sabl_exp = c("10+ years", "6mo", "10+ years", "1-2 years", "<1 year", "10+ years", "2 years", "10+ years"),
                          cnry_exp = c("10+ years", "6mo", "10+ years", "1-2 years", "<1 year", "10+ years", "2 years", "10+ years"),
                          arth_exp = c("10+ years", "6mo", "10+ years", "2 years", "<1 year", "10+ years", "2 years", "10+ years"))

SABL.cert %>%
  mutate(bio_macro_agree = ifelse(biological_maturity == 0 & macro_maturity_code %in% c(1,2) | 
                                    biological_maturity == 1 & macro_maturity_code %in% c(3,4,5,6,7,8), 1, 0)) -> SABL_for_comp

# Change biologist initials so that all are capitalized
SABL_for_comp %>% 
  mutate(port_biologist_initials = toupper(port_biologist_initials)) -> SABL_for_comp

SABL_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  # count(port_biologist_initials)
  summarise(n_correct = sum(bio_macro_agree)) -> SABL_correct_by_biologist

SABL_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  count(port_biologist_initials) -> SABL_nstaged_by_biologist

SABL_correct_by_biologist %>% 
  left_join(., SABL_nstaged_by_biologist, by = "port_biologist_initials") %>% 
  mutate(accuracy = n_correct/n) %>% 
  left_join(., portbio_exp, by = "port_biologist_initials") %>% 
  dplyr::select(-c(cnry_exp, arth_exp))-> SABL_accuracy_by_biologist



# Create table of individual stage comparisons

SABL.cert %>% 
  dplyr::select(macro_maturity_code, histo_stage) -> SABL_macro_histo

# Check which histology stages are present
unique(SABL_macro_histo$histo_stage)
# "2"   "7"   "3"   "12"  "8"   "11"  "4_2" "4_1"

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "2")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_2 = Freq) -> sabl_histo_2

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "3")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_3 = Freq) -> sabl_histo_3

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "4_1")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_1 = Freq) -> sabl_histo_4_1

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "4_2")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_2 = Freq) -> sabl_histo_4_2

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "7")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_7 = Freq) -> sabl_histo_7

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "8")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_8 = Freq) -> sabl_histo_8

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "11")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_11 = Freq) -> sabl_histo_11

as.data.frame(table(subset(SABL_macro_histo, histo_stage == "12")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_12 = Freq) -> sabl_histo_12

# Join all histology stages by macro code

# Create data frame of all stages to join to
sabl_histo_v_macro_table <- data.frame(macro_code = c("1", "2", "3", "4", "5", "6", "7"))

# Create dummy data frames for all codes that weren't observed
sabl_histo_1 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_1 = rep(NA, 13))
sabl_histo_5 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_5 = rep(NA, 13))
sabl_histo_6 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_6 = rep(NA, 13))
sabl_histo_9 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_9 = rep(NA, 13))
sabl_histo_10 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_10 = rep(NA, 13))


sabl_histo_v_macro_table %>% 
  left_join(., sabl_histo_1, by = "macro_code") %>% 
  left_join(., sabl_histo_2, by = "macro_code") %>% 
  left_join(., sabl_histo_3, by = "macro_code") %>% 
  left_join(., sabl_histo_4_1, by = "macro_code") %>% 
  left_join(., sabl_histo_4_2, by = "macro_code") %>% 
  left_join(., sabl_histo_5, by = "macro_code") %>% 
  left_join(., sabl_histo_6, by = "macro_code") %>% 
  left_join(., sabl_histo_7, by = "macro_code") %>% 
  left_join(., sabl_histo_8, by = "macro_code") %>%
  left_join(., sabl_histo_9, by = "macro_code") %>% 
  left_join(., sabl_histo_10, by = "macro_code") %>%
  left_join(., sabl_histo_11, by = "macro_code") %>%
  left_join(., sabl_histo_12, by = "macro_code") -> sabl_histo_v_macro_table

# Replace all NAs with zeros
sabl_histo_v_macro_table[is.na(sabl_histo_v_macro_table)] <- 0

#### Calculate percentage of disagreement for binary maturity

# For immature stages: 100-sum(sabl_histo_v_macro_table[i,2:4])/sum(sabl_histo_v_macro_table[i,2:ncol(sabl_histo_v_macro_table)])*100
# This is sum of all immature fish divided by sum of all fish, where i is the macroscopic code

# For mature stages: 100-sum(sabl_histo_v_macro_table[i,5:14])/sum(sabl_histo_v_macro_table[i,2:ncol(sabl_histo_v_macro_table)])*100
# This is sum of all mature fish divided by sum of all fish, where i is the macroscopic code

sabl_histo_v_macro_table %>% 
  mutate(., binary_disagree = ifelse(macro_code == "1", 100-sum(sabl_histo_v_macro_table[1,2:4])/sum(sabl_histo_v_macro_table[1,2:ncol(sabl_histo_v_macro_table)])*100,
                                  ifelse(macro_code == "2", 100-sum(sabl_histo_v_macro_table[2,2:4])/sum(sabl_histo_v_macro_table[2,2:ncol(sabl_histo_v_macro_table)])*100,
                                         ifelse(macro_code == "3", 100-sum(sabl_histo_v_macro_table[3,5:14])/sum(sabl_histo_v_macro_table[3,2:ncol(sabl_histo_v_macro_table)])*100,
                                                ifelse(macro_code == "4", 100-sum(sabl_histo_v_macro_table[4,5:14])/sum(sabl_histo_v_macro_table[4,2:ncol(sabl_histo_v_macro_table)])*100,
                                                       ifelse(macro_code == "5", 100-sum(sabl_histo_v_macro_table[5,5:14])/sum(sabl_histo_v_macro_table[5,2:ncol(sabl_histo_v_macro_table)])*100,
                                                              ifelse(macro_code == "6", 100-sum(sabl_histo_v_macro_table[6,5:14])/sum(sabl_histo_v_macro_table[6,2:ncol(sabl_histo_v_macro_table)])*100,
                                                                     ifelse(macro_code == "7", 100-sum(sabl_histo_v_macro_table[7,5:14])/sum(sabl_histo_v_macro_table[7,2:ncol(sabl_histo_v_macro_table)])*100, NA)))))))) -> sabl_histo_v_macro_table
# NOTE: In the originally submitted manuscript, I put 0% instead of 100% for stage 6. The correct value is 100% (1 out of 1)                                                                            

#### Calculate percentage of correct classification for specific stages


sabl_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = ifelse(macro_code == "1", NA,
                                     ifelse(macro_code == "2", 100-sum(sabl_histo_v_macro_table[2,2:4])/sum(sabl_histo_v_macro_table[2,2:14])*100,
                                            ifelse(macro_code == "3", 100-sum(sabl_histo_v_macro_table[3,5:9])/sum(sabl_histo_v_macro_table[3,2:14])*100,
                                                   ifelse(macro_code == "4", 100-sum(sabl_histo_v_macro_table[4,10:11])/sum(sabl_histo_v_macro_table[4,2:14])*100,
                                                          # Code guide has been updated so that 11 is spent and 12 is resting/recovering
                                                          ifelse(macro_code == "5", 100-sum(sabl_histo_v_macro_table[5,13])/sum(sabl_histo_v_macro_table[5,2:14])*100,
                                                                 ifelse(macro_code == "6", 100-sum(sabl_histo_v_macro_table[6,13])/sum(sabl_histo_v_macro_table[6,2:14])*100,
                                                                        ifelse(macro_code == "7", 100-sum(sabl_histo_v_macro_table[7,14])/sum(sabl_histo_v_macro_table[7,2:14])*100, NA)))))))) -> sabl_histo_v_macro_table

#### Add column for N at each stage
sabl_histo_v_macro_table %>% 
  rowwise() %>% 
  mutate(N = sum(histo_1, histo_2, histo_3, histo_4_1, histo_4_2, histo_5, histo_6, histo_7, histo_8, histo_9, histo_10, histo_11, histo_12)) -> sabl_histo_v_macro_table



#### Calculate overall misclassification rates
sabl_binary_overall_disagree <- sum(sabl_histo_v_macro_table$N*sabl_histo_v_macro_table$binary_disagree/100, na.rm = TRUE)/sum(sabl_histo_v_macro_table$N, na.rm = TRUE)
sabl_specific_overall_disagree <- sum(sabl_histo_v_macro_table$N*sabl_histo_v_macro_table$specific_stage_disagree/100, na.rm = TRUE)/sum(sabl_histo_v_macro_table$N, na.rm = TRUE)

# Add a summary row
sabl_histo_v_macro_table %>% 
  bind_rows(., data.frame(macro_code = "Total", binary_disagree = c(round(sabl_binary_overall_disagree*100,1)), 
                          specific_stage_disagree = c(round(sabl_specific_overall_disagree*100,1)),
                          N = sum(sabl_histo_v_macro_table$N, na.rm = TRUE))) -> sabl_histo_v_macro_table

#### Round columns
sabl_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = round(specific_stage_disagree, 1)) %>% 
  mutate(., binary_disagree = round(binary_disagree, 1)) -> sabl_histo_v_macro_table

# Export
# write.csv(sabl_histo_v_macro_table, here("tables", "sablefish_table.csv"), row.names = FALSE)



#####----Canary rockfish----####

# load data
CNRY.mat <-read.csv(here("data", "2015_2017_ODFW_canary_maturity_reread.csv"))
CNRY.mat <- clean_names(CNRY.mat)
CNRY.cert<-subset(CNRY.mat,certainty==1) #subset only certain samples and those that were staged macroscopically#
CNRY.cert <- subset(CNRY.cert, !(is.na(maturity_code)))

# Move spent to stage 11 and resting/recovering/regenerating to stage 12
CNRY.cert %>% 
  dplyr::rename("x11" = "spent", "x12" = "reorganizing_regenerating") %>% 
  # rename maturity_code as macro_maturity_code
  dplyr::rename(macro_maturity_code = maturity_code) -> CNRY.cert

# Create new field to store most advanced histological stage
CNRY.cert %>% 
  mutate(., histo_stage = ifelse(x12 == "Y", "12",
                                 ifelse(x11 == "Y", "11",
                                        ifelse(x10 == "Y", "10",
                                        ifelse(x9 == "Y", "9",
                                               ifelse(x8 == "Y", "8",
                                                      ifelse(x7 == "Y", "7",
                                                             ifelse(x6 == "Y", "6",
                                                                    ifelse(x5 == "Y", "5",
                                                                           ifelse(x4_2 == "Y", "4_2",
                                                                                  ifelse(x4_1 == "Y", "4_1",
                                                                                         ifelse(x3 == "Y", "3",
                                                                                                ifelse(x2 == "Y", "2",
                                                                                                       ifelse(x1 == "Y", "1", NA)))))))))))))) -> CNRY.cert


# # Check accuracy by port sampler

CNRY.cert %>%
  mutate(bio_macro_agree = ifelse(biological_maturity == 0 & macro_maturity_code %in% c(1,2) | 
                                    biological_maturity == 1 & macro_maturity_code %in% c(3,4,5,6,7,8), 1, 0)) -> CNRY_for_comp

# Change biologist initials so that all are capitalized
CNRY_for_comp %>% 
  mutate(port_biologist_initials = toupper(port_biologist_initials)) -> CNRY_for_comp

# Change JM to JSM
CNRY_for_comp %>% 
  mutate(port_biologist_initials = ifelse(port_biologist_initials == "JM", "JSM", port_biologist_initials)) -> CNRY_for_comp

CNRY_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  # count(port_biologist_initials)
  summarise(n_correct = sum(bio_macro_agree)) -> CNRY_correct_by_biologist

CNRY_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  count(port_biologist_initials) -> CNRY_nstaged_by_biologist

CNRY_correct_by_biologist %>% 
  left_join(., CNRY_nstaged_by_biologist, by = "port_biologist_initials") %>% 
  mutate(accuracy = n_correct/n) %>% 
  left_join(., portbio_exp, by = "port_biologist_initials") %>% 
  dplyr::select(-c(sabl_exp, arth_exp))-> CNRY_accuracy_by_biologist



# Create table of individual stage comparisons

CNRY.cert %>% 
  dplyr::select(macro_maturity_code, histo_stage) -> CNRY_macro_histo

# Check which histology stages are present
unique(CNRY_macro_histo$histo_stage)
# "3"   "4_1" "5"   "4_2" "7"   "6"   "8"   "11"  "10"  "12"  


as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "3")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_3 = Freq) -> cnry_histo_3

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "4_1")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_1 = Freq) -> cnry_histo_4_1

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "4_2")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_2 = Freq) -> cnry_histo_4_2

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "5")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_5 = Freq) -> cnry_histo_5

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "6")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_6 = Freq) -> cnry_histo_6

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "7")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_7 = Freq) -> cnry_histo_7

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "8")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_8 = Freq) -> cnry_histo_8

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "10")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_10 = Freq) -> cnry_histo_10

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "11")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_11 = Freq) -> cnry_histo_11

as.data.frame(table(subset(CNRY_macro_histo, histo_stage == "12")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_12 = Freq) -> cnry_histo_12

# Join all histology stages by macro code

# Create data frame of all stages to join to
cnry_histo_v_macro_table <- data.frame(macro_code = c("1", "2", "3", "4", "5", "6", "7"))

# Create dummy data frames for all codes that weren't observed
cnry_histo_1 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_1 = rep(NA, 13))
cnry_histo_2 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_2 = rep(NA, 13))
cnry_histo_9 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_9 = rep(NA, 13))


cnry_histo_v_macro_table %>% 
  left_join(., cnry_histo_1, by = "macro_code") %>% 
  left_join(., cnry_histo_2, by = "macro_code") %>% 
  left_join(., cnry_histo_3, by = "macro_code") %>% 
  left_join(., cnry_histo_4_1, by = "macro_code") %>% 
  left_join(., cnry_histo_4_2, by = "macro_code") %>% 
  left_join(., cnry_histo_5, by = "macro_code") %>% 
  left_join(., cnry_histo_6, by = "macro_code") %>% 
  left_join(., cnry_histo_7, by = "macro_code") %>% 
  left_join(., cnry_histo_8, by = "macro_code") %>%
  left_join(., cnry_histo_9, by = "macro_code") %>% 
  left_join(., cnry_histo_10, by = "macro_code") %>%
  left_join(., cnry_histo_11, by = "macro_code") %>%
  left_join(., cnry_histo_12, by = "macro_code") -> cnry_histo_v_macro_table

# Replace all NAs with zeros
cnry_histo_v_macro_table[is.na(cnry_histo_v_macro_table)] <- 0

# MAKE ONE MANUAL CHANGE - CHANGE OVARY ID 654 TO SPENT
# This individual was marked as both spent and resting and is the only ovary marked macroscopically as spent. Because
# my code only looks at the most advanced stage and regards resting as more advanced than spent, it calls it spent.
# Change this back to spent.
cnry_histo_v_macro_table[6,13] <- 1
cnry_histo_v_macro_table[6,14] <- 0

#### Calculate percentage of disagreement for binary maturity

# For immature stages: 100-sum(cnry_histo_v_macro_table[i,2:4])/sum(cnry_histo_v_macro_table[i,2:ncol(cnry_histo_v_macro_table)])*100
# This is sum of all immature fish divided by sum of all fish, where i is the macroscopic code

# For mature stages: 100-sum(cnry_histo_v_macro_table[i,5:14])/sum(cnry_histo_v_macro_table[i,2:ncol(cnry_histo_v_macro_table)])*100
# This is sum of all mature fish divided by sum of all fish, where i is the macroscopic code

cnry_histo_v_macro_table %>% 
  mutate(., binary_disagree = ifelse(macro_code == "1", 100-sum(cnry_histo_v_macro_table[1,2:4])/sum(cnry_histo_v_macro_table[1,2:ncol(cnry_histo_v_macro_table)])*100,
                                     ifelse(macro_code == "2", 100-sum(cnry_histo_v_macro_table[2,2:4])/sum(cnry_histo_v_macro_table[2,2:ncol(cnry_histo_v_macro_table)])*100,
                                            ifelse(macro_code == "3", 100-sum(cnry_histo_v_macro_table[3,5:14])/sum(cnry_histo_v_macro_table[3,2:ncol(cnry_histo_v_macro_table)])*100,
                                                   ifelse(macro_code == "4", 100-sum(cnry_histo_v_macro_table[4,5:14])/sum(cnry_histo_v_macro_table[4,2:ncol(cnry_histo_v_macro_table)])*100,
                                                          ifelse(macro_code == "5", 100-sum(cnry_histo_v_macro_table[5,5:14])/sum(cnry_histo_v_macro_table[5,2:ncol(cnry_histo_v_macro_table)])*100,
                                                                 ifelse(macro_code == "6", 100-sum(cnry_histo_v_macro_table[6,5:14])/sum(cnry_histo_v_macro_table[6,2:ncol(cnry_histo_v_macro_table)])*100,
                                                                        ifelse(macro_code == "7", 100-sum(cnry_histo_v_macro_table[7,5:14])/sum(cnry_histo_v_macro_table[7,2:ncol(cnry_histo_v_macro_table)])*100, NA)))))))) -> cnry_histo_v_macro_table
# NOTE: There is a discrepancy with the original manuscript, which has 48% of stage 7 (resting) canary as misread. Here it is 44%. That is because on the
# re-read, one of these individuals was found indeed to be resting (OVARY ID 639). Will have to re-do L50 calculation because of this, but should check with Melissa.

#### Calculate percentage of correct classification for specific stages


cnry_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = ifelse(macro_code == "1", 100-sum(cnry_histo_v_macro_table[1,2:4])/sum(cnry_histo_v_macro_table[1,2:14])*100,
                                             ifelse(macro_code == "2", 100-sum(cnry_histo_v_macro_table[2,2:4])/sum(cnry_histo_v_macro_table[2,2:14])*100,
                                                    ifelse(macro_code == "3", 100-sum(cnry_histo_v_macro_table[3,5:8])/sum(cnry_histo_v_macro_table[3,2:14])*100,
                                                           ifelse(macro_code == "4", 100-sum(cnry_histo_v_macro_table[4,9:11])/sum(cnry_histo_v_macro_table[4,2:14])*100,
                                                                  # Code guide has been updated so that 11 is spent and 12 is resting/recovering
                                                                  ifelse(macro_code == "5", 100-sum(cnry_histo_v_macro_table[5,12])/sum(cnry_histo_v_macro_table[5,2:14])*100,
                                                                         ifelse(macro_code == "6", 100-sum(cnry_histo_v_macro_table[6,13])/sum(cnry_histo_v_macro_table[6,2:14])*100,
                                                                                ifelse(macro_code == "7", 100-sum(cnry_histo_v_macro_table[7,14])/sum(cnry_histo_v_macro_table[7,2:14])*100, NA)))))))) -> cnry_histo_v_macro_table

#### Round columns
cnry_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = round(specific_stage_disagree, 1)) %>% 
  mutate(., binary_disagree = round(binary_disagree, 1)) -> cnry_histo_v_macro_table


#### Add column for N at each stage
cnry_histo_v_macro_table %>% 
  rowwise() %>% 
  mutate(N = sum(histo_1, histo_2, histo_3, histo_4_1, histo_4_2, histo_5, histo_6, histo_7, histo_8, histo_9, histo_10, histo_11, histo_12)) -> cnry_histo_v_macro_table



#### Calculate overall misclassification rates
cnry_binary_overall_disagree <- sum(cnry_histo_v_macro_table$N*cnry_histo_v_macro_table$binary_disagree/100, na.rm = TRUE)/sum(cnry_histo_v_macro_table$N, na.rm = TRUE)
cnry_specific_overall_disagree <- sum(cnry_histo_v_macro_table$N*cnry_histo_v_macro_table$specific_stage_disagree/100, na.rm = TRUE)/sum(cnry_histo_v_macro_table$N, na.rm = TRUE)

# Add a summary row
cnry_histo_v_macro_table %>% 
  bind_rows(., data.frame(macro_code = "Total", binary_disagree = c(round(cnry_binary_overall_disagree*100,1)), 
                      specific_stage_disagree = c(round(cnry_specific_overall_disagree*100,1)),
                      N = sum(cnry_histo_v_macro_table$N, na.rm = TRUE))) -> cnry_histo_v_macro_table

# Export
# write.csv(cnry_histo_v_macro_table, here("tables", "canary_table.csv"), row.names = FALSE)


#####----Arrowtooth Flounder----####

# load data
ARTH.mat<-read_excel(here("data", "2016_2017 ODFW Arrowtooth maturity reread.xlsx"))
ARTH.cert<-subset(ARTH.mat,Certainty==1) #subset only certain samples and those that were staged macroscopically#
ARTH.cert <- clean_names(ARTH.cert)
ARTH.cert <- subset(ARTH.cert, !(is.na(maturity_code)))

# Rename to stages 11 and 12
ARTH.cert %>% 
  dplyr::rename("x11" = "post_spawn", "x12" = "regenerating_recovering") %>% 
  # Rename to macro maturity code
  dplyr::rename(macro_maturity_code = maturity_code) -> ARTH.cert

# Create new field to store most advanced histological stage
ARTH.cert %>% 
  mutate(., histo_stage = ifelse(x12 == "Y", "12",
                                 ifelse(x11 == "Y", "11",
                                        # Stage 10 is NA for ARTH, so can ignore
                                        # ifelse(x10 == "Y", "10",
                                               ifelse(x9 == "Y", "9",
                                                      ifelse(x8 == "Y", "8",
                                                             ifelse(x7 == "Y", "7",
                                                                    ifelse(x6 == "Y", "6",
                                                                           ifelse(x5 == "Y", "5",
                                                                                  ifelse(x4_2 == "Y", "4_2",
                                                                                         ifelse(x4_1 == "Y", "4_1",
                                                                                                ifelse(x3 == "Y", "3",
                                                                                                       ifelse(x2 == "Y", "2",
                                                                                                              ifelse(x1 == "Y", "1", NA))))))))))))) -> ARTH.cert

# # Check accuracy by port sampler

# Change "port_sampler" to "port_biologist_initials
ARTH.cert %>%
  dplyr::rename(port_biologist_initials = port_sampler) %>% 
  mutate(bio_macro_agree = ifelse(biological_maturity == 0 & macro_maturity_code %in% c(1,2) | 
                                    biological_maturity == 1 & macro_maturity_code %in% c(3,4,5,6,7,8), 1, 0)) -> ARTH_for_comp

# Change biologist initials so that all are capitalized
ARTH_for_comp %>% 
  mutate(port_biologist_initials = toupper(port_biologist_initials)) -> ARTH_for_comp

# Change JM to JSM
ARTH_for_comp %>%
  mutate(port_biologist_initials = ifelse(port_biologist_initials == "JM", "JSM", port_biologist_initials)) -> ARTH_for_comp

ARTH_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  # count(port_biologist_initials)
  summarise(n_correct = sum(bio_macro_agree)) -> ARTH_correct_by_biologist

ARTH_for_comp %>% 
  group_by(port_biologist_initials) %>% 
  count(port_biologist_initials) -> ARTH_nstaged_by_biologist

ARTH_correct_by_biologist %>% 
  left_join(., ARTH_nstaged_by_biologist, by = "port_biologist_initials") %>% 
  mutate(accuracy = n_correct/n) %>% 
  left_join(., portbio_exp, by = "port_biologist_initials") %>% 
  dplyr::select(-c(sabl_exp, cnry_exp))-> ARTH_accuracy_by_biologist



# Create table of individual stage comparisons

ARTH.cert %>% 
  dplyr::select(macro_maturity_code, histo_stage) -> ARTH_macro_histo

# Check which histology stages are present
unique(ARTH_macro_histo$histo_stage)
# "2"   "3"   "7"   "5"   "4_1" "8"   "11"  "4_2" "6"   "12"  "9"

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "2")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_2 = Freq) -> arth_histo_2

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "3")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_3 = Freq) -> arth_histo_3

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "4_1")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_1 = Freq) -> arth_histo_4_1

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "4_2")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_4_2 = Freq) -> arth_histo_4_2

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "5")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_5 = Freq) -> arth_histo_5

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "6")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_6 = Freq) -> arth_histo_6

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "7")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_7 = Freq) -> arth_histo_7

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "8")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_8 = Freq) -> arth_histo_8

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "9")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_9 = Freq) -> arth_histo_9

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "11")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_11 = Freq) -> arth_histo_11

as.data.frame(table(subset(ARTH_macro_histo, histo_stage == "12")$macro_maturity_code)) %>% 
  dplyr::rename(macro_code = Var1, histo_12 = Freq) -> arth_histo_12

# Join all histology stages by macro code

# Create data frame of all stages to join to
arth_histo_v_macro_table <- data.frame(macro_code = c("1", "2", "3", "4", "5", "6", "7"))

# Create dummy data frames for all codes that weren't observed
arth_histo_1 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_1 = rep(NA, 13))
arth_histo_10 <- data.frame(macro_code = c("1", "2", "3", "4_1", "4_2", "5", "6", "7", "8", "9", "10", "11", "12"), histo_10 = rep(NA, 13))


arth_histo_v_macro_table %>% 
  left_join(., arth_histo_1, by = "macro_code") %>% 
  left_join(., arth_histo_2, by = "macro_code") %>% 
  left_join(., arth_histo_3, by = "macro_code") %>% 
  left_join(., arth_histo_4_1, by = "macro_code") %>% 
  left_join(., arth_histo_4_2, by = "macro_code") %>% 
  left_join(., arth_histo_5, by = "macro_code") %>% 
  left_join(., arth_histo_6, by = "macro_code") %>% 
  left_join(., arth_histo_7, by = "macro_code") %>% 
  left_join(., arth_histo_8, by = "macro_code") %>%
  left_join(., arth_histo_9, by = "macro_code") %>% 
  left_join(., arth_histo_10, by = "macro_code") %>%
  left_join(., arth_histo_11, by = "macro_code") %>%
  left_join(., arth_histo_12, by = "macro_code") -> arth_histo_v_macro_table

# Replace all NAs with zeros
arth_histo_v_macro_table[is.na(arth_histo_v_macro_table)] <- 0

# MAKE ONE MANUAL CHANGE - CHANGE OVARY ID 355 TO stage 9, from 11
# This individual was marked as both spent and stage 9. Stage 9 would make it a correct classification, since it was a code 5.
arth_histo_v_macro_table[5,11] <- 2
arth_histo_v_macro_table[5,13] <- 0

# MAKE TWO MANUAL CHANGES - CHANGE OVARY ID 1179 AND 1195 TO SPENT
# This individual was marked as both spent and resting and is the only ovary marked macroscopically as spent. Because
# my code only looks at the most advanced stage and regards resting as more advanced than spent, it calls it spent.
# Change this back to spent.
arth_histo_v_macro_table[6,13] <- 10
arth_histo_v_macro_table[6,14] <- 0

# MAKE THREE MANUAL CHANGES - CHANGE OVARY ID 1202, 1203, 1211 TO RESTING
# these are odd because the histology notes state "spent, recovering", but they are only marked as spent and not recovering. Change these to recovering.
arth_histo_v_macro_table[7,14] <- 7
arth_histo_v_macro_table[7,13] <- 0

#### Calculate percentage of disagreement for binary maturity

# For immature stages: 100-sum(arth_histo_v_macro_table[i,2:4])/sum(arth_histo_v_macro_table[i,2:ncol(arth_histo_v_macro_table)])*100
# This is sum of all immature fish divided by sum of all fish, where i is the macroscopic code

# For mature stages: 100-sum(arth_histo_v_macro_table[i,5:14])/sum(arth_histo_v_macro_table[i,2:ncol(arth_histo_v_macro_table)])*100
# This is sum of all mature fish divided by sum of all fish, where i is the macroscopic code

arth_histo_v_macro_table %>% 
  mutate(., binary_disagree = ifelse(macro_code == "1", 100-sum(arth_histo_v_macro_table[1,2:4])/sum(arth_histo_v_macro_table[1,2:ncol(arth_histo_v_macro_table)])*100,
                                     ifelse(macro_code == "2", 100-sum(arth_histo_v_macro_table[2,2:4])/sum(arth_histo_v_macro_table[2,2:ncol(arth_histo_v_macro_table)])*100,
                                            ifelse(macro_code == "3", 100-sum(arth_histo_v_macro_table[3,5:14])/sum(arth_histo_v_macro_table[3,2:ncol(arth_histo_v_macro_table)])*100,
                                                   ifelse(macro_code == "4", 100-sum(arth_histo_v_macro_table[4,5:14])/sum(arth_histo_v_macro_table[4,2:ncol(arth_histo_v_macro_table)])*100,
                                                          ifelse(macro_code == "5", 100-sum(arth_histo_v_macro_table[5,5:14])/sum(arth_histo_v_macro_table[5,2:ncol(arth_histo_v_macro_table)])*100,
                                                                 ifelse(macro_code == "6", 100-sum(arth_histo_v_macro_table[6,5:14])/sum(arth_histo_v_macro_table[6,2:ncol(arth_histo_v_macro_table)])*100,
                                                                        ifelse(macro_code == "7", 100-sum(arth_histo_v_macro_table[7,5:14])/sum(arth_histo_v_macro_table[7,2:ncol(arth_histo_v_macro_table)])*100, NA)))))))) -> arth_histo_v_macro_table

#### Calculate percentage of correct classification for specific stages

# Some confusion with how stages match up, since we have overlap. Check with Melissa.
arth_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = ifelse(macro_code == "1", 100-sum(arth_histo_v_macro_table[1,2:4])/sum(arth_histo_v_macro_table[1,2:14])*100,
                                             # this line is irrelevant because code 2 isn't used for arrowtooth anymore
                                             ifelse(macro_code == "2", 100-sum(arth_histo_v_macro_table[2,2:4])/sum(arth_histo_v_macro_table[2,2:14])*100,
                                                    # This corresponds to stages 4.1, 4.2, and 5. 
                                                    ifelse(macro_code == "3", 100-sum(arth_histo_v_macro_table[3,5:7])/sum(arth_histo_v_macro_table[3,2:14])*100,
                                                           # This corresponds to stages 6 and 7. 
                                                           ifelse(macro_code == "4", 100-sum(arth_histo_v_macro_table[4,8:9])/sum(arth_histo_v_macro_table[4,2:14])*100,
                                                                  # corresponds to stages 8, and 9 (10 is only for rockfish, since it's eyed larvae)
                                                                  ifelse(macro_code == "5", 100-sum(arth_histo_v_macro_table[5,10:11])/sum(arth_histo_v_macro_table[5,2:14])*100,
                                                                         # Code guide has been updated so that 11 is spent and 12 is resting/recovering
                                                                         ifelse(macro_code == "6", 100-sum(arth_histo_v_macro_table[6,13])/sum(arth_histo_v_macro_table[6,2:14])*100,
                                                                                ifelse(macro_code == "7", 100-sum(arth_histo_v_macro_table[7,14])/sum(arth_histo_v_macro_table[7,2:14])*100, NA)))))))) -> arth_histo_v_macro_table

#### Round columns
arth_histo_v_macro_table %>% 
  mutate(., specific_stage_disagree = round(specific_stage_disagree, 1)) %>% 
  mutate(., binary_disagree = round(binary_disagree, 1)) -> arth_histo_v_macro_table

#### Add column for N at each stage
arth_histo_v_macro_table %>% 
  rowwise() %>% 
  mutate(N = sum(histo_1, histo_2, histo_3, histo_4_1, histo_4_2, histo_5, histo_6, histo_7, histo_8, histo_9, histo_10, histo_11, histo_12)) -> arth_histo_v_macro_table



#### Calculate overall misclassification rates
arth_binary_overall_disagree <- sum(arth_histo_v_macro_table$N*arth_histo_v_macro_table$binary_disagree/100, na.rm = TRUE)/sum(arth_histo_v_macro_table$N, na.rm = TRUE)
arth_specific_overall_disagree <- sum(arth_histo_v_macro_table$N*arth_histo_v_macro_table$specific_stage_disagree/100, na.rm = TRUE)/sum(arth_histo_v_macro_table$N, na.rm = TRUE)

# Add a summary row
arth_histo_v_macro_table %>% 
  bind_rows(., data.frame(macro_code = "Total", binary_disagree = c(round(arth_binary_overall_disagree*100,1)), 
                          specific_stage_disagree = c(round(arth_specific_overall_disagree*100,1)),
                          N = sum(arth_histo_v_macro_table$N, na.rm = TRUE))) -> arth_histo_v_macro_table

# Export
# write.csv(arth_histo_v_macro_table, here("tables", "arrowtooth_table.csv"), row.names = FALSE)

# Check accuracy for each species
ARTH_accuracy_by_biologist
CNRY_accuracy_by_biologist
SABL_accuracy_by_biologist


#### Run Fisher's Exact Test ####

# in order to do this, we need to group samplers by amount of experience. (because sample sizes are so low)
# Let's do <= 2, and > 2.

# Group samplers, convert to matrix, run test

# ARTH
ARTH_accuracy_by_biologist %>% 
  mutate(exp_cat = ifelse(arth_exp == "10+ years", ">2 years", "<=2 years")) %>% 
  group_by(exp_cat) %>% 
  summarise(n_correct = sum(n_correct), n = sum(n)) %>% 
  mutate(n_incorrect = n - n_correct) %>% 
  dplyr::select(-c(exp_cat, n)) %>% 
  t() %>% 
  as.matrix() -> ARTH_contingency_matrix

fisher.test(ARTH_contingency_matrix)

# CNRY
CNRY_accuracy_by_biologist %>% 
  mutate(exp_cat = ifelse(cnry_exp == "10+ years", ">2 years", "<=2 years")) %>% 
  group_by(exp_cat) %>% 
  summarise(n_correct = sum(n_correct), n = sum(n)) %>% 
  mutate(n_incorrect = n - n_correct) %>% 
  dplyr::select(-c(exp_cat, n)) %>% 
  t() %>% 
  as.matrix() -> CNRY_contingency_matrix

fisher.test(CNRY_contingency_matrix)

# SABL
SABL_accuracy_by_biologist %>% 
  mutate(exp_cat = ifelse(sabl_exp == "10+ years", ">2 years", "<=2 years")) %>% 
  group_by(exp_cat) %>% 
  summarise(n_correct = sum(n_correct), n = sum(n)) %>% 
  mutate(n_incorrect = n - n_correct) %>% 
  dplyr::select(-c(exp_cat, n)) %>% 
  t() %>% 
  as.matrix() -> sabl_contingency_matrix

fisher.test(sabl_contingency_matrix)



# Export tables - need to merge into one Excel file to share with Sheryl
write.csv(ARTH_for_comp, here("tables", "for_sheryl", "arrowtooth_accuracy.csv"), row.names = FALSE)
write.csv(CNRY_for_comp, here("tables", "for_sheryl", "canary_accuracy.csv"), row.names = FALSE)
write.csv(SABL_for_comp, here("tables", "for_sheryl", "sablefish_accuracy.csv"), row.names = FALSE)
write.csv(ARTH_accuracy_by_biologist, here("tables", "for_sheryl", "arrowtooth_accuracy_by_portbio.csv"), row.names = FALSE)
write.csv(CNRY_accuracy_by_biologist, here("tables", "for_sheryl", "canary_accuracy_by_portbio.csv"), row.names = FALSE)
write.csv(SABL_accuracy_by_biologist, here("tables", "for_sheryl", "sablefish_accuracy_by_portbio.csv"), row.names = FALSE)
