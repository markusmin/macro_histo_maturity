# Prepare final data files for maturity agreement/maturity analyses

# For this, we have to combine the original data files and the re-reads that Melissa did that for resting and spent ovaries.

## Load libraries
library(tidyverse)
library(readxl)

##### CANARY ROCKFISH #####
# Load original file
CNRY.mat<-read.csv(here("data", "canary_maturity.csv"))
CNRY.cert<-subset(CNRY.mat,Certainty==1) 

# Load re-reads
CNRY.reread <-read.csv(here("data", "2015_2017 ODFW canary maturity_updated.csv"))
CNRY.reread.cert <- subset(CNRY.reread, `Certainty.of.reader..1.0.` == 1)
# Keep only new fields (spent and regenerating fields) and index (Ovary ID)
CNRY.reread.cert %>% 
  dplyr::select("Ovary.ID.", "Spent", "Reorganizing.Regenerating") %>% 
  dplyr::rename("X11" = "Spent", "X12" = "Reorganizing.Regenerating", "Ovary_ID." = "Ovary.ID.") -> CNRY.reread.subset

# Join to old data
CNRY.cert %>% 
  left_join(., CNRY.reread.subset, by = "Ovary_ID.") %>% 
  # Move columns to be in line with other histo stages
  relocate(X11, .after = X10) %>% 
  relocate(X12, .after = X11)-> CNRY.cert

# Change column names to remove X from numeric names
names(CNRY.cert) <- sub("X", "", names(CNRY.cert))

# Export data
write.csv(CNRY.cert, here("data", "canary_maturity_final.csv"), row.names = FALSE)

##### SABLEFISH #####
# Load original file
SABL.mat<-read.csv(here("data", "sablefish_maturity.csv"))
SABL.cert<-subset(SABL.mat,Certainty==1) 

# Load re-reads
SABL.reread<-read_excel(here("data", "2015_2016 ODFW Sablefish maturity_updated.xlsx"))
SABL.reread.cert<-subset(SABL.reread,Certainty==1 & !(is.na(`Maturity Code`)))
# Keep only new fields (spent and regenerating fields) and index (Ovary ID)
SABL.reread.cert %>% 
  dplyr::select("Ovary_ID#", "Spent/Post_spawn", "Regenerating/recovering") %>% 
  dplyr::rename("X11" = "Spent/Post_spawn", "X12" = "Regenerating/recovering" , "Ovary_ID" = "Ovary_ID#") -> SABL.reread.subset

# Join to old data
SABL.cert %>% 
  left_join(., SABL.reread.subset, by = "Ovary_ID") %>% 
  # Add column of NAs for stage 10
  mutate(X10 = NA) %>% 
  # Move columns to be in line with other histo stages
  relocate(X10, .after = X9) %>% 
  relocate(X11, .after = X10) %>% 
  relocate(X12, .after = X11)-> SABL.cert

# Change column names to remove X from numeric names
names(SABL.cert) <- sub("X", "", names(SABL.cert))

# Export data
write.csv(SABL.cert, here("data", "sablefish_maturity_final.csv"), row.names = FALSE)


##### ARROWTOOTH FLOUNDER #####
# Load original file
ARTH.mat<-read.csv(here("data", "arrowtooth_maturity.csv"))
ARTH.cert<-subset(ARTH.mat,Certainty==1) 

# Load re-reads
ARTH.reread<-read_excel(here("data", "2016_2017 ODFW Arrowtooth maturity reread.xlsx"))
ARTH.reread.cert<-subset(ARTH.reread,Certainty==1) #subset only certain samples and those that were staged macroscopically#

# Keep only new fields (spent and regenerating fields) and index (Ovary ID)
ARTH.reread.cert %>% 
  dplyr::select("Ovary_Id", "Post_spawn", "Regenerating/Recovering") %>% 
  dplyr::rename("X11" = "Post_spawn", "X12" = "Regenerating/Recovering") -> ARTH.reread.subset

# Join to old data
ARTH.cert %>% 
  left_join(., ARTH.reread.subset, by = "Ovary_Id") %>% 
  # Move columns to be in line with other histo stages
  relocate(X11, .after = X10) %>% 
  relocate(X12, .after = X11)-> ARTH.cert

# Change column names to remove X from numeric names
names(ARTH.cert) <- sub("X", "", names(ARTH.cert))

# Export data
write.csv(ARTH.cert, here("data", "arrowtooth_maturity_final.csv"), row.names = FALSE)


