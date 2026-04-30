library(rjags)
library(dplyr)
library(tidyverse)

sba <- read.csv("Spring 26/STAT-431/final-project/sba_loans.csv")

sba_clean <- sba %>%
  filter(
    MIS_Status != "",
    RevLineCr %in% c("Y", "N"),
    LowDoc %in% c("Y", "N"),
    ApprovalFY <= "2010",
    
    Bank != "",
    
    UrbanRural != 0,
    substr(NAICS, 1, 2) != "0",
  ) %>%
  mutate(
    # Convert currency columns to numeric
    DisbursementGross_num = as.numeric(gsub("[\\$,]", "", DisbursementGross)),
    ChgOffPrinGr_num = as.numeric(gsub("[\\$,]", "", ChgOffPrinGr)),
    GrAppv_num = as.numeric(gsub("[\\$,]", "", GrAppv)),
    SBA_Appv_num = as.numeric(gsub("[\\$,]", "", SBA_Appv)),
    BalanceGross_num = as.numeric(gsub("[\\$,]", "", BalanceGross)),
    
    # Binary response: 1 = Paid in Full, 0 = Charged Off
    PaidInFull = ifelse(MIS_Status == "P I F", 1, 0),
    
    # Portion of loan guaranteed by SBA
    SBA_Portion = as.numeric(SBA_Appv_num / GrAppv_num),
    
    # Categorical variables
    NAICS_sector = factor(substr(NAICS, 1, 2)),
    IsFranchise = ifelse(FranchiseCode %in% c("00000", "00001", 0, 1), "No", "Yes"),
    NewExist_f = case_when(
      NewExist == 1 ~ "Existing",
      NewExist == 2 ~ "New",
      TRUE ~ NA_character_
    ) %>% factor(),
    UrbanRural_f = factor(UrbanRural, levels = c(0, 1, 2),
                          labels = c("Undefined", "Urban", "Rural")),
    RevLineCr_f = factor(RevLineCr, levels = c("Y", "N"), labels = c("Yes", "No")),
    LowDoc_f = factor(LowDoc, levels = c("Y", "N"), labels = c("Yes", "No")),
    GFC_Period = ifelse(ApprovalFY >= 2008, "GFC", "Pre-GFC")
  ) %>% 
  na.omit()

nrow(sba)
nrow(sba_clean)

idx <- sample(1:nrow(sba_clean), size = 50000, replace = FALSE)

write.csv(sba_clean, "Spring 26/STAT-431/final-project/sba_clean_full.csv", row.names = FALSE)
write.csv(sba_clean[idx,], "Spring 26/STAT-431/final-project/sba_clean_50k.csv", row.names = FALSE)

