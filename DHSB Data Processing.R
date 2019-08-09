#####Digital Health-Seeking Behaviors
#####> Data Processing and Variable Transformations

#This code assumes access to an intact data set with columns from baseline and 6 month ACASI assessments
#> and medical chart data (Viral Suppression, HIV visits, HIV Diagnosis Year).
#> Note that the HIV Visits (CAREHV06_MCD) variable is a binary variable indicating whether a participant had 
#> HIV care visits within 6 months of the day of baseline assessment.

#####Load data set
load("acasi_mcd.RData")

#####Load required packages
library(tidyverse)
library(lubridate)

#####Creation of new variables
#####> Collapse existing demographic variables and create scales

dhsb <- acasi_mcd %>%
  mutate(
    #Variable transformations
    #> Procedure:
    ##> 1) Create (if necessary) and re-level factors, collapse levels into fewer options (RC = Recode)
    ##> 2) Create dummy variables (RCD = Recode Dummy, RCR = Recode Reference for one-hot coding)
    ###>     Treated 'Refused to answer' or 'Don`t know` as separate category
    ##> 3) Change 'Refused to answer' or 'Skipped' to NA for continuous variables
    ##> 4) Make other transformations to continuous variables (sum scales)
    
    #> Site
    SITE1 = factor(SITE1, levels = c(
      "CBW", "FRI", "NYSDA", "HBHC", "MHS", "PSU", "PFC", "SFDPH", "WFU", "WUSL"
    )),
    SITE_RC = fct_recode(as.factor(SITE1),
                         "C. Christi" = "CBW", "L.A." = "FRI", 
                         "N.Y." = "NYSDA", "Chicago" = "HBHC", 
                         "Cleveland"  = "MHS", "Hershey" = "PSU", 
                         "Phil." = "PFC", "S.F." = "SFDPH", 
                         "W.-Salem"  = "WFU", "St. Louis" = "WUSL"),
    SITE_RCR_CBW   = if_else(SITE1 == "CBW", 1, 0),
    SITE_RCD_FRI   = if_else(SITE1 == "FRI", 1, 0),
    SITE_RCD_NYSDA = if_else(SITE1 == "NYSDA", 1, 0),
    SITE_RCD_HBHC  = if_else(SITE1 == "HBHC", 1, 0),
    SITE_RCD_MHS   = if_else(SITE1 == "MHS", 1, 0),
    SITE_RCD_PSU   = if_else(SITE1 == "PSU", 1, 0),
    SITE_RCD_PFC   = if_else(SITE1 == "PFC", 1, 0),
    SITE_RCD_SFDPH = if_else(SITE1 == "SFDPH", 1, 0),
    SITE_RCD_WFU   = if_else(SITE1 == "WFU", 1, 0),
    SITE_RCD_WUSL  = if_else(SITE1 == "WUSL", 1, 0),
    #> Survey language
    surveylanguage_RCR_Span = if_else(surveylanguage == "Spanish", 1, 0), ##> Reference (not included in final set)
    surveylanguage_RCD_Eng  = if_else(surveylanguage == "English", 1, 0),
    #> Age
    AGE_RC = as.integer(floor((ymd(TODAY) - ymd(DOB)) / 365.25)),
    #> Ethnicity & Race
    ##> Non-mutually exclusive variable RACE combined with LATINO for new mutually exclusive RACE_RC variable
    RACE_RC = case_when(LATINO == 1 ~ "Latino",
                        RACEC == 1 ~ "Black, Not Latino",
                        RACEE == 1 ~ "White, Not Latino",
                        LATINO == 8 & RACE == 8 ~ "Refuse to answer", #None refused to answer
                        !is.na(RACERECODE) ~ RACERECODE,
                        TRUE ~ "Other race"),
    RACE_RCR_White    = if_else(RACE_RC == "White, Not Latino", 1, 0), ##> Reference (not included in final set)
    RACE_RCD_Latino   = if_else(RACE_RC == "Latino", 1, 0),
    RACE_RCD_Black    = if_else(RACE_RC == "Black, Not Latino", 1, 0),
    RACE_RCD_Other    = if_else(RACE_RC == "Other race", 1, 0),
    RACE_RCD_Missing  = if_else(RACE_RC == "Refuse to answer", 1, 0),
    #> Gender Identity
    GENDER_RC = fct_recode(as.factor(GENDER),
                           "Male (cis man)"     = "1", 
                           "Female (cis woman)" = "2",
                           "Trans-identified"   = "3", 
                           "Trans-identified"   = "4",
                           "Other gender"       = "5", 
                           "Other gender"       = "6",
                           "Refuse to answer"   = "8"), #None refused to answer
    GENDER_RC = if_else(!is.na(GENDERRECODE), 
                        GENDERRECODE, as.character(GENDER_RC)),
    GENDER_RCR_Male    = if_else(GENDER_RC == "Male (cis man)", 1, 0), ##> Reference (not included in final set)
    GENDER_RCD_Female  = if_else(GENDER_RC == "Female (cis woman)", 1, 0),
    GENDER_RCD_Trans   = if_else(GENDER_RC == "Trans-identified", 1, 0),
    GENDER_RCD_Other   = if_else(GENDER_RC == "Other gender", 1, 0),
    GENDER_RCD_Missing = if_else(GENDER_RC == "Refuse to answer", 1, 0),
    #> Sexual Orientation
    ORIENT_RC = fct_recode(as.factor(ORIENT),
                           "Straight"          = "1", 
                           "Gay or lesbian"    = "2", 
                           "Bisexual"          = "3",
                           "Other orientation" = "4", 
                           "Other orientation" = "5", 
                           "Other orientation" = "7",
                           "Refuse to answer"  = "8"), #None refused to answer
    ORIENT_RC = if_else(!is.na(ORIENTRECODE), 
                        ORIENTRECODE, as.character(ORIENT_RC)),
    ORIENT_RCR_Straight = if_else(ORIENT_RC == "Straight", 1, 0), ##> Reference (not included in final set)
    ORIENT_RCD_Gay      = if_else(ORIENT_RC == "Gay or lesbian", 1, 0),
    ORIENT_RCD_Bi       = if_else(ORIENT_RC == "Bisexual", 1, 0),
    ORIENT_RCD_Other    = if_else(ORIENT_RC == "Other orientation", 1, 0),
    ORIENT_RCD_Missing  = if_else(ORIENT_RC == "Refuse to answer", 1, 0),
    #> Education
    GRADE_RC = fct_recode(as.factor(GRADE),
                          "High school, equivalent or less"         = "1", 
                          "High school, equivalent or less"         = "2", 
                          "High school, equivalent or less"         = "3", 
                          "Some post-K12"                           = "4", 
                          "College graduate or trade certification" = "5", 
                          "College graduate or trade certification" = "6",
                          "College graduate or trade certification" = "7", 
                          "Refuse to answer"                        = "8"), #None refused to answer
    GRADE_RCR_HS      = if_else(GRADE_RC == "High school, equivalent or less", 1, 0), ##> Reference (not included in final set)
    GRADE_RCD_PostK   = if_else(GRADE_RC == "Some post-K12", 1, 0),
    GRADE_RCD_Grad    = if_else(GRADE_RC == "College graduate or trade certification", 1, 0),
    GRADE_RCD_Missing = if_else(GRADE_RC == "Refuse to answer", 1, 0),
    #> Income
    MONEY_RC = ifelse(MONEY %in% c(99997, 99998), NA, MONEY),
    MONEY_RC_Log = log(MONEY_RC + 1),
    MONEY_RC_Cat = case_when(
      !is.na(MONEY_RC_Log) & MONEY_RC_Log == 0 ~ "Zero",
      !is.na(MONEY_RC_Log) & MONEY_RC_Log > 0 & 
        MONEY_RC_Log < median(MONEY_RC_Log[MONEY_RC_Log != 0], na.rm = TRUE) ~ "Low",
      !is.na(MONEY_RC_Log) & 
        MONEY_RC_Log >= median(MONEY_RC_Log[MONEY_RC_Log != 0], na.rm = TRUE) ~ "High",
      is.na(MONEY_RC_Log) ~ "Don't know"
    ),
    MONEY_RCR_Zero     = if_else(MONEY_RC_Cat == "Zero", 1, 0), ##> Reference (not included in final set)
    MONEY_RCD_Low      = if_else(MONEY_RC_Cat == "Low", 1, 0),
    MONEY_RCD_High     = if_else(MONEY_RC_Cat == "High", 1, 0),
    MONEY_RCD_DontKnow = if_else(MONEY_RC_Cat == "Don't know", 1, 0),
    #> Residence, Last 7 Days
    STAY7D_RC = fct_recode(as.factor(STAY7D),
                           "Stable housing"   =  "1", 
                           "Unstable housing" =  "2",
                           "Unstable housing" =  "3", 
                           "Unstable housing" =  "4",
                           "Unstable housing" =  "5", 
                           "Unstable housing" =  "6",
                           "Unstable housing" =  "7", 
                           "Unstable housing" =  "8",
                           "Unstable housing" =  "9", 
                           "Unstable housing" = "10",
                           "Unstable housing" = "11", 
                           "Other residence"  = "12",
                           "Refuse to answer" = "98"), #None refused to answer
    STAY7D_RC = if_else(!is.na(STAYRECODE), 
                        STAYRECODE, as.character(STAY7D_RC)),
    STAY7D_RCR_Unstable    = if_else(STAY7D_RC == "Unstable housing", 1, 0), ##> Reference (not included in final set)
    STAY7D_RCD_Stable      = if_else(STAY7D_RC == "Stable housing", 1, 0),
    STAY7D_RCD_Missing     = if_else(STAY7D_RC == "Refuse to answer", 1, 0),
    #> HIV History
    TIMESINCEHIV = case_when(
      DIAGHIV != 2099 ~ year(TODAY) - DIAGHIV,
      DIAGHIV == 2099 ~ as.numeric(AGE_RC)
    ),
    BORNHIV_MCD = case_when(
      !is.na(HIVDiagnosisYear_MCD) & DOBY == HIVDiagnosisYear_MCD ~ 1,
      !is.na(HIVDiagnosisYear_MCD) & DOBY != HIVDiagnosisYear_MCD ~ 0
    ),
    TIMESINCEHIV_MCD = year(TODAY) - HIVDiagnosisYear_MCD, #Includes those born with HIV
    #> Viral Suppression
    ViralSupp_MCD_RC = case_when(
      !is.na(ViralSupp_MCD) & ViralSupp_MCD == 1 ~ "Suppressed",
      !is.na(ViralSupp_MCD) & ViralSupp_MCD == 0 ~ "Not suppressed",
      is.na(ViralSupp_MCD) ~ "Missing"
    ),
    ViralSupp_RCR_Unsuppressed = if_else(ViralSupp_MCD_RC == "Not suppressed", 1, 0), ##> Reference (not included in final set)
    ViralSupp_RCD_Suppressed   = if_else(ViralSupp_MCD_RC == "Suppressed", 1, 0),
    ViralSupp_RCD_Missing      = if_else(ViralSupp_MCD_RC == "Missing", 1, 0),
    #> Insurance
    INSURE_RC = case_when(INSUREA == 1 | INSURE == 97 ~ "Not insured",
                          # INSURE == 97 ~ "Don't know",
                          INSURE == 98 ~ "Refuse to answer",
                          INSURE == 99 ~ "Skipped", #None refused to answer
                          INSUREB == 1 | INSUREC == 1 | INSURED == 1 |
                            INSUREE == 1 | INSUREF == 1 | INSUREG == 1 ~ "Insured",
                          TRUE ~ INSURERECODE),
    INSURE_RCR_Uninsured = if_else(INSURE_RC == "Not insured", 1, 0), ##> Reference (not included in final set)
    INSURE_RCD_Insured   = if_else(INSURE_RC == "Insured", 1, 0),
    INSURE_RCD_Unknown   = if_else(INSURE_RC == "Don't know", 1, 0),
    INSURE_RCD_Missing   = if_else(INSURE_RC == "Refuse to answer" |
                                   INSURE_RC == "Skipped", 1, 0),
    #> Medical Care
    CARED6_RC = case_when(
      CARED6 == 0 ~ "No",
      CARED6 > 0 & CARED6 < 998 ~ "Yes",
      CARED6 >= 998 ~ "Missing"
    ),
    CARED6_RCR_No      = if_else(CARED6_RC == "No", 1, 0), ##> Reference (not included in final set)
    CARED6_RCD_Yes     = if_else(CARED6_RC == "Yes", 1, 0),
    CARED6_RCD_Missing = if_else(CARED6_RC == "Missing", 1, 0),
    CAREHV06_RC = case_when(
      CARELHIV == 1 & CAREHV06 > 0 ~ "Yes",
      CARELHIV == 0 | CAREHV06 == 0 ~ "No",
      CARELHIV == 8 ~ "Missing"
    ), #None refused
    # CAREHV06_RCD_Yes = if_else(CAREHV06 > 0 & CAREHV06 <= 99, 1, 0),
    # CAREHV06_RCD_Missing = if_else(CAREHV06 == 998 | CAREHV06 == 999, 1, 0),
    CAREHV06_MCD_RC = case_when(
      !is.na(CAREHV06_MCD) & CAREHV06_MCD > 0 ~ "Yes",
      !is.na(CAREHV06_MCD) & CAREHV06_MCD == 0 ~ "No",
      is.na(CAREHV06_MCD) ~ "Missing"
    ),
    CAREHV06_MCD_RCR_No      = if_else(CAREHV06_MCD_RC == "No", 1, 0), ##> Reference (not included in final set)
    CAREHV06_MCD_RCD_Yes     = if_else(CAREHV06_MCD_RC == "Yes", 1, 0),
    CAREHV06_MCD_RCD_Missing = if_else(CAREHV06_MCD_RC == "Missing", 1, 0),
    #> ART Usage
    ARTNOW_RC = case_when(
      ARTNOW == 1 ~ "Yes",
      ARTNOW == 0 ~ "No",
      TRUE ~ "Missing"
    ),
    ARTNOW_RCR_No      = if_else(ARTNOW_RC == "No", 1, 0), ##> Reference (not included in final set)
    ARTNOW_RCD_Yes     = if_else(ARTNOW_RC == "Yes", 1, 0),
    ARTNOW_RCD_Missing = if_else(ARTNOW_RC == "Missing", 1, 0),
    ARTADHR_RC = fct_recode(as.factor(ARTADHR),
                            "Negative" = "1",
                            "Negative" = "2",
                            "Neutral"  = "3",
                            "Positive" = "4",
                            "Positive" = "5",
                            "Positive" = "6",
                            "Missing"  = "8", #None refused
                            "Missing"  = "9"), #Many skipped
    ARTADHR_RCR_Negative = if_else(ARTADHR_RC == "Negative", 1, 0), ##> Reference (not included in final set)
    ARTADHR_RCD_Neutral  = if_else(ARTADHR_RC == "Neutral", 1, 0),
    ARTADHR_RCD_Positive = if_else(ARTADHR_RC == "Positive", 1, 0),
    ARTADHR_RCD_Missing  = if_else(ARTADHR_RC == "Missing", 1, 0),
    #> HIV Disclosure
    DISC_RCD_Partner = if_else(DISCB == 1 | 
                                 DISCC == 1 |
                                 grepl("Partner", DISCRECODE),
                               1, 0),
    DISC_RCD_Family  = if_else(DISCD == 1 | 
                                 DISCE == 1 |
                                 grepl("Family", DISCRECODE),
                               1, 0),
    DISC_RCD_Other   = if_else(DISCF == 1 | DISCG == 1 |
                                 DISCH == 1 | DISCI == 1 |
                                 grepl("Other", DISCRECODE), 1, 0),
    DISC_RCD_Missing = if_else(!DISC %in% c(1:10), 1, 0),
    DISC_RCR_None    = if_else(DISC_RCD_Partner == 0 & 
                                 DISC_RCD_Family == 0 &
                                 DISC_RCD_Other == 0 &
                                 DISC_RCD_Missing == 0, 1, 0), ##> Not included in final set
    DISC_RCD_Anyone  = if_else(DISC_RCR_None == 1, 0, 1), ##> Collapsed version dummy variable
    #> Substance Use
    DRUG_RCD_Alcohol    = if_else(DRUG1LA == 1 | 
                                    grepl("Alcohol", DRUGRECODE), 1, 0),
    DRUG_RCD_Tobacco    = if_else(DRUG1LB == 1 |
                                    grepl("Tobacco", DRUGRECODE), 1, 0),
    DRUG_RCD_Marijuana  = if_else(DRUG1LC == 1 |
                                    grepl("Marijuana", DRUGRECODE), 1, 0),
    DRUG_RCD_Other      = if_else(
      rowSums(
        select(.,
               one_of(paste0("DRUG1L", LETTERS[4:12]),
                      paste0("DRUG2L", LETTERS[1:9]))               
        ) == 1) > 0 |
        grepl("Other", DRUGRECODE),
      1, 0),
    DRUG_RCD_Missing    = if_else(DRUG1L == 98 & DRUG2L == 98, 1, 0),
    DRUG_RCR_None       = if_else(DRUG_RCD_Alcohol == 0 &
                                    DRUG_RCD_Tobacco == 0 &
                                    DRUG_RCD_Marijuana == 0 &
                                    DRUG_RCD_Other == 0 &
                                    DRUG_RCD_Missing == 0, 1, 0), ##> Not included in final set
    DRUG_RCD_Uses       = if_else(DRUG_RCR_None == 1, 0, 1), ##> Collapsed version dummy variable
    INJECTL_RC = case_when(
      INJECTL == 1 ~ "Yes",
      INJECTL == 0 ~ "No",
      TRUE ~ "Missing"
    ),
    INJECTL_RCR_No      = if_else(INJECTL_RC == "No", 1, 0), ##> Reference (not included in final set)
    INJECTL_RCD_Yes     = if_else(INJECTL_RC == "Yes", 1, 0),
    INJECTL_RCD_Missing = if_else(INJECTL_RC == "Missing", 1, 0)
  ) %>%
    
  #Scales
  #>Procedure:
  ##> 1) Convert any values not included in the item to NA ("Refuse to answer", "Don't know", or "Skipped")
  ###>     Each variable coded as integers; values above certain integer are generally refuse, don't know, skip (often 7, 97, or 997--varies by question)
  ##> 2) Check whether all items are NA (Does number of NA = number of items?)
  ###>     If all items in a scale were skipped, do not sum to 0 (participant should be marked as missing rather than 0)
  ##> 3) If not, sum all items (ignoring NA values: na.rm = TRUE), else leave as NA
  
  #> Social support, 3 items
  mutate_at(vars(matches("SOCIALS\\d{1}")),
            list(RC = ~replace(., which(. > 10), NA))) %>% #Values of 0-10 expected; above that = Refuse/Skip
  mutate(
    SOCIALS_RC = case_when(
      rowSums(is.na(select(., matches("SOCIALS\\d{1}_RC")))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., matches("SOCIALS\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> HIV-related Stigma, 10 items
  mutate_at(vars(matches("STIGMA\\d{1}")),
            list(RC = ~replace(., which(. > 4), NA))) %>% #Values of 0-4 expected; above that = Refuse
  mutate(
    STIGMA_RC = case_when(
      rowSums(is.na(select(., matches("STIGMA\\d+_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("STIGMA\\d+_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Youth Health Engagement, 10 items
  ##> Health Access Literacy (HAL): HE01-HE05; exclude HE05 because all but 17 participants skipped (17 under age of 18)
  ##> Health Self-Efficacy (HSE): HE06-HE10
  mutate_at(vars(starts_with("HE"), -HE05),
            list(RC = ~replace(., which(. > 4), NA))) %>% #Values of 1-4 expected; 7 = Don't Know, 8 = Refuse, 9 = Skip
  mutate(
    HE_RC_HAL = case_when(
      rowSums(is.na(select(., one_of(paste0("HE0", 1:4, "_RC"))))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("HE0", 1:4, "_RC"))), na.rm = TRUE) #If so, sum columns
    ),
    HE_RC_HSE = case_when(
      rowSums(is.na(select(., one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))))) < 5 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Provider Empathy (CARE), 10 items
  mutate_at(vars(matches("CARE\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 1-5 expected; 8 = refuse, 9 = "Not Applicable"
  mutate(
    CARE_RC = case_when(
      rowSums(is.na(select(., matches("CARE\\d{2}_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("CARE\\d{2}_RC")), na.rm = TRUE), #If so, sum columns
      TRUE ~ 0
    )
  ) %>%
  #> Physical and Mental Health, 3 items (excluding MENTALH because it differs from 1-3)
  mutate_at(vars(starts_with("MENTALH")),
            list(RC = ~replace(., which(. > 6), NA))) %>% #Values of 1-6 expected; 8 = refuse to answer
  mutate(MENTALH3_RC = 7 - MENTALH3_RC) %>% #Reverse code MENTALH3 because it is negatively correlated with MENTALH1/2
  mutate(
    MENTALH_RC = case_when(
      rowSums(is.na(select(., matches("(MENTALH)(1|2|3)(_RC)")))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., matches("(MENTALH)(1|2|3)(_RC)")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Email Usage, 4 items
  mutate_at(vars(starts_with("MTUEX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUEX_RC = case_when(
      rowSums(is.na(select(., matches("MTUEX\\d{1}_RC")))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., matches("MTUEX\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Text Usage, 3 items (MTUSPX01, MTUSPX02, MTUSPX12)
  mutate_at(vars(starts_with("MTUSPX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUSPX_RC_Text = case_when(
      rowSums(is.na(select(., MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC)) #If so, sum columns
    )
  ) %>%
  #> Mobile Phone Usage, 9 items (MTUSPX03 through MTUSPX11)
  mutate(
    MTUSPX_RC_Smartphone = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUSPX",
                                            str_pad(3:11, width = 2, pad = 0),
                                            "_RC"
      ))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUSPX",
                                        str_pad(3:11, width = 2, pad = 0),
                                        "_RC"
        ))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Internet Search, 4 items (MTUIX5 and MTUIX6 excluded from scale: added for this study, not part of original subscale)
  mutate_at(vars(starts_with("MTUIX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUIX_RC = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUIX", c(1:4), "_RC"))))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUIX", c(1:4), "_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> General Social Media Usage, 9 items (Excluded MTUSNX10:MTUSNX12; added for this study, not part of original subscale; can use MTUSNX10_RC as separate predictor; MTUSNX11:MTUSNX12 are potential outcomes)
  mutate_at(vars(starts_with("MTUSNX")),
            list(RC = ~replace(., which(. == 99), 0))) %>% #99 = skipped; change 99 to 0 as those skipped indicated not using social media
  mutate_at(vars(matches("MTUSNX\\d+_RC")),
            list(~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUSNX_RC = case_when(
      rowSums(is.na(select(., one_of(c(paste0("MTUSNX0", 1:9, "_RC")))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(c(paste0("MTUSNX0", 1:9, "_RC")))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Positive Attitudes Toward Technology, 6 items (MTUAX01, MTUAX03:MTUAX04, MTUAX09:MTUAX11; MTUAX02 to be kept separate)
  mutate_at(vars(matches("MTUAX\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 0-5 expected; 8 = refuse to answer; 9 = skipped (none)
  mutate(
    MTUAX_RC_Pos = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), "_RC")
      )))) < 6 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Anxiety About Being Without Technology or Dependence on Technology, 3 items (MTUAX05, MTUAX06, MTUAX08; MTUAX07 to be kept separate)
  mutate(
    MTUAX_RC_Anx = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(c(5, 6, 8), width = 2, pad = 0), "_RC")
      )))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(c(5, 6, 8), width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Negative Attitudes Toward Technology, 3 items (MTUAX12:MTUAX14)
  mutate(
    MTUAX_RC_Neg = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(12:14, width = 2, pad = 0), "_RC")
      )))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(12:14, width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%

  #Outcomes
  #> _RECODE variables are recoded from text (S56_25S) for participants who responded that they searched for other health topics (S56_25L)
  ##> Coded as 1 if it should be included in the appropriate outcome, or 0 otherwise
  mutate(
    #> Internet Searches for Sexual Health (Lifetime)
    ##> Convert NA to 0 so that conditionals work below
    outcome_SSex_RECODE = replace( 
      outcome_SSex_RECODE,
      which(is.na(outcome_SSex_RECODE)),
      0
    ), 
    ##> Create indicator variable indicating whether any of sexual health variables are answered affirmatively
    outcome_Search_SexHealth = if_else( 
        rowSums(
          select(., S56_25B, S56_25C, S56_25D, S56_25E, S56_25F, S56_25G) == 1
        ) > 0 | outcome_SSex_RECODE == 1,
        1, 0
      ),
    #> Internet Searches for General Health (Lifetime)
    ##> Convert NA to 0 so that conditionals work below
    outcome_SGen_RECODE = replace(
      outcome_SGen_RECODE,
      which(is.na(outcome_SGen_RECODE)),
      0
    ),
    ##> Create indicator variable indicating whether any of general health variables are answered affirmatively
    outcome_Search_GenHealth = if_else(
      rowSums(
        select(., S56_25A, S56_25H, S56_25I, S56_25J, S56_25K) == 1
      ) > 0 | outcome_SGen_RECODE == 1,
      1, 0
    ),
    #> Communication about Sexual Health
    outcome_Comms_SexHealth = if_else(
      rowSums(select(., 
                     paste0("S56_6", c("K", "M")),
                     paste0("S56_9", c("K", "M")),
                     paste0("S56_15", c("K", "M")),
                     paste0("S56_18", c("K", "M"))) == 1) > 0, 1, 0
    )
  ) %>%
  mutate_at(vars(contains("_RCR"), contains("_RCD"), BORNHIV, CARELHIV, contains("outcome")),
            list(as.factor))

##### Create a data set for analysis excluding original variables
dhsb_analysis <- dhsb %>%
  filter(Set == 1) %>% #Remove participants without 06m assessment
  select(-Set) %>%
  filter(AGE_RC >= 18) %>% #> Remove minors
  filter(!(is.na(HE_RC_HAL) | is.na(HE_RC_HSE))) %>% #> Remove participants with missing values for either YEHS variable
  select(SITE_RCD_FRI, SITE_RCD_NYSDA, SITE_RCD_HBHC, SITE_RCD_MHS,
         SITE_RCD_PFC, SITE_RCD_PSU, SITE_RCD_SFDPH, SITE_RCD_WFU, SITE_RCD_WUSL, #Site
         surveylanguage_RCD_Eng,
         AGE_RC, #Age
         RACE_RCD_Latino, RACE_RCD_Black, RACE_RCD_Other, RACE_RCD_Missing, #Ethnicity & Race
         GENDER_RCD_Female, GENDER_RCD_Trans, 
         GENDER_RCD_Other, GENDER_RCD_Missing, #Gender
         ORIENT_RCD_Gay, ORIENT_RCD_Bi, ORIENT_RCD_Other, #Orientation
         GRADE_RCD_PostK, GRADE_RCD_Grad, #Education
         MONEY_RCD_Low, MONEY_RCD_High, MONEY_RCD_DontKnow, 
         STAY7D_RCD_Stable, STAY7D_RCD_Missing, #Housing
         BORNHIV, TIMESINCEHIV,
         ViralSupp_RCD_Suppressed, #Viral Suppression
         INSURE_RCD_Insured, INSURE_RCD_Missing, #INSURE_RCD_Unknown, #Healthcare utilization: Insurance
         CARED6_RCD_Yes, CARED6_RCD_Missing, #Healthcare utilization: Recent care
         CAREHV06_MCD_RCD_Yes, CAREHV06_MCD_RCD_Missing,
         ARTNOW_RCD_Yes, ARTNOW_RCD_Missing, #Healthcare utilization: Treatment
         ARTADHR_RCD_Neutral, ARTADHR_RCD_Positive, #Healthcare utilization: Adherence
         HE_RC_HAL, HE_RC_HSE, #Youth Health Engagement scale
         CARELHIV, CARE_RC, #Provider Empathy (CARE) scale, along with indicator of whether it is skipped (CARELHIV)
         STIGMA_RC, #HIV-related stigma
         DISC_RCD_Missing, 
         DISC_RCD_Anyone, #> Shouldn't be included in model with Partner/Family/Other disclosures
         MENTALH_RC, MENTALH4_RC,#Mental health
         DRUG_RCD_Alcohol, DRUG_RCD_Tobacco, DRUG_RCD_Marijuana, 
         DRUG_RCD_Other, #DRUG_RCD_Missing, #Substance use: non-injected
         DRUG_RCD_Uses, #> Shouldn't be included in model with Tobacco/Marijuana/Other substances
         INJECTL_RCD_Yes, INJECTL_RCD_Missing, #Substance use: injected
         SOCIALS_RC, #Social support
         #Media Technology Usage and Attitudes Scale
         MTUEX_RC, 
         MTUSPX_RC_Text, 
         MTUSPX_RC_Smartphone, 
         MTUIX_RC,
         MTUSNX_RC,
         MTUAX_RC_Pos, MTUAX_RC_Anx, MTUAX_RC_Neg,
         #Outcomes
         outcome_Search_SexHealth, outcome_Search_GenHealth, outcome_Comms_SexHealth
  ) %>%
  #> Remove variables that were included but have no affirmative responses
  ##> This includes _RCD_Missing dummy variables for RACE, GENDER, STAY7D, INSURE, CARED6, and DISC
  select_if(~length(which(. == 0)) < length(.))

##### Create list of variables with more easily interpretable labels
dhsb_labels <- tribble(
  ~Variable,                    ~Label,
  "SITE_RCD_FRI",               "Site: Los Angeles",
  "SITE_RCD_NYSDA",             "Site: New York",
  "SITE_RCD_HBHC",              "Site: Chicago",
  "SITE_RCD_MHS",               "Site: Cleveland",
  "SITE_RCD_PFC",               "Site: Hershey",
  "SITE_RCD_PSU",               "Site: Philadelphia",
  "SITE_RCD_SFDPH",             "Site: San Francisco",
  "SITE_RCD_WFU",               "Site: Winston-Salem",
  "SITE_RCD_WUSL",              "Site: St. Louis",
  "surveylanguage_RCD_Eng",     "Survey Language: English",
  "AGE_RC",                     "Age (Years)",
  "RACE_RCD_Latino",            "Ethnicity and Race: Latino",
  "RACE_RCD_Black",             "Ethnicity and Race: Black, Not Latino",
  "RACE_RCD_Other",             "Ethnicity and Race: Other",
  "GENDER_RCD_Female",          "Gender: Female (cis woman)",
  "GENDER_RCD_Trans",           "Gender: Trans-identified",
  "GENDER_RCD_Other",           "Gender: Other",
  "ORIENT_RCD_Gay",             "Sexual Orientation: Gay or Lesbian",
  "ORIENT_RCD_Bi",              "Sexual Orientation: Bisexual",
  "ORIENT_RCD_Other",           "Sexual Orientation: Other",
  "GRADE_RCD_PostK",            "Education: Some Post-K12",
  "GRADE_RCD_Grad",             "Education: College Graduate or Trade-Certified",
  "MONEY_RCD_Low",              "Income Last Month: Low (Under Median)",
  "MONEY_RCD_High",             "Income Last Month: High (At or Above Median)",
  "MONEY_RCD_DontKnow",         "Income Last Month: Don't Know",
  "STAY7D_RCD_Stable",          "Housing Last 7 Days: Stable Housing",
  "BORNHIV",                    "Born with HIV",
  "TIMESINCEHIV",               "Years Since HIV Diagnosis",
  "ViralSupp_RCD_Suppressed",   "Viral Suppression (Self-reported)",
  "INSURE_RCD_Insured",         "Insurance: Insured",
  # "INSURE_RCD_Unknown",         "Insurance: Don't Know",
  "CARED6_RCD_Yes",             "Non-HIV Care Visits, Past 6 Months: Yes",
  "CAREHV06_MCD_RCD_Yes",       "HIV Care Visits, Past 6 Months: Yes",
  "CAREHV06_MCD_RCD_Missing",   "HIV Care Visits, Past 6 Months: Missing",
  "ARTNOW_RCD_Yes",             "Taking ART/HIV Medication Now: Yes",
  "ARTNOW_RCD_Missing",         "Taking ART/HIV Medication Now: Missing",
  "ARTADHR_RCD_Neutral",        "ART Adherence: Neutral",
  "ARTADHR_RCD_Positive",       "ART Adherence: Positive",
  "HE_RC_HAL",                  "YEHS: Health Access Literacy Subscale",
  "HE_RC_HSE",                  "YEHS: Health Self-Efficacy Subscale",
  "CARELHIV",                   "Ever received HIV care",
  "CARE_RC",                    "Provider Empathy Scale",
  "STIGMA_RC",                  "HIV-related Stigma Scale",
  "DISC_RCD_Anyone",            "HIV Status Disclosed",
  "MENTALH_RC",                 "Physical and Mental Health Scale",
  "MENTALH4_RC",                "Physical Health (1 item)",
  "DRUG_RCD_Alcohol",           "Substance Use: Alcohol",
  "DRUG_RCD_Tobacco",           "Substance Use: Tobacco",
  "DRUG_RCD_Marijuana",         "Substance Use: Marijuana",
  "DRUG_RCD_Other",             "Substance Use: Other Non-Injected",
  "DRUG_RCD_Uses",              "Substance Use Other Than Alcohol",
  "INJECTL_RCD_Yes",            "Injected Substance Use: Yes",
  "INJECTL_RCD_Missing",        "Injected Substance Use: Missing",
  "SOCIALS_RC",                 "Social Support Scale",
  "MTUEX_RC",                   "MTUAS: Email Usage Subscale",
  "MTUSPX_RC_Text",             "MTUAS: Text Usage Subscale",
  "MTUSPX_RC_Smartphone",       "MTUAS: Smartphone Usage Subscale",
  "MTUIX_RC",                   "MTUAS: Internet Search Usage Subscale",
  "MTUSNX_RC",                  "MTUAS: General Social Media Usage Subscale",
  "MTUAX_RC_Pos",               "MTUAS: Positive Attitudes",
  "MTUAX_RC_Anx",               "MTUAS: Anxiety/Dependence on Technology",
  "MTUAX_RC_Neg",               "MTUAS: Negative Attitudes"
)

save(dhsb, dhsb_analysis, dhsb_labels,
     file = "dhsb.RData")


