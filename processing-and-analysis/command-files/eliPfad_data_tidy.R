
# Processing script for tidying and transforming the raw data for analysis

# Setup ####
# R version:  4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
# R Studio Version: RStudio 2021.09.0+351 "Ghost Orchid" Release for Windows
# Platform: x86_64-w64-mingw32/x64

## Packages ####
# To ensure reproducibility, we specify the package versions used in this analysis with renv. 
# This allows us to recreate the same environment in the future, even if package updates have occurred.

# Exact package versions for future replication
renv::use(
  bit         = "bit@4.6.0",
  bit64       = "bit64@4.6.0-1",
  cli         = "cli@3.6.5",
  clipr       = "clipr@0.8.0",
  cpp11       = "cpp11@0.5.3",
  crayon      = "crayon@1.5.3",
  dplyr       = "dplyr@1.2.0",
  forcats     = "forcats@1.0.1",
  generics    = "generics@0.1.4",
  glue        = "glue@1.8.0",
  haven       = "haven@2.5.5",
  hms         = "hms@1.1.4",
  labelled    = "labelled@2.16.0",
  lifecycle   = "lifecycle@1.0.5",
  magrittr    = "magrittr@2.0.4",
  pillar      = "pillar@1.11.1",
  pkgconfig   = "pkgconfig@2.0.3",
  prettyunits = "prettyunits@1.2.0",
  progress    = "progress@1.2.3",
  purrr       = "purrr@1.2.1",
  R6          = "R6@2.6.1",
  readr       = "readr@2.2.0",
  rlang       = "rlang@1.1.7",
  stringi     = "stringi@1.8.7",
  stringr     = "stringr@1.6.0",
  tibble      = "tibble@3.3.1",
  tidyr       = "tidyr@1.3.2",
  tidyselect  = "tidyselect@1.2.1",
  tzdb        = "tzdb@0.5.0",
  utf8        = "utf8@1.2.6",
  vctrs       = "vctrs@0.7.2",
  vroom       = "vroom@1.7.0",
  withr       = "withr@3.0.2"
)

## Load required packages
library(cli)
library(dplyr)
library(generics)
library(glue)
library(haven)
library(labelled)
library(lifecycle)
library(magrittr)
library(pillar)
library(pkgconfig)
library(R6)
library(rlang)
library(tibble)
library(tidyselect)
library(utf8)
library(vctrs)
library(withr)


## Set working directory ####

setwd("./eliPfad-data-publications/original-data")


## 1. Import raw data ####
## Different data sets originate from different recruitment strategies (e-mail, qr-code, paper-based survey)

# Email datasets
rl_email_a <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_email_alt_20240813.sav",
                       user_na = TRUE)

rl_email_n <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_email_neu_20240930.sav",
                       user_na = TRUE)

# QR-code datasets

rl_qr_logo2_a <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_qr_logo2_alt_20240809_kurz.sav",
                              user_na = TRUE)

rl_qr_logo2_n <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_qr_logo2_neu_20240930.sav",
                          user_na = TRUE)

rl_qr_nologo3_a <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_qr_nologo3_alt_20241029.sav",
                            user_na = TRUE)

rl_qr_nologo3_n <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/export_qr_nologo3_neu_20240930.sav",
                            user_na = TRUE)

# Paper-based dataset

rl_papi <- read_sav("./Datenaufbereitung/Input - Daten im Rohformat/GESAMT - eliPfad Stand 02.10.24.SAV", user_na = FALSE) |> 
  zap_labels()

## 2. Data Merging (multi-stage) ####

### 2.1 Merge email datasets ####

# Harmonize variable types before merging

rl_email_a <- rl_email_a |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT))

rl_email_n <- rl_email_n |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT))

# Full join retains all observations (no loss of cases)

rl_email <- full_join(rl_email_a, rl_email_n) # join_by())


### 2.2 Merge QR datasets ####

# Convert inconsistent variable types

rl_qr_logo2_a <- rl_qr_logo2_a |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    AUSKW_other = as.character(AUSKW_other)
    )

rl_qr_logo2_n <- rl_qr_logo2_n |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT),
    AUSKW_other = as.character(AUSKW_other)
    )

# Merging
rl_qr_logo <- full_join(rl_qr_logo2_a, rl_qr_logo2_n)

## Repeat for "no logo" condition

rl_qr_nologo3_a <- rl_qr_nologo3_a |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT),
    FMAUFG_other = as.character(FMAUFG_other),
    GKINI_other = as.character(GKINI_other),
    FAWB_other = as.character(FAWB_other),
    AUSKW_other = as.character(AUSKW_other)
  )

rl_qr_nologo3_n <- rl_qr_nologo3_n |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT),
    AUSKW_other = as.character(AUSKW_other)
  )

# Merging
rl_qr_nologo <- full_join(rl_qr_nologo3_a, rl_qr_nologo3_n)


### 2.3 Create mode variable to identify recruitment strategie -> "MODUS" ####

# Encodes recruitment strategy 

rl_email$MODUS <- as.double(1)

rl_qr_logo$MODUS <- as.double(2)
 
rl_qr_nologo$MODUS <- as.double(3)


### 2.4 Combine QR datasets####

rl_qr_logo <- rl_qr_logo |> 
  mutate(
    # BER2_FM = as.double(BER2_FM),
    FMAUFG_other = as.character(FMAUFG_other),
    GKINI_other = as.character(GKINI_other),
    FAWB_other = as.character(FAWB_other)
  )

# Merging
rl_qr <- full_join(rl_qr_logo, rl_qr_nologo)

### 2.5 Combine online datasets####

rl_qr <- rl_qr |> 
  mutate(
    AUSKW_other = as.character(AUSKW_other),
    SDSTAT_other = as.character(SDSTAT_other)
  )

# Merging
rl_online <- full_join(rl_qr, rl_email)


### 2.6. Create unique case identifier ####

rl_online$case <- 1:nrow(rl_online)

rl_papi$case <- (nrow(rl_online)+1):(nrow(rl_online)+nrow(rl_papi))


### 2.7. Harmonization and recoding - full data set ####

# Preparation for merging

rl_online <- rl_online |> 
  mutate(
    AUSKW_8 = case_when(AUSKW_other == 0 ~ 0, # hier Mehrfachauswahl berücksichtigen
                        AUSKW_other == "" ~ NA,
                        !is.na(AUSKW_other) & AUSKW_other != "" ~ 1,  # Wenn Text vorhanden, setze auf 1
                        TRUE ~ NA),
    AUSKW_T = case_when(AUSKW_other == 0 ~ NA,
                        AUSKW_other == "" ~ NA,
                        AUSKW_other == 2 ~ NA,
                        !is.na(AUSKW_other) & AUSKW_other != "" ~ AUSKW_other,
                        TRUE ~ NA),
    BPMEET = case_when(BPMEET == 1 ~ 1,
                       BPMEET == 2 ~ 2,
                       BPMEET == 3 ~ 3,
                       BPMEET == "-oth-" ~ 4,
                       BPMEET == "" ~ NA),
    BPMEET = as_factor(BPMEET),
    BPMEET_T = ifelse(BPMEET_other == "", NA, BPMEET_other),
    FMAUFG_10 = case_when(FMAUFG_other == 0 ~ 0, # hier Mehrfachauswahl berücksichtigen
                         FMAUFG_other == "" ~ NA,
                         !is.na(FMAUFG_other) & FMAUFG_other != "" ~ 1,  # Wenn Text vorhanden, setze auf 1
                         TRUE ~ NA),
    FMAUFG_T = ifelse(FMAUFG_other == "", NA, FMAUFG_other),
    across(starts_with("FMAUFG"), ~ ifelse(. == 2, 1, .)), #BUG fix LS: setzt die vorhandenen 2 -> 1 
    SDFREQ = case_when(SDFREQ == 1 ~ 1,
                       SDFREQ == 2 ~ 2,
                       SDFREQ == 3 ~ 3,
                       SDFREQ == 4 ~ 4,
                       SDFREQ == 5 ~ 5,
                       SDFREQ == "-oth-" ~ 6,
                       SDFREQ == "" ~ NA),
    SDFREQ_T = ifelse(SDFREQ_other == "", NA, SDFREQ_other), # bei Antwort 6 ohne Text = 0
    across(starts_with("SDSTAT"), ~ ifelse(. == 2, 1, .)), #BUG fix LS: setzt die vorhandenen 2 -> 1 
    SDSTAT_7 = case_when(SDSTAT_other == 0 ~ 0, # hier Mehrfachauswahl berücksichtigen
                         SDSTAT_other == "" ~ NA,
                         !is.na(SDSTAT_other) & SDSTAT_other != "" ~ 1,  # Wenn Text vorhanden, setze auf 1
                         TRUE ~ NA),
    SDSTAT_T = ifelse(SDSTAT_other == "", NA, SDSTAT_other), # ggf. muss die Null noch NA gesetzt werden
    SDKONFM = case_when(SDKONFM == 1 ~ 1,
                        SDKONFM == 2 ~ 2,
                        SDKONFM == 3 ~ 3,
                        SDKONFM == 4 ~ 4,
                        SDKONFM == 5 ~ 5,
                        SDKONFM == "-oth-" ~ 6,
                        SDKONFM == "" ~ NA),
    SDKONFM_T = ifelse(SDKONFM_other == "", NA, SDKONFM_other),
    across(starts_with("GKINI"), ~ ifelse(. == 2, 1, .)), #BUG fix LS: setzt die vorhandenen 2 -> 1
    GKINI_5 = case_when(GKINI_other == 0 ~ 0, # hier Mehrfachauswahl berücksichtigen
                        GKINI_other == "" ~ NA,
                        !is.na(GKINI_other) & GKINI_other != "" ~ 1,  # Wenn Text vorhanden, setze auf 1
                        TRUE ~ NA),
    GKINI_T = ifelse(GKINI_other == "", NA, GKINI_other), # ggf. muss die Null noch NA gesetzt werden
    GKPOT = case_when(GKPOT == 1 ~ 1,
                      GKPOT == 2 ~ 2,
                      GKPOT == 3 ~ 3,
                      GKPOT == 4 ~ 4,
                      GKPOT == 5 ~ 5,
                      GKPOT == "-oth-" ~ 6,
                      GKPOT == "" ~ NA),
    GKPOT = as_factor(GKPOT),
    GKPOT_T = ifelse(GKPOT_other == "", NA, GKPOT_other),
    FAWB_6 =  case_when(FAWB_other == 0 ~ 0, # hier Mehrfachauswahl berücksichtigen
                        FAWB_other == "" ~ NA,
                        !is.na(FAWB_other) & FAWB_other != "" ~ 1,  # Wenn Text vorhanden, setze auf 1
                        TRUE ~ NA),
    FAWB_T = ifelse(FAWB_other == "", NA, FAWB_other) # ggf. muss die Null noch NA gesetzt werden 
  )

## papi Datensatz
rl_papi <- rl_papi |> 
  mutate(
    BPMEET = as_factor(BPMEET),
    SDFREQ = as_factor(SDFREQ),
    SDKONFM = as_factor(SDKONFM),
    GKPOT = as_factor(GKPOT),
    AUSKW_T = ifelse(AUSKW_8T == "", NA, AUSKW_8T),
    BPMEET_T = ifelse(BPMEETTX == "", NA, BPMEETTX),
    FMAUFG_T = ifelse(FMAUFGTX  == "", NA, FMAUFGTX),
    SDFREQ = ifelse(SDFREQ == 7, 6, SDFREQ),
    SDFREQ_T = ifelse(SDFREQTX == "", NA, SDFREQTX),
    SDSTAT_T = ifelse(SDSTATTX == "", NA, SDSTATTX),
    SDKONFM = ifelse(SDKONFM == 7, 6, SDKONFM),
    SDKONFM_T = ifelse(SDKONFTX == "", NA, SDKONFTX),
    GKINI_T = ifelse(GKINITX == "", NA, GKINITX),
    GKPOT_T = ifelse(GKPOTTX == "", NA, GKPOTTX),
    FAWB_T = ifelse(FAWBTX == "", NA, FAWBTX),
    PERGEN = case_when(
    PERGEN == 1 ~ 1,             # Männlich bleibt
    PERGEN == 2 ~ 2,             # Weiblich bleibt
    PERGEN == 3 ~ 3,             # Weiteres bleibt
    PERGEN %in% 0 ~ 4),     #Keine Angabe aus Papi wird an online angepasst -> Fehlzuweisung in Teleform
    RC1_COM = case_when( # Recodieren von RC Variable RC1_COM -> Fehler in Teleform
      RC1_COM == 1 ~ 1,
      RC1_COM == 2 ~ 3,
      RC1_COM == 3 ~ 5,
      RC1_COM == 4 ~ 4,
      RC1_COM == 5 ~ 2),
    FM_INIT = case_when( # Recodieren von FM_INIT -> Fehler in Teleform
      FM_INIT == 1 ~ 1,
      FM_INIT == 2 ~ 2,
      FM_INIT == 3 ~ 3,
      FM_INIT == 4 ~ 4,
      FM_INIT == 5 ~ 5,
      FM_INIT == 6 ~ 999
    ))

### 2.8 Merge online + paper datasets ####

merged_data <- full_join(x = rl_papi, y = rl_online)
                         
# delete redundant datasets to free up memory.
rm(rl_email_a, rl_email_n, rl_qr_logo2_a, rl_qr_logo2_n,rl_qr_nologo3_n, rl_qr_nologo3_a, rl_qr_logo, rl_qr_nologo,rl_qr, rl_email)


## 3. Data cleaning - tidy data ####

tidy_data <- merged_data |>

# unpassende Variablen des gemergeten Satzes zusammenführen

  mutate(datestamp = as.Date(datestamp),
         Datum = coalesce(DATUM, datestamp),
         BPMEET = as.numeric(BPMEET),
         GKPOT = as.numeric(GKPOT),
         JOBPAT1 = if_else(JOBPAT1 >= 0 & JOBPAT1 <= 100, JOBPAT1, NA_real_), # Plausibilität. Range wurde bei Plausibilitätsprüfung verletzt
         ANZKH = if_else(ANZKH >= 1 & ANZKH <= 25, ANZKH, NA_real_),
         FMHB = if_else(FMHB >= 0 & FMHB <= 15, FMHB, NA_real_)
         ) |> 
 
# Ausschluss von nicht benötigten Variablen (reduzieren des Datasets)
  select(-c("lastpage","lfd_nr","BATCHNO","id","DATUM", "datestamp", "AUSKW", 
              "AUSKW_8T", "AUSKW_other", "BPMEETTX","BPMEET_other", "FMAUFG", 
              "FMAUFGTX", "FMAUFG_other", "SDFREQTX", "SDFREQ_other", "SDSTAT", 
              "SDSTATTX", "SDSTAT_other", "SDKONFTX", "SDKONFM_other", "GKINI", 
              "GKINITX", "GKINI_other", "GKPOT_other", "GKPOTTX","FAWB","FAWBTX", 
              "FAWB_other", "LASTQ")
           ) 
         
## 4. Transform #####

# Bilden der Relational Coordination scores

transformed_data <- tidy_data |> 
  dplyr::mutate(
    RC_score = rowMeans(across(RC1_COM:RC7_REL)), # Gesamtscore 
    RC_cat = case_when(
      RC_score < 3.5 ~ 1,
      RC_score >= 3.5 & RC_score <= 4 ~ 2,
      RC_score > 4 ~ 3), # Kategorisierung des RC Scores
    RC_com = rowMeans(across(RC1_COM:RC4_COM)), # Score für die Subskala Kommunikation
    RC_rel = rowMeans(across(RC5_REL:RC7_REL)), # Score für die Subskala Relation
    RC_di = case_when(
      RC_cat == 1 ~ 1,
      RC_cat == 2  ~ 2,
      RC_cat == 3 ~ 2), # Dichotomisieren des RC Scores
    .after = RC7_REL)

# Verhältnis zu Ärzt:innen

transformed_data <- transformed_data |>
  mutate(
    VERKLI_roh = rowSums(across(VERKLI_1:VERKLI_5)), # Rohe Skala 
    VERKLI_score = rowMeans(across(VERKLI_1:VERKLI_5)), # relative Skala
    .after = VERKLI_5) |> 
  
# Intention to use eliPfad
  mutate(
    BER_score = rowMeans(across(c(BER1_BP,BER2_FM, BER3_SD, BER4_GK))),
                         
# Variablen für Befragungs und Versandmodus
    Befragungsmodus = case_when(
      MODUS == 1 ~ 1,
      MODUS == 2 ~ 1,
      MODUS == 3 ~ 1,
      MODUS == 4 ~ 2,
      MODUS == 5 ~ 2),


  Versandmodus = case_when(
    MODUS ==  1 ~ 1,
    MODUS ==  2 ~ 2,
    MODUS ==  3 ~ 2,
    MODUS ==  4 ~ 3,
    MODUS ==  5 ~ 3),



# Weiteres aus der Geschelechtvariable entfernen, da nicht zutreffend und "Keine Angabe wird zu NA"
PERGEN = case_when(
  PERGEN == 1 ~ 1,             # Männlich bleibt
  PERGEN == 2 ~ 2,
  PERGEN == 3 ~ 3,
  PERGEN == 4 ~ NA,
  TRUE ~ NA_real_),     #Keine Angabe = NA

# Keine Angabe in der Praxisform wird zu NA
PRXFORM = case_when(
  PRXFORM == 1 ~ 1,
  PRXFORM == 2 ~ 2,
  PRXFORM == 3 ~ 3,
  PRXFORM == 4 ~ 4,
  PRXFORM == 5 ~ NA,
  TRUE ~ NA_real_),

WUN_BP = case_when(
  WUN_BP == 1 ~ 1,
  WUN_BP == 2 ~ 3,
  WUN_BP == 3 ~ 5,
  WUN_BP == 4 ~ 4,
  WUN_BP == 5 ~ 2,
  WUN_BP == 6 ~ 6,
  TRUE ~ NA_real_),

WUN_SOZ = case_when(
  WUN_SOZ == 1 ~ 1,
  WUN_SOZ == 2 ~ 3,
  WUN_SOZ == 3 ~ 5,
  WUN_SOZ == 4 ~ 4,
  WUN_SOZ == 5 ~ 2,
  WUN_SOZ == 6 ~ 6,
  TRUE ~ NA_real_),
)

## 5. Labeling ####

# Assign variable label
rl_data <- transformed_data |> 
    
  set_variable_labels(
    case = "Anonyme Zählnummer",
    MODUS = "Rekrutierungsstrategie",
    Befragungsmodus = "Befragungsmodus",
    Versandmodus = "Versandmodus",
    EM_QUAL = "Qualität Entlassmanagement",
    ANZKH = "Krankenhäuser Zusammenarbeit",
    ANZENTL = "Entlassene Patient:innen",
    AUSGES = "Kollegialer Austausch",
    AUSINIT = "Initiation Austausch",
    AUSKW_1 = "Austausch Telefonat",
    AUSKW_2 = "Austausch E-Mail",
    AUSKW_3 = "Austausch Messenger",
    AUSKW_4 = "Austausch KIM",
    AUSKW_5 = "Austausch Videokonferenz",
    AUSKW_6 = "Austausch Persönlich",
    AUSKW_7 = "Austausch Fax",
    AUSKW_8 = "Austausch Sonstiger",
    AUSERR = "Erreichbarkeit außerhalb",
    AUSGEL = "Austausch Gelegenheiten",
    AUSGEL2 = "Austausch Verantstaltungen",
    RC1_COM = "RC Häufigkeit Kommunikation",
    RC2_COM = "RC Zeitpunkt Kommunikation",
    RC3_COM = "RC Präzision Kommunikation",
    RC4_COM = "RC Problemlösung Kommunikation",
    RC5_REL = "RC Ziele teilen",
    RC6_REL = "RC Wissen Klinikärzt:innen",
    RC7_REL = "RC Respekt",
    RC_di = "Relational Coordination dichotomisiert",
    VERKLI_1 = "Verhältnis Klinikärzt:innen",
    VERKLI_2 = "Verhältnis KÄ Arroganz",
    VERKLI_4 = "Verhältnis KÄ vergessen",
    VERKLI_5 = "Verhältnis KÄ Zeit",
    INFO_BP = "Sie vor der Entlassung für die Behandlungsplanung Ihrer Patient:innen kontaktiert werden?",
    INFO_HLP = "die Einleitung pflegerischer oder sozialer Hilfe mit Ihnen abgestimmt wird?",
    INFO_EB1 = "Sie im Entlassbrief unvollständige Informationen erhalten, die eine kontinuierliche Weiterversorgung beeinträchtigen",
    INFO_EB2 = "Sie sich mit Rückfragen zum Entlassbrief an eine/einen der Klinikärzt:innen wenden?",
    INFO_AP1 = "Sie keine Rufnummer einer zuständigen ärztlichen Ansprechperson für Rückfragen haben?",
    INFO_AP2 = "Sie eine Ansprechperson nach Enlassung erreichen wollen und es Ihnen nicht gelingt?",
    INFO_MD1 = "Sie die Medikation aus der Klinik unverändert übernehmen können?",
    INFO_MD2 = "Sie die aus der Klinik verordneten Medikamente nicht inhaltlich nachvollziehen können?",
    WUN_BP = "Wunsch Kontakt",
    WUN_SOZ = "Wunsch Abstimmung",
    BER1_BP = "Zustimmung Behandlungs- und Medikationsplanung",
    BPMEET = "Durchführung Behandlungsplanung",
    BPDAUER = "effektive Besprechungsdauer",
    BPEURO = "Vergütung Besprechung",
    BP_MED = "Mehrwert Medikation",
    BER2_FM = "Zustimmung Fallmanager:innen",
    FMHB = "Zeitpunkt Hausbesuch FM",
    FMEIN_1 = "Einschätzung FM",
    FMEIN_2 = "Empfehlungen FM",
    FMEIN_3 = "Unterstützung Medikation",
    FM_INIT = "Initiation Unterstützung",
    #CAVE: ES GIBT EINIGE BEOBACHTER, DIE MEHR ALS 3 FMAUFG-VARIABLEN GEWÄHLT HABEN. DIESE MÜSSEN WÜR DIE ANALYSEN AUF NA GESETZT WERDEN.
    FMAUFG_1 = "Aufgabe Entlassung Koordination ", 
    FMAUFG_2 = "Aufgabe Beratung",
    FMAUFG_3 = "Aufgabe Dienste Koordination",
    FMAUFG_4 = "Aufgabe Rehaplanung",
    FMAUFG_5 = "Aufgabe Vitalwerte",
    FMAUFG_6 = "Aufgabe Risikofaktoren",
    FMAUFG_7 = "Aufgabe Medikation",
    FMAUFG_8 = "Aufgabe Bedarfe",
    FMAUFG_9 = "Aufgabe Adhärenzförderung",
    FMAUFG_10 = "Aufgabe Sonstige",
    FMEFFEKT = "Unterstützung FM hilfreich",
    BER3_SD = "Zustimmung Smart Devices",
    SDEFFEKT = "Nutzen SD",
    SDFREQ = "Frequenz Monitoring",
    SDMPRAX = "Monitoring ohne FM",
    SDSTAT_1 = "Statusbericht E-Mail",
    SDSTAT_2 = "Statusbericht Brief",
    SDSTAT_3 = "Statusbericht Fax",
    SDSTAT_4 = "Statusbericht KIM",
    SDSTAT_5 = "Statusbericht Online",
    SDSTAT_6 = "Statusbericht Praxissoftware",
    SDSTAT_7 = "Statusbericht Sonstiges",
    SDKONFM = "Kontaktaufnahme FM",
    SD_AAUFW = "Arbeitsaufwand SD",
    SD_GRENZ = "SD Überwachung",
    BER4_GK = "Zustimmung Gesamtkonzept",
    GKINI_1 = "Initiation Klinikärzt:in",
    GKINI_2 = "Initiation Hausärzt:in",
    GKINI_3 = "Initiation Fallmanager:in",
    GKINI_4 = "Initiation Gemeinsam",
    GKINI_5 = "Initiation Weitere",
    ZEIT_GK = "Zeitaufwand GK",
    BUERO_GK = "Bürokratieaufwand GK",
    BLUT_GK = "Verfrühte Entlassung GK",
    REHO_GK = "Rehospitalisierung GK",
    GK_NUTZ = "Nutzen Konzept",
    GKPOT = "Bestandteil Nutzen",
    PRXFORM = "Praxisform",
    PRXREGIO = "Praxisregion",
    PRXBULA = "Praxis-Bundesland",
    PRXVZA = "Anzahl Ärzt:innen",
    PRXVZNA = "Anzahl Personal",
    JOBANG = "Angestelltenverhältnis",
    JOBZEIT = "Arbeitszeit je Woche (h)",
    JOBFALLZ = "Behandlungsfallzahl je Quartal",
    JOBPAT1 = "Anteil multimorbider Patient:innen (%)",
    JOBPAT_2 = "Drehtür-Patient:innen",
    HAUSBPW1 = "Hausbesuche je Woche",
    HAUSBPW2 = "Hausbesuche auch am Wochenende",
    HAJZA = "Berufserfahrung Hausärzt:in",
    SAJZA = "Berufserfahrung stationär",
    FAWB_1 = "in Weiterbildung",
    FAWB_2 = "Praktische Ärzt:in",
    FAWB_3 = "Allgemeinmedizin",
    FAWB_4 = "Innere Medizin",
    FAWB_5 = "Innere und Allgemeinmedizin",
    FAWB_6 = "Sonstige FOBI",
    JOBBEL = "Arbeitsbelastung",
    PERALT = "Alter",
    PERGEN = "Geschlecht",
    AUSKW_T = "Sonstiger Austausch",
    BPMEET_T = "Sonstige Behandlungsplanung",
    FMAUFG_T = "Sonstige Aufgaben FM",
    SDFREQ_T = "Sonstige Frequenz",
    SDSTAT_T = "Sonstige Statusbericht",
    SDKONFM_T = "Sonstige Kontakt FM",
    GKINI_T = "Sonstige Initiation",
    GKPOT_T = "Sonstige Potential",
    FAWB_T = "Sonstige Weiterbildung",
    RC_score = "Relational Coordination Score",
    RC_cat = "Relational Coordination Kategorien",
    RC_com = "Relational Coordination Subscore Kommunikation",
    RC_rel = "Relational Coordination Subscore Beziehung",
    VERKLI_roh = "Verhältnis zu Klinikärzt:innen Rohskala",
    VERKLI_score = "Verhältnis zu Klinikärzt:innen",
    BER_score = "Bereitschaft zur Umsetzung von eliPfad"
  ) |> 

# Assign value label
  add_value_labels(
    MODUS = c("E-Mail" = 1,
              "QR-Code: postalisch mit Logo" = 2,
              "QR-Code: postalisch ohne Logo" = 3,
              "Fragebogen mit Logo" = 4,
              "Fragebogen ohne Logo" = 5),
    
    Befragungsmodus = c("online" = 1,
                        "schriftlich" = 2),
    
    Versandmodus = c("E-Mail" = 1,
                     "Postalisch: QR-Code" = 2,
                     "Postalisch: Fragebogen" = 3),
    
    EM_QUAL = c( "Sehr selten" = 1,
                   "Selten" = 2,
                   "Manchmal" = 3,
                   "Häufig" = 4,
                   "Sehr häufig" = 5),
    
    AUSERR = c("Nein" = 0,
               "Ja" = 1),
    
    AUSGEL = c("Es gibt keine Gelegenheiten für den Austausch" = 1,
               "Es gibt wenige Gelegenheiten für den Austausch" = 2,
               "Es gibt einige Gelegenheiten für den Austausch" = 3,
               "Es gibt viele Gelegenheiten für den Austausch" = 4),
    
    AUSGEL2 = c("Nein" = 0,
               "Ja" = 1,
               "Es gibt keine gemeinsamen Veranstaltungen" = 2),
    
    RC1_COM   = c( "Nicht annähernd genug" = 1,
                   "Viel zu oft" = 2,
                   "Nicht genug" = 3,
                   "Zu oft" = 4,
                   "Genau im richtigen Maß" = 5),
    
    RC2_COM   = c( "Nie" = 1,
                   "Selten" = 2,
                   "Manchmal" = 3,
                   "Häufig" = 4,
                   "Immer" = 5),
    
    RC3_COM   = c( "Nie" = 1,
                   "Selten" = 2,
                   "Manchmal" = 3,
                   "Häufig" = 4,
                   "Immer" = 5),
    
    RC4_COM   = c( "Machen immer andere verantwortlich" = 1,
                   "Machen meistens andere verantwortlich" = 2,
                   "Weder noch" = 3,
                   "Lösen das Problem meistens zusammen" = 4,
                   "Lösen das Problem immer zusammen" = 5),
    
    RC5_REL   = c( "Überhaupt nicht" = 1,
                   "Ein wenig" = 2,
                   "Teilweise" = 3,
                   "Größtenteils" = 4,
                   "Voll und ganz" = 5),
    
    RC6_REL   = c( "Nichts" = 1,
                   "Wenig" = 2,
                   "Etwas" = 3,
                   "Viel" = 4,
                   "Alles" = 5),
    
    RC7_REL   = c( "Überhaupt nicht" = 1,
                   "Ein wenig" = 2,
                   "Teilweise" = 3,
                   "Größtenteils" = 4,
                   "Voll und ganz" = 5),
    
    RC_cat = c("Schwach" = 1,
               "Moderat" = 2,
               "Stark" = 3),
    
    RC_di = c("Schwach" = 1,
              "Moderat/stark" = 2),
    
    VERKLI_1   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    VERKLI_2   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    VERKLI_3   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    VERKLI_4   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    VERKLI_5   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    INFO_BP   = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_HLP  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_EB1  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_EB2  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_AP1  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_AP2  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_MD1  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    INFO_MD2  = c( "0%" = 0,
                   "10%" = 10,
                   "20%" = 20,
                   "30%" = 30,
                   "40%" = 40,
                   "50%" = 50,
                   "60%" = 60,
                   "70%" = 70,
                   "80%" = 80,
                   "90%" = 90,
                   "100%" = 100,
                   "Weiß nicht" = 999),
    
    WUN_BP    = c( "Deutlich häufiger als aktuell" = 1,
                   "Etwas häufiger als aktuell" = 3,
                   "Gleich häufig wie aktuell" = 5,
                   "Etwas seltener als aktuell" = 4,
                   "Deutlich seltener als aktuell" = 2,
                   "Wünsche ich mir nicht" = 6),
    
    WUN_SOZ   = c( "Deutlich häufiger als aktuell" = 1,
                   "Etwas häufiger als aktuell" = 3,
                   "Gleich häufig wie aktuell" = 5,
                   "Etwas seltener als aktuell" = 4,
                   "Deutlich seltener als aktuell" = 2,
                   "Wünsche ich mir nicht" = 6),
    
    BER1_BP    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    BPMEET     = c( "als Videokonferenz" = 1,
                    "als Telefonkonferenz" = 2,
                    "als persönliches Treffen" = 3,
                    "Anders" = 4),

    BP_MED     = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Neutral" = 3,
                    "Stimme eher zu" = 4,
                    "Stimme voll und ganz zu" = 5),
    
    BER2_FM    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    FMEIN_1    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    FMEIN_2    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    FMEIN_3    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    FM_INIT    = c( "Gar nicht wichtig" = 1,
                    "Weniger wichtig" = 2,
                    "Neutral" = 3,
                    "Wichtig" = 4,
                    "Sehr wichtig" = 5,
                    "keine Angabe" = 999),
    
    BER3_SD    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    SDFREQ     = c( "Mehrmals täglich" = 1,
                    "Täglich" = 2,
                    "Jeden 2. Tag" = 3,
                    "Mehrmals pro Woche" = 4,
                    "Wöchentlich" = 5,
                    "Andere Frequenz" = 6),
    
    SDMPRAX    = c( "Ja" = 1,
                    "Vielleicht" = 2,
                    "Nein" = 0),
    
    SDKONFM    = c( "E-Mail" = 1,
                    "Fax" = 2,
                    "KIM" = 3,
                    "Praxistelefon" = 4,
                    "Diensthandy" = 5,
                    "Privathandy" = 6,
                    "Etwas anderes" = 7),
    
    SD_AAUFW   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    SD_GRENZ   = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    BER4_GK    = c( "Stimme überhaupt nicht zu" = 1,
                    "Stimme eher nicht zu" = 2,
                    "Stimme eher zu" = 3,
                    "Stimme voll und ganz zu" = 4),
    
    ZEIT_GK    = c( "Geringer" = 1,
                    "Etwas geringer" = 2,
                    "Gleich" = 3,
                    "Etwas höher" = 4,
                    "Höher" = 5),
    
    BUERO_GK   = c( "Geringer" = 1,
                    "Etwas geringer" = 2,
                    "Gleich" = 3,
                    "Etwas höher" = 4,
                    "Höher" = 5),
    
    BLUT_GK    = c( "Wird seltener" = 1,
                    "Wird eher seltener" = 2,
                    "Bleibt gleich" = 3,
                    "Wird eher häufiger" = 4,
                    "Wird häufiger" = 5),
    
    REHO_GK    = c( "Wird verhindert" = 1,
                    "Wird eher verhindert" = 2,
                    "Bleibt gleich" = 3,
                    "Wird eher verstärkt" = 4,
                    "Wird verstärkt" = 5),
    
    GK_NUTZ    = c( "Kein Nutzen" = 1,
                    "Geringer Nutzen" = 2,
                    "Mittlerer Nutzen" = 3,
                    "Großer Nutzen" = 4,
                    "Weiß nicht" = 5),
    
    GKPOT      = c( "Sektorenübergreifende Behandlungspläne" = 1,
                    "Fallmanager:innen" = 2,
                    "Telemedizinische Betreuung" = 3,
                    "Das Konzept als Ganzes" = 4,
                    "Ich sehe kein Potential zur Verbesserung" = 5,
                    "Etwas anderes" = 6),

    PRXFORM    = c( "Einzelpraxis" = 1,
                    "Praxisgemeinschaft" = 2,
                    "Berufsausübungsgemeinschaft (Gemeinschaftspraxis)" = 3,
                    "Medizinisches Versorgungszentrum (MVZ)" = 4),
    
    PRXREGIO   = c( "Landgemeinde (≤ 5000 Einwohner:innen)" = 1,
                    "Kleinstadt (> 5000-20.000 Einwohner:innen)" = 2,
                    "Mittelstadt (> 20.000-100.000 Einwohner:innen)" = 3,
                    "Großstadt (> 100.000 Einwohner:innen))" = 4),
    
    PRXBULA    = c ( "Baden-Württemberg" = 1,
                     "Bayern" = 2,
                     "Berlin" = 3,
                     "Brandenburg" = 4,
                     "Bremen" = 5,
                     "Hamburg" = 6,
                     "Hessen" = 7,
                     "Mecklenburg-Vorpommern" = 8,
                     "Niedersachsen" = 9,
                     "Nordrhein-Westfalen" = 10,
                     "Rheinland-Pfalz" = 11,
                     "Saarland" = 12,
                     "Sachsen" = 13,
                     "Sachsen-Anhalt" = 14,
                     "Schleswig-Holstein" = 15,
                     "Thüringen" = 16),
    
    JOBANG      = c("Nein" = 0,
                    "Ja" = 1),
    
    JOBPAT_2   = c( "Keine" = 1,
                    "Wenige" = 2,
                    "Einige" = 3,
                    "Viele" = 4,
                    "Sehr viele" = 5),
    
    HAUSBPW2    = c("Nein" = 0,
                    "Ja" = 1),
    
    PERALT     = c( "30 Jahre oder jünger" = 1,
                    "31 bis 40 Jahre" = 2,
                    "41 bis 50 Jahre" = 3,
                    "51 bis 60 Jahre" = 4,
                    "Über 60 Jahre" = 5),
    
    PERGEN     = c( "Männlich" = 1,
                    "Weiblich" = 2,
                    "Weiteres" = 3)
  ) 


## 6. Export  ####

# Select relevant variables for paper "Measuring intersectoral coordination dynamics Psychometric properties of the German Relational Coordination Survey""

rl_data_psych <- rl_data |> 
  select(RC1_COM:RC7_REL, RC_com, RC_rel, RC_score, PERGEN, PERALT, JOBANG, JOBPAT1, PRXREGIO)

# save data set
save(rl_data, file = "./processing-and-analysis/analysis-data/rl_data_psych.Rdata")

