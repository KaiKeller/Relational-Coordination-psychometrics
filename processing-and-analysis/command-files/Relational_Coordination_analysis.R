
# Setup
# R version:  4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
# RStudio 2025.09.1+401 "Cucumberleaf Sunflower" Release for Windows
# Platform: x86_64-w64-mingw32/x64


# Packages 
renv::use( # install "renv" and then run this to get the correct environment
  gt        = "gt@1.1.0",
  gtsummary = "gtsummary@2.4.0",
  labelled  = "labelled@2.15.0",
  lavaan    = "lavaan@0.6-20",
  likert    = "likert@1.3.5.1",
  renv      = "renv@1.0.11",
  semPlot   = "semPlot@1.1.7",
  semTools  = "semTools@0.5-7",
  tidySEM   = "tidySEM@0.2.9",
  tidyverse = "tidyverse@2.0.0"
)

library(gt)
library(gtsummary)
library(labelled)
library(lavaan)
library(likert)
library(renv)
library(semPlot)
library(semTools)
library(tidySEM)
library(tidyverse)

# Reproduciblity
set.seed(123)

# Set theme gtsummary
theme_gtsummary_compact()
theme_gtsummary_language("en")

# Set working directory to project folder


# Load data
load(file = "./eliPfad-data-publications/processing-and-analysis/analysis-data/rl_data_psych.Rdata")


# Data prep

# Exclude missing values in RCS items
rl_data_no_na <- rl_data_psych |>
  drop_na(c(RC1_COM:RC7_REL)) 

# Add English value labels and variable labels, set ordinal items as ordered factors
rl_data_sem <- rl_data_no_na |> 
  add_value_labels(
    RC1_COM   = c( "Not nearly enough" = 1,
                   "Much too often" = 2,
                   "Not enough" = 3,
                   "Too often" = 4,
                   "Just the right amount" = 5),
    
    RC2_COM   = c( "Never" = 1,
                   "Rarely" = 2,
                   "Sometimes" = 3,
                   "Often" = 4,
                   "Always" = 5),
    
    RC3_COM   = c( "Never" = 1,
                   "Rarely" = 2,
                   "Sometimes" = 3,
                   "Often" = 4,
                   "Always" = 5),
    
    RC4_COM   = c( "Always blame" = 1,
                   "Mostly blame" = 2,
                   "Neither blame nor solve" = 3,
                   "Mostly Solve" = 4,
                   "Always solve" = 5),
    
    RC5_REL   = c( "Not at all" = 1,
                   "A little" = 2,
                   "Somewhat" = 3,
                   "A lot" = 4,
                   "Completely" = 5),
    
    RC6_REL   = c( "Nichts" = 1,
                   "A little" = 2,
                   "Some" = 3,
                   "A lot" = 4,
                   "Everything" = 5),
    
    RC7_REL   = c( "Not at all" = 1,
                   "A little" = 2,
                   "Somewhat" = 3,
                   "A lot" = 4,
                   "Completely" = 5),
    
    PRXREGIO   = c( "Rural Community" = 1,
                    "Small town" = 2,
                    "Medium-sized town" = 3,
                    "Large town" = 4),
    
    JOBANG      = c("No" = 0,
                    "Yes" = 1),
    
    PERALT     = c( "\u2264 30" = 1,
                    "31 - 40" = 2,
                    "41 - 50" = 3,
                    "51 - 60" = 4,
                    "\u2264 60" = 5),
    
    PERGEN     = c( "Male" = 1,
                    "Female" = 2,
                    "Divers" = 3)) |> 
  
  set_variable_labels(
    RC_score = "Relational Coordination Score",
    RC_rel = "Subscore Relation",
    RC_com = "Subscore Communication"
  ) |> 
  
 mutate(
    across(c(RC1_COM:RC7_REL), ordered))


# Descriptive Statistics #####

## Table 2 - sample characteristics #####

table2 <-
  rl_data_sem |>
  mutate(
    across(c(PERGEN, PERALT, JOBANG, PRXREGIO), as_factor),
    across(c(PERGEN),fct_drop)) |>
  tbl_summary(
    include = c(PERGEN, PERALT, JOBANG, JOBPAT1, PRXREGIO),  
    type = list(
      all_continuous() ~ "continuous2",
      JOBANG ~ "dichotomous"),
    label = list(
      PERGEN = "Gender",
      PERALT = "Age",
      PRXREGIO = "Practice Region",
      JOBANG = "Self-employed",
      JOBPAT1 = "Proportion of multimorbid patients in practice (%)"),
    value = JOBANG ~ "No",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median}"),
      all_categorical() ~ c("{n} ({p}%)")),
    missing = "ifany",
    missing_text = "Missing") |>
  modify_header(label ~ "Characteristic") |> 
  modify_spanning_header(~ "**General Practitioner characteristics**") |> 
  italicize_levels() |> 
  bold_labels() |>
  as_gt() |> 
  gt::gtsave(filename = "table2.docx", path = "./eliPfad-data-publications/processing-and-analysis/results/tables")


## Table 3 - German Relational Coordination Scale - item and score distribution  #####

table3 <- 
  bind_rows(
    # Communication items
    rl_data_sem |> 
      mutate(across(RC1_COM:RC7_REL, ~ as.numeric(as.character(.)))) |> 
      summarise(across(
        c(RC1_COM:RC4_COM),
        list(
          Md = ~median(.x, na.rm = TRUE),
          Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
          Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
          Min = ~min(.x, na.rm = TRUE),
          Max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) |> 
      pivot_longer(
        everything(),
        names_to = c("Item", ".value"),
        names_pattern = "(RC\\d+_COM)_(.*)"
      ) |>  
      mutate(
        IQR = paste0(Q1, "–", Q3),
        Label = case_when(
          Item == "RC1_COM" ~ "Frequent Communication",
          Item == "RC2_COM" ~ "Timely Communication",
          Item == "RC3_COM" ~ "Accurate Communication",
          Item == "RC4_COM" ~ "Problem Solving Communication"
        ),
        Block = "Communication Index",
        M = NA_real_, SD = NA_real_, Skew = NA_real_, Kurt = NA_real_   
      ) |> 
      select(Block, Label, Md, IQR, M, SD, Skew, Kurt, Min, Max),
    
    # Relation items
    rl_data_sem |> 
      mutate(across(RC1_COM:RC7_REL, ~ as.numeric(as.character(.)))) |> 
      summarise(across(
        c(RC5_REL:RC7_REL),
        list(
          Md = ~median(.x, na.rm = TRUE),
          Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
          Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
          Min = ~min(.x, na.rm = TRUE),
          Max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) |> 
      pivot_longer(
        everything(),
        names_to = c("Item", ".value"),
        names_pattern = "(RC\\d+_REL)_(.*)"
      ) |>  
      mutate(
        IQR = paste0(Q1, "–", Q3),
        Label = case_when(
          Item == "RC5_REL" ~ "Shared Goals",
          Item == "RC6_REL" ~ "Shared Knowledge",
          Item == "RC7_REL" ~ "Mutual Respect"
        ),
        Block = "Relation Index",
        M = NA_real_, SD = NA_real_, Skew = NA_real_, Kurt = NA_real_
      ) |> 
      select(Block, Label, Md, IQR, M, SD, Skew, Kurt, Min, Max),
    
    # Scores
    rl_data_sem |> 
      summarise(across(
        c(RC_com, RC_rel, RC_score),
        list(
          M   = ~mean(.x, na.rm = TRUE),
          SD  = ~sd(.x, na.rm = TRUE),
          Skew = ~{
            x <- .x[!is.na(.x)]
            m <- mean(x)
            s <- sd(x)
            mean(((x - m) / s)^3)
          },
          Kurt = ~{
            x <- .x[!is.na(.x)]
            m <- mean(x)
            s <- sd(x)
            mean(((x - m) / s)^4) - 3
          },
          Min = ~min(.x, na.rm = TRUE),
          Max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) |> 
      pivot_longer(
        everything(),
        names_to = c("Item", ".value"),
        names_pattern = "(RC_[a-z]+)_(.*)"
      ) |> 
      mutate(
        Label = case_when(
          Item == "RC_com"   ~ "Relational Coordination Subscore Communication",
          Item == "RC_rel"   ~ "Relational Coordination Subscore Relation",
          Item == "RC_score" ~ "Relational Coordination Total Score"
        ),
        Block = "Overall Scores",
        Md = NA_real_, IQR = NA_character_  
      ) |> 
      select(Block, Label, Md, IQR, M, SD, Skew, Kurt, Min, Max)
    
  ) |> 
  gt(groupname_col = "Block") |> 
  fmt_number(columns = c(Md, M, SD, Skew, Kurt, Min, Max), decimals = 2) |> 
  sub_missing(columns = everything(), missing_text = "") |>    
  cols_label(
    Label = "",
    Md = "Md",
    IQR = "IQR",
    M = "M",
    SD = "SD",
    Skew = "Skewness",
    Kurt = "Kurtosis",
    Min = "Min",
    Max = "Max"
  ) |> 
  tab_header(
    title = md("**Relational Coordination Scale - Item and Index Distribution**")
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())           
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()                          
  ) |> 
  
  gt::gtsave(
    filename = "table3.docx",
    path = "./eliPfad-data-publications/processing-and-analysis/results/tables"
  )


## APPENDIX C Figure 1: German Relational Coordination: item distribution #####

# Prepare data with variable labels for plotting
rl_l <- rl_data_no_na |>
  set_variable_labels(
    RC1_COM = "RC1_COM\n\"Frequent Communication\"",
    RC2_COM = "RC2_COM\n\"Timely Communication\"",
    RC3_COM = "RC3_COM\n\"Accurate Communication\"",
    RC4_COM = "RC4_COM\n\"Problem Solving Communication\"",
    RC5_REL = "RC5_REL\n\"Shared Goals\"",
    RC6_REL = "RC6_REL\n\"Shared Knowledge\"",
    RC7_REL = "RC7_REL\n\"Mutual Respect\""
  )
labels_RC_l <- sapply(rl_l[, c(
  "RC1_COM", "RC2_COM", "RC3_COM", "RC4_COM",
  "RC5_REL", "RC6_REL", "RC7_REL"
)], var_label)

label_order_l <- labels_RC_l[c(
  "RC1_COM", "RC2_COM", "RC3_COM", "RC4_COM",
  "RC5_REL", "RC6_REL", "RC7_REL"
)]

# Create figure
RC_figure <-
  rl_l |>
  select(RC1_COM:RC7_REL) |>
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Antwort") |>
  filter(!is.na(Antwort)) |>
  mutate(
    Item_code = as.character(Item),
    Item = labels_RC_l[Item],
    Item = fct_relevel(Item, rev(label_order_l)),
  ) |>
  count(Item, Antwort, name = "n") |>
  group_by(Item) |>
  mutate(
    Prozent = n / sum(n),
    Prozent_label = paste0(round(Prozent * 100, 1), "%")
  ) |>
  ggplot(aes(x = Item, y = Prozent, fill = factor(Antwort, levels = 5:1))) +
  geom_col(position = "fill", na.rm = TRUE) +
  geom_text(
    aes(label = Prozent_label),
    position = position_fill(vjust = 0.5),
    size = 4.5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#317794", "#8baebc", "grey90", "#F2A6A5", "#FF6665"),
    name = "Value"
  ) +
  labs(
    x = "Relational Coordination Survey items",
    y = "Percentage of responses"
  ) +  
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 14),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12)
  )+
  guides(fill = guide_legend(reverse =TRUE))

ggsave("./eliPfad-data-publications/processing-and-analysis/results/figures/RC_labels.png",
       plot = RC_figure, width = 50, height = 25, units = "cm", dpi = 300
)


# Confirmatory Factor Analysis #####


## Model specification #####

# 1-factor model
RC_val_model <-
  'RC_sco =~ RC1_COM + RC2_COM + RC3_COM + RC4_COM + RC5_REL + RC6_REL + RC7_REL'

# Hierarchical 2. order model 
RC_val_model2_2 <- '
  RC_c =~ RC1_COM + RC2_COM + RC3_COM + RC4_COM
  RC_r =~ RC5_REL + RC6_REL + RC7_REL
  RC_sco =~ 1*RC_c + 1*RC_r' # equalizing of factor loadings -> model identification

## Model estimation ####

# 1-factor model

RC_val_fit <- cfa(RC_val_model, data = rl_data_sem,
                  ordered = c("RC1_COM", "RC2_COM", "RC3_COM", "RC4_COM", "RC5_REL", "RC6_REL", "RC7_REL"),
                  , std.lv = TRUE)

# Hierarchical 2. order model

RC_val_fit2_2 <- cfa(RC_val_model2_2, 
                     ordered = c("RC1_COM", "RC2_COM", "RC3_COM", "RC4_COM", "RC5_REL", "RC6_REL", "RC7_REL"), 
                     data = rl_data_sem) # std.lv = TRUE)


## Model evaluation ####

# 1-factor model

summary(RC_val_fit, fit.measures = TRUE, standardized = TRUE)

standardizedsolution(RC_val_fit, level = .90) 

# Hierarchical 2. order model

summary(RC_val_fit2_2, fit.measures = TRUE, standardized = TRUE)

standardizedsolution(RC_val_fit2_2, level = .90) # 90%-CI for standardized parameter

## Model comparison chi² #####

lavTestLRT(RC_val_fit, RC_val_fit2_2)

# Convergent validity ####

## Average Variance Extracted (AVE) ####

semTools::AVE(RC_val_fit2_2)

# Bootstrap for AVE 90% CIs 

boot_fun_AVE <- function(x){ # returns AVE values for all factors
  AVE(x)  
}

boot_AVE <- lavaan::bootstrapLavaan( # Bootstrapping of AVE
  RC_val_fit2_2,   
  FUN = boot_fun_AVE,
  R = 10000
)

# Display AVE 90%-Bootstrap-CIs
apply(boot_AVE, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE)


# Discriminant validity ####

## Shared variance between subdimensions Communication and Relation ####

# CFA 2-factor model to estimate correlation (shared variance between RC_c and RC_r within CFA-framework
# Note: model is necessary to obtain robust standard errors and CIs for correlation and differences between AVE and r²

RC_val_model2 <-
  'RC_c =~ RC1_COM + RC2_COM + RC3_COM + RC4_COM
RC_r =~ RC5_REL + RC6_REL + RC7_REL
RC_c ~~ RC_r
'

RC_val_fit2 <- cfa(RC_val_model2,
                   ordered = c("RC1_COM", "RC2_COM", "RC3_COM", "RC4_COM", "RC5_REL", "RC6_REL", "RC7_REL"),
                   data = rl_data_sem, std.lv = TRUE)

summary(RC_val_fit2, fit.measures = TRUE) # model estimation is identical to 2nd order model

# Display 90%-CI for correlation (rho) between RC_c and RC_r

standardizedsolution(RC_val_fit2, level = .90) 


## Differences between AVE and r² ####

# Function to calculate AVE, r² and their differences to estimate 90% CIs via bootstrapping

get_AVE_r2_diff <- function(fit) {
  
  loadings <- standardizedSolution(fit)
  
  # Loadings only (faktor -> indicator)
  load_c <- subset(loadings, lhs == "RC_c" & op == "=~")
  load_r <- subset(loadings, lhs == "RC_r" & op == "=~")
  
  # Calculate AVE 
  AVE_c <- mean(load_c$est.std^2)
  AVE_r <- mean(load_r$est.std^2)
  
  # Latent correlation
  rho <- subset(loadings, lhs == "RC_c" & rhs == "RC_r" & op == "~~")$est.std
  r2 <- rho^2
  
  # estimate Differences
  diff_c <- AVE_c - r2
  diff_r <- AVE_r - r2
  
  return(c(AVE_c = AVE_c, AVE_r = AVE_r, r2 = r2, 
           diff_c = diff_c, diff_r = diff_r))
}

# Bootstrap for AVE, r² and their differences to estimate 90% CIs
boot_res <- bootstrapLavaan(
  RC_val_fit2,
  R = 10000,
  FUN = get_AVE_r2_diff
)

# Display bootstrapped estimates with 90%-CIs
apply(boot_res, 2, function(x) quantile(x, probs = c(0.05, 0.5, 0.95), na.rm = TRUE))



# Reliability ####

# Calculate composite reliability for hierarchical 2. order model

semTools::compRelSEM(RC_val_fit2_2, higher = "RC_sco", tau.eq = FALSE)


# Bootstrap function for composite reliability 90% CIs
boot_fun_relSEM2 <- function(x) {
  compRelSEM(x, higher = "RC_sco")
}

# Bootstrap for composite reliability 90% CIs
boot_relSEM2 <- bootstrapLavaan(
  RC_val_fit2_2,
  FUN = boot_fun_relSEM2,
  R = 10000,
)

# Display composite reliability 90%-Bootstrap-CIs
apply(boot_relSEM2, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE)

