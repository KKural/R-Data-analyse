# ============================================================================
# SCRIPT: Code Raw Data from SPSS to Proper R Format
# ============================================================================
# Purpose: Load raw SPSS file, code all variables (Types 1-6), save as CSV/Excel
# Dataset: Corona & Welzijn (C&W) Wave 1
# Output: Coded data ready for cleaning and analysis
# ============================================================================

# Load required packages
library(haven)      # Read SPSS files
library(readr)      # Write CSV
library(writexl)    # Write Excel
library(dplyr)      # Data manipulation

# ============================================================================
# STEP 1: LOAD RAW SPSS DATA
# ============================================================================

# Load the raw SPSS file - WAVE 1 ONLY
ruwe_covid_data <- haven::read_sav("Data/DATA_WAVE1_CenW.sav")

# Inspect the raw data
str(ruwe_covid_data[1:15])  # First 15 variables

# ============================================================================
# STEP 2: CODE ALL 67 WAVE 1 VARIABLES BY TYPE
# ============================================================================

# ---- TYPE 1: NOMINALE VARIABELEN (No ordering) ----

# W1_Geslacht: 0=Man, 1=Vrouw
ruwe_covid_data$geslacht <- factor(ruwe_covid_data$W1_Geslacht,
                                   levels = c(0, 1),
                                   labels = c("Man", "Vrouw"))

# W1_Relatiestatus: 0=Nee, 1=Ja
ruwe_covid_data$relatiestatus <- factor(ruwe_covid_data$W1_Relatiestatus,
                                        levels = c(0, 1),
                                        labels = c("Geen relatie", "In relatie"))

# W1_Nationaliteit: 1=België geboren beide ouders, 2=België één ouder, 3=Niet België geboren
ruwe_covid_data$nationaliteit <- factor(ruwe_covid_data$W1_Nationaliteit,
                                        levels = c(1, 2, 3),
                                        labels = c("België (beide ouders)",
                                                  "België (één ouder)",
                                                  "Niet België geboren"))

# W1_Burg_staat: 1=Ongehuwd, 2=Gehuwd, 3=Weduwe/weduwnaar, 4=Gescheiden
ruwe_covid_data$burgerlijke_staat <- factor(ruwe_covid_data$W1_Burg_staat,
                                            levels = c(1, 2, 3, 4),
                                            labels = c("Ongehuwd", "Gehuwd", 
                                                      "Weduwe/Weduwnaar", "Gescheiden"))

# W1_Ouder: 0=Nee, 1=Ja
ruwe_covid_data$ouder <- factor(ruwe_covid_data$W1_Ouder,
                                levels = c(0, 1),
                                labels = c("Geen kinderen", "Ouder"))

# W1_Handicap: 0=Nee, 1=Ja
ruwe_covid_data$handicap <- factor(ruwe_covid_data$W1_Handicap,
                                   levels = c(0, 1),
                                   labels = c("Geen handicap", "Heeft handicap"))

# W1_W_ICT: 0=Nee, 1=Ja
ruwe_covid_data$werk_ict <- factor(ruwe_covid_data$W1_W_ICT,
                                   levels = c(0, 1),
                                   labels = c("Geen ICT", "Gebruik ICT"))

# W1_W_ICTDeel: 0=Nee, 1=Ja
ruwe_covid_data$werk_ict_delen <- factor(ruwe_covid_data$W1_W_ICTDeel,
                                         levels = c(0, 1),
                                         labels = c("Niet delen", "Delen apparaat"))

# W1_Contact: 0=Nee, 1=Ja
ruwe_covid_data$contact_gewenst <- factor(ruwe_covid_data$W1_Contact,
                                          levels = c(0, 1),
                                          labels = c("Geen contact", "Contact gewenst"))

# ---- TYPE 2: ORDINALE VARIABELEN (With ordering) ----

# W1_Diploma: 1=Geen, 2=Middelbaar, 3=Hoger
ruwe_covid_data$diploma <- factor(ruwe_covid_data$W1_Diploma,
                                  levels = c(1, 2, 3),
                                  labels = c("Geen/Lager", "Middelbaar", "Hoger onderwijs"),
                                  ordered = TRUE)

# W1_ACT: 1=Studeert thuis, 2=Werkt thuis, 3=Werkt op werkplek
ruwe_covid_data$activiteit <- factor(ruwe_covid_data$W1_ACT,
                                     levels = c(1, 2, 3),
                                     labels = c("Studeert thuis", "Werkt thuis", "Werkt op werkplek"),
                                     ordered = FALSE)

# W1_Eigen_ruimte: 1=Ja zeer zeker, 2=Ja beetje, 3=Nee niet echt, 4=Nee helemaal niet
ruwe_covid_data$eigen_ruimte <- factor(ruwe_covid_data$W1_Eigen_ruimte,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Ja zeer zeker", "Ja beetje",
                                                 "Nee niet echt", "Nee helemaal niet"),
                                       ordered = TRUE)

# W1_Gezondheid: 1=Slecht, 2=Redelijk, 3=Goed, 4=Erg goed, 5=Zeer goed
ruwe_covid_data$gezondheid <- factor(ruwe_covid_data$W1_Gezondheid,
                                     levels = c(1, 2, 3, 4, 5),
                                     labels = c("Slecht", "Redelijk", "Goed", "Erg goed", "Zeer goed"),
                                     ordered = TRUE)

# W1_Corona: 1=Geen symptomen, 2=Symptomen geen test, 3=Symptomen test negatief, 4=Positief
ruwe_covid_data$corona_status <- factor(ruwe_covid_data$W1_Corona,
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Geen symptomen", "Symptomen geen test",
                                                  "Test negatief", "Test positief"))

# ---- TYPE 3: RATIO/INTERVAL VARIABELEN (Numeric calculations) ----

# Calculate age from birth year (2024 - birth year)
ruwe_covid_data$leeftijd <- 2024 - ruwe_covid_data$W1_Gebjaar

# W1_RESID01: Aantal personen in huishouden (keep as numeric)
ruwe_covid_data$huishouden_personen <- ruwe_covid_data$W1_RESID01

# W1_Kinderen: Aantal kinderen (keep as numeric)
ruwe_covid_data$aantal_kinderen <- ruwe_covid_data$W1_Kinderen

# W1_Soc_kap: Aantal personen voor belangrijke gesprekken (keep as numeric)
ruwe_covid_data$sociaal_kapitaal <- ruwe_covid_data$W1_Soc_kap

# ---- TYPE 4: LIKERT SCHAAL VARIABELEN (Treat as ordered factors) ----

# ANXIETY ITEMS (W1_Angst1-6): 1=Helemaal niet, 2=Minder helft, 3=Meer helft, 4=Bijna elke dag
for (i in 1:6) {
  var_name <- paste0("W1_Angst", i)
  new_name <- paste0("angst", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Helemaal niet", "Minder dan helft",
                                                  "Meer dan helft", "Bijna elke dag"),
                                        ordered = TRUE)
}

# DEPRESSION ITEMS (W1_Depressie1-3): 1=Zelden, 2=Soms/weinig, 3=Regelmatig, 4=Meestal/altijd
for (i in 1:3) {
  var_name <- paste0("W1_Depressie", i)
  new_name <- paste0("depressie", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Zelden/nooit", "Soms/weinig",
                                                  "Regelmatig", "Meestal/altijd"),
                                        ordered = TRUE)
}

# WORK STRESS ITEMS (W1_W_STRESS1-4): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:4) {
  var_name <- paste0("W1_W_STRESS", i)
  new_name <- paste0("werk_stress", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# WORK CONCENTRATION ITEMS (W1_W_CONC1-3): 1=Veel minder, 2=Minder, 3=Net zoveel, 4=Meer, 5=Veel meer
for (i in 1:3) {
  var_name <- paste0("W1_W_CONC", i)
  new_name <- paste0("werk_concentratie", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Veel minder", "Minder", "Net zoveel", "Meer", "Veel meer"),
                                        ordered = TRUE)
}

# LONELINESS ITEMS (W1_Eenz1-4): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:4) {
  var_name <- paste0("W1_Eenz", i)
  new_name <- paste0("eenzaamheid", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# CORONA STRESS ITEMS (W1_Corstress1-3): 1=Niet akkoord, 2=Eerder niet, 3=Noch/noch, 4=Eerder, 5=Zeer akkoord
for (i in 1:3) {
  var_name <- paste0("W1_Corstress", i)
  new_name <- paste0("corona_stress", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# VERBAL AGGRESSION ITEMS (W1_VerbAgr1-3): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:3) {
  var_name <- paste0("W1_VerbAgr", i)
  new_name <- paste0("verbale_agressie", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# RELATIONSHIP SATISFACTION ITEMS (W1_QMI1-5): 1=Niet akkoord, 2=Eerder niet, 3=Noch/noch, 4=Eerder, 5=Zeer akkoord
for (i in 1:5) {
  var_name <- paste0("W1_QMI", i)
  new_name <- paste0("relatie_tevredenheid", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# RELATIONSHIP STRESS - PAST 7 DAYS (W1_Relstress1_1-4): 1=Niet stressvol tot 5=Zeer stressvol
for (i in 1:4) {
  var_name <- paste0("W1_Relstress1_", i)
  new_name <- paste0("relatie_stress_7d", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet stressvol", "Beetje", "Matig",
                                                  "Behoorlijk", "Zeer stressvol"),
                                        ordered = TRUE)
}

# RELATIONSHIP STRESS - PAST 12 MONTHS (W1_Relstress2_1-4): 1=Niet stressvol tot 5=Zeer stressvol
for (i in 1:4) {
  var_name <- paste0("W1_Relstress2_", i)
  new_name <- paste0("relatie_stress_12m", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet stressvol", "Beetje", "Matig",
                                                  "Behoorlijk", "Zeer stressvol"),
                                        ordered = TRUE)
}

# FINANCIAL STRESS (W1_FINSTRESS1-3): 1=Niet akkoord tot 5=Zeer akkoord
for (i in 1:3) {
  var_name <- paste0("W1_FINSTRESS", i)
  new_name <- paste0("financiele_stress", i)
  ruwe_covid_data[[new_name]] <- factor(ruwe_covid_data[[var_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# ---- TYPE 5: MISSING VALUES HANDLING ----

# Most variables already handled by haven::read_sav() which respects SPSS NA structure
# W1_Relduur: Relationship duration (keep as numeric, has NA for no relationship)
ruwe_covid_data$relduur <- ruwe_covid_data$W1_Relduur

# W1_Leefsit: Living situation with partner (only for those in relationship)
ruwe_covid_data$leefsituatie <- factor(ruwe_covid_data$W1_Leefsit,
                                       levels = c(1, 2, 3),
                                       labels = c("Samen wonend", "Deeltijds samen",
                                                 "Niet samen wonend"))

# W1_ACT_partner: Partner's activity status
ruwe_covid_data$partner_activiteit <- factor(ruwe_covid_data$W1_ACT_partner,
                                             levels = c(1, 2, 3, 4, 5, 6),
                                             labels = c("Studeert", "Werkt thuis",
                                                       "Werkt op werkplek", "Werkloos",
                                                       "Huiswerk/zorg", "Ander"))

# W1_Gesl_Partner: Partner gender (only for those with partners)
ruwe_covid_data$partner_geslacht <- factor(ruwe_covid_data$W1_Gesl_Partner,
                                           levels = c(0, 1),
                                           labels = c("Man", "Vrouw"))

# W1_Rel_Tevreden: Relationship happiness scale (1-10, keep numeric)
ruwe_covid_data$relatie_geluk <- ruwe_covid_data$W1_Rel_Tevreden

# W1_Inkomen: Household income (categorical, 18 levels)
ruwe_covid_data$huishouden_inkomen <- factor(ruwe_covid_data$W1_Inkomen,
                                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
                                             labels = c("< €500", "€500-€999", "€1000-€1499",
                                                       "€1500-€1999", "€2000-€2499", "€2500-€2999",
                                                       "€3000-€3499", "€3500-€3999", "€4000-€4499",
                                                       "€4500-€4999", "€5000-€5499", "€5500-€5999",
                                                       "€6000-€6499", "€6500-€6999", "€7000-€7499",
                                                       "€7500-€7999", "€8000-€8499", "> €8500"))

# ---- TYPE 6: PREPARE FOR SAVING ----

# Rename to covid_data for final output
covid_data <- ruwe_covid_data

# Convert all factors to character (labels only, no numeric codes)
covid_data <- covid_data %>%
  mutate(across(where(is.factor), as.character))

# ============================================================================
# STEP 3: INSPECT CODED DATA
# ============================================================================

# Check the structure of coded data
str(covid_data[1:20])

# Check frequency tables of key coded factors
table(covid_data$geslacht, useNA = "always")
table(covid_data$diploma, useNA = "always")
table(covid_data$gezondheid, useNA = "always")
table(covid_data$activiteit, useNA = "always")

# Summary statistics
summary(covid_data$leeftijd)
summary(covid_data$sociaal_kapitaal)

