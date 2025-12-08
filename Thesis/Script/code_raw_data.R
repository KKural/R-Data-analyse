# ============================================================================
# SCRIPT: code_raw_data.R - Codeer Ruwe Data naar Juist R Formaat
# ============================================================================
# Doel: Laad ruwe data (Excel bestand), codeer alle variabelen (Types 1-6), 
#       sla op als CSV/Excel
# Dataset: Corona & Welzijn (C&W) Golf 1
# Gegevensbron: Excel bestand geëxporteerd uit SPSS met labels + waarden
#               (Vergelijkbaar met Google Forms, Microsoft Forms, websurvey output)
# Output: Gecodeerde data klaar voor cleaning en analyse
# ============================================================================

# Benodigde packages laden
library(readxl)     # Lees Excel bestanden
library(readr)      # Schrijf CSV
library(writexl)    # Schrijf Excel
library(dplyr)      # Data manipulatie

# ============================================================================
# STAP 1: LAAD RUWE DATA UIT EXCEL
# ============================================================================

# HOE KRIJG JE DATA TOT DIT PUNT:
# 1. Google Forms → Auto export naar Excel/CSV → Download
# 2. Microsoft Forms → Export naar Excel
# 3. LimeSurvey → Export naar Excel
# 4. SPSS → File → Save As → Excel (xlsx) met value labels aangevinkt
# 5. Handmatige data entry → Maak kolommen in Excel, sla op als xlsx
#
# Al deze methoden maken een Excel bestand met nummerieke codes (0, 1, 2, etc.)
# En je moet deze nummers in R CODEREN naar betekenisvolle labels

# Laad het ruwe Excel bestand - GOLF 1 ALLEEN
# Dit bestand bevat ruwe nummerieke codes: 0=Man/1=Vrouw, etc.
ruwe_covid_data <- readxl::read_excel("Data/DATA_WAVE1_CenW - Raw.xlsx")

# Converteer naar reguliere data frame (verwijdert Excel-specifieke attributen)
ruwe_covid_data <- as.data.frame(ruwe_covid_data)

# Inspecteer de structuur van ruwe data
# Je ziet kolommen als W1_Geslacht met waarden: 0, 1, 0, 1, 0... (nummerieke codes)
str(ruwe_covid_data[1:15])  # Eerste 15 variabelen

# ============================================================================
# STAP 2: CODEER ALLE 67 GOLF 1 VARIABELEN PER TYPE
# ============================================================================

# ---- TYPE 1: NOMINALE VARIABELEN (Geen ordening) ----

# W1_Geslacht: 0=Man, 1=Vrouw
ruwe_covid_data$W1_Geslacht <- factor(ruwe_covid_data$W1_Geslacht,
                                   levels = c(0, 1),
                                   labels = c("Man", "Vrouw"))

# W1_Relatiestatus: 0=Nee, 1=Ja
ruwe_covid_data$W1_Relatiestatus <- factor(ruwe_covid_data$W1_Relatiestatus,
                                        levels = c(0, 1),
                                        labels = c("Geen relatie", "In relatie"))

# W1_Nationaliteit: 1=België geboren beide ouders, 2=België één ouder, 3=Niet België geboren
ruwe_covid_data$W1_Nationaliteit <- factor(ruwe_covid_data$W1_Nationaliteit,
                                        levels = c(1, 2, 3),
                                        labels = c("België (beide ouders)",
                                                  "België (één ouder)",
                                                  "Niet België geboren"))

# W1_Burg_staat: 1=Ongehuwd, 2=Gehuwd, 3=Weduwe/weduwnaar, 4=Gescheiden
ruwe_covid_data$W1_Burg_staat <- factor(ruwe_covid_data$W1_Burg_staat,
                                            levels = c(1, 2, 3, 4),
                                            labels = c("Ongehuwd", "Gehuwd", 
                                                      "Weduwe/Weduwnaar", "Gescheiden"))

# W1_Ouder: 0=Nee, 1=Ja
ruwe_covid_data$W1_Ouder <- factor(ruwe_covid_data$W1_Ouder,
                                levels = c(0, 1),
                                labels = c("Geen kinderen", "Ouder"))

# W1_Handicap: 0=Nee, 1=Ja
ruwe_covid_data$W1_Handicap <- factor(ruwe_covid_data$W1_Handicap,
                                   levels = c(0, 1),
                                   labels = c("Geen handicap", "Heeft handicap"))

# W1_W_ICT: 0=Nee, 1=Ja
ruwe_covid_data$W1_W_ICT <- factor(ruwe_covid_data$W1_W_ICT,
                                   levels = c(0, 1),
                                   labels = c("Geen ICT", "Gebruik ICT"))

# W1_W_ICTDeel: 0=Nee, 1=Ja
ruwe_covid_data$W1_W_ICTDeel <- factor(ruwe_covid_data$W1_W_ICTDeel,
                                         levels = c(0, 1),
                                         labels = c("Niet delen", "Delen apparaat"))

# W1_Contact: 0=Nee, 1=Ja
ruwe_covid_data$W1_Contact <- factor(ruwe_covid_data$W1_Contact,
                                          levels = c(0, 1),
                                          labels = c("Geen contact", "Contact gewenst"))

# ---- TYPE 2: ORDINALE VARIABELEN (Met ordening) ----

# W1_Diploma: 1=Geen, 2=Middelbaar, 3=Hoger
ruwe_covid_data$W1_Diploma <- factor(ruwe_covid_data$W1_Diploma,
                                  levels = c(1, 2, 3),
                                  labels = c("Geen/Lager", "Middelbaar", "Hoger onderwijs"),
                                  ordered = TRUE)

# W1_ACT: 1=Studeert thuis, 2=Werkt thuis, 3=Werkt op werkplek
ruwe_covid_data$W1_ACT <- factor(ruwe_covid_data$W1_ACT,
                                     levels = c(1, 2, 3),
                                     labels = c("Studeert thuis", "Werkt thuis", "Werkt op werkplek"),
                                     ordered = FALSE)

# W1_Eigen_ruimte: 1=Ja zeer zeker, 2=Ja beetje, 3=Nee niet echt, 4=Nee helemaal niet
ruwe_covid_data$W1_Eigen_ruimte <- factor(ruwe_covid_data$W1_Eigen_ruimte,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Ja zeer zeker", "Ja beetje",
                                                 "Nee niet echt", "Nee helemaal niet"),
                                       ordered = TRUE)

# W1_Gezondheid: 1=Slecht, 2=Redelijk, 3=Goed, 4=Erg goed, 5=Zeer goed
ruwe_covid_data$W1_Gezondheid <- factor(ruwe_covid_data$W1_Gezondheid,
                                     levels = c(1, 2, 3, 4, 5),
                                     labels = c("Slecht", "Redelijk", "Goed", "Erg goed", "Zeer goed"),
                                     ordered = TRUE)

# W1_Corona: 1=Geen symptomen, 2=Symptomen geen test, 3=Symptomen test negatief, 4=Positief
ruwe_covid_data$W1_Corona <- factor(ruwe_covid_data$W1_Corona,
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Geen symptomen", "Symptomen geen test",
                                                  "Test negatief", "Test positief"))

# ---- TYPE 3: RATIO/INTERVAL VARIABELEN (Numerieke berekeningen) ----

# Bereken leeftijd uit geboortejaar (2024 - geboortejaar)
ruwe_covid_data$W1_Leeftijd <- 2024 - ruwe_covid_data$W1_Gebjaar

# W1_RESID01: Aantal personen in huishouden (houd als numeriek)
# (al numeriek, geen verandering nodig)

# W1_Kinderen: Aantal kinderen (houd als numeriek)
# (al numeriek, geen verandering nodig)

# W1_Soc_kap: Aantal personen voor belangrijke gesprekken (houd als numeriek)
# (al numeriek, geen verandering nodig)

# ---- TYPE 4: LIKERT SCHAAL VARIABELEN (Behandel als geordende factors) ----

# ANGST ITEMS (W1_Angst1-6): 1=Helemaal niet, 2=Minder helft, 3=Meer helft, 4=Bijna elke dag
for (i in 1:6) {
  col_name <- paste0("W1_Angst", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Helemaal niet", "Minder dan helft",
                                                  "Meer dan helft", "Bijna elke dag"),
                                        ordered = TRUE)
}

# DEPRESSIE ITEMS (W1_Depressie1-3): 1=Zelden, 2=Soms/weinig, 3=Regelmatig, 4=Meestal/altijd
for (i in 1:3) {
  col_name <- paste0("W1_Depressie", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Zelden/nooit", "Soms/weinig",
                                                  "Regelmatig", "Meestal/altijd"),
                                        ordered = TRUE)
}

# WERKSTRESS ITEMS (W1_W_STRESS1-4): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:4) {
  col_name <- paste0("W1_W_STRESS", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# WERKCONCENTRATIE ITEMS (W1_W_CONC1-3): 1=Veel minder, 2=Minder, 3=Net zoveel, 4=Meer, 5=Veel meer
for (i in 1:3) {
  col_name <- paste0("W1_W_CONC", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Veel minder", "Minder", "Net zoveel", "Meer", "Veel meer"),
                                        ordered = TRUE)
}

# EENZAAMHEID ITEMS (W1_Eenz1-4): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:4) {
  col_name <- paste0("W1_Eenz", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# CORONASTRESS ITEMS (W1_Corstress1-3): 1=Niet akkoord, 2=Eerder niet, 3=Noch/noch, 4=Eerder, 5=Zeer akkoord
for (i in 1:3) {
  col_name <- paste0("W1_Corstress", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# VERBALE AGRESSIE ITEMS (W1_VerbAgr1-3): 1=Nooit, 2=Zelden, 3=Soms, 4=Vaak, 5=Altijd
for (i in 1:3) {
  col_name <- paste0("W1_VerbAgr", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"),
                                        ordered = TRUE)
}

# RELATIEBEVREDENHEID ITEMS (W1_QMI1-5): 1=Niet akkoord, 2=Eerder niet, 3=Noch/noch, 4=Eerder, 5=Zeer akkoord
for (i in 1:5) {
  col_name <- paste0("W1_QMI", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# RELATIESTRESS - AFGELOPEN 7 DAGEN (W1_Relstress1_1-4): 1=Niet stressvol tot 5=Zeer stressvol
for (i in 1:4) {
  col_name <- paste0("W1_Relstress1_", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet stressvol", "Beetje", "Matig",
                                                  "Behoorlijk", "Zeer stressvol"),
                                        ordered = TRUE)
}

# RELATIESTRESS - AFGELOPEN 12 MAANDEN (W1_Relstress2_1-4): 1=Niet stressvol tot 5=Zeer stressvol
for (i in 1:4) {
  col_name <- paste0("W1_Relstress2_", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet stressvol", "Beetje", "Matig",
                                                  "Behoorlijk", "Zeer stressvol"),
                                        ordered = TRUE)
}

# FINANCIELE STRESS (W1_FINSTRESS1-3): 1=Niet akkoord tot 5=Zeer akkoord
for (i in 1:3) {
  col_name <- paste0("W1_FINSTRESS", i)
  ruwe_covid_data[[col_name]] <- factor(ruwe_covid_data[[col_name]],
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c("Niet akkoord", "Eerder niet",
                                                  "Noch/noch", "Eerder", "Zeer akkoord"),
                                        ordered = TRUE)
}

# ---- TYPE 5: ONTBREKENDE WAARDEN BEHANDELING ----

# W1_Relduur: Relatieduur (houd als numeriek)
# (al numeriek, geen verandering nodig)

# W1_Leefsit: Woonplaats met partner (alleen voor mensen in relatie)
ruwe_covid_data$W1_Leefsit <- factor(ruwe_covid_data$W1_Leefsit,
                                       levels = c(1, 2, 3),
                                       labels = c("Samen wonend", "Deeltijds samen",
                                                 "Niet samen wonend"))

# W1_ACT_partner: Activiteitsstatus partner
ruwe_covid_data$W1_ACT_partner <- factor(ruwe_covid_data$W1_ACT_partner,
                                             levels = c(1, 2, 3, 4, 5, 6),
                                             labels = c("Studeert", "Werkt thuis",
                                                       "Werkt op werkplek", "Werkloos",
                                                       "Huiswerk/zorg", "Ander"))

# W1_Gesl_Partner: Geslacht partner (alleen voor mensen met partners)
ruwe_covid_data$W1_Gesl_Partner <- factor(ruwe_covid_data$W1_Gesl_Partner,
                                           levels = c(0, 1),
                                           labels = c("Man", "Vrouw"))

# W1_Rel_Tevreden: Relatiebevredenheid schaal (1-10, houd numeriek)
# (al numeriek, geen verandering nodig)

# W1_Inkomen: Huishouding inkomen (categorisch, 18 niveaus)
ruwe_covid_data$W1_Inkomen <- factor(ruwe_covid_data$W1_Inkomen,
                                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
                                             labels = c("< €500", "€500-€999", "€1000-€1499",
                                                       "€1500-€1999", "€2000-€2499", "€2500-€2999",
                                                       "€3000-€3499", "€3500-€3999", "€4000-€4499",
                                                       "€4500-€4999", "€5000-€5499", "€5500-€5999",
                                                       "€6000-€6499", "€6500-€6999", "€7000-€7499",
                                                       "€7500-€7999", "€8000-€8499", "> €8500"))

# ---- TYPE 6: VOORBEREIDING VOOR OPSLAAN ----

# Hernoem naar covid_data voor uiteindelijke output
covid_data <- ruwe_covid_data

# ============================================================================
# STAP 3: INSPECTEER GECODEERDE DATA
# ============================================================================

# Controleer de structuur van gecodeerde data
str(covid_data[1:20])

# Controleer frequentietabellen van sleutel gecodeerde factors
table(covid_data$W1_Geslacht, useNA = "always")
table(covid_data$W1_Diploma, useNA = "always")
table(covid_data$W1_Gezondheid, useNA = "always")
table(covid_data$W1_ACT, useNA = "always")

# Samenvattingsstatistieken
summary(covid_data$W1_Leeftijd)
summary(covid_data$W1_Soc_kap)

# ============================================================================
# STAP 4: SLA GECODEERDE DATA OP IN MEERDERE FORMATEN
# ============================================================================

# Maak Data folder aan als deze niet bestaat
if (!dir.exists("Data")) {
  dir.create("Data")
}

# Haal vandaag's datum op voor bestandsnaam (DD-MM-YYYY formaat)
today_date <- format(Sys.Date(), "%d%m%Y")

# OPTIE 1: Sla op als CSV (universeel, klein bestand)
readr::write_csv(covid_data, paste0("Data/", today_date, "_covid_data_coded.csv"))

# OPTIE 2: Sla op als Excel (leesbaar, visuele inspectie mogelijk)
writexl::write_xlsx(covid_data, paste0("Data/", today_date, "_covid_data_coded.xlsx"))

# OPTIE 3: Sla op als SPSS (compatibel met SPSS gebruikers)
haven::write_sav(covid_data, paste0("Data/", today_date, "_covid_data_coded.sav"))
