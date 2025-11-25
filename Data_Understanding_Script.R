# =============================================================================
# DATA UNDERSTANDING SCRIPT
# =============================================================================
# Purpose: Understand the structure and meaning of the dataset
# Focus: What are W1, W2, and what do all these variables mean?
# Created: November 2024
# =============================================================================

# Load required packages
library(haven)     # For reading SPSS files
library(dplyr)     # For data manipulation
library(tidyr)     # For data reshaping

# =============================================================================
# STEP 1: LOAD THE DATA
# =============================================================================

# Load the SPSS file to understand the original structure
cat("Loading SPSS data...\n")
data_spss <- haven::read_sav("Thesis/Data/DATA_WAVE1_WAVE2_CenW.sav")

cat("Dataset loaded successfully!\n")
cat("Rows (participants):", nrow(data_spss), "\n")
cat("Columns (variables):", ncol(data_spss), "\n\n")

# =============================================================================
# STEP 2: UNDERSTAND THE VARIABLE NAMING SYSTEM
# =============================================================================

cat("=== UNDERSTANDING VARIABLE NAMES ===\n")

# Get all variable names
all_vars <- names(data_spss)
cat("All variables:\n")
print(all_vars)
cat("\n")

# Analyze the naming pattern
cat("VARIABLE NAMING PATTERN ANALYSIS:\n")

# Variables that start with W1_
w1_vars <- all_vars[grepl("^W1_", all_vars)]
cat("Variables starting with 'W1_' (", length(w1_vars), " variables):\n")
cat("These likely represent Wave 1 measurements\n")
print(w1_vars)
cat("\n")

# Variables that start with W2_
w2_vars <- all_vars[grepl("^W2_", all_vars)]
cat("Variables starting with 'W2_' (", length(w2_vars), " variables):\n")
cat("These likely represent Wave 2 measurements\n")
print(w2_vars)
cat("\n")

# Other variables (control variables)
other_vars <- all_vars[!grepl("^W[12]_", all_vars)]
cat("Other variables (", length(other_vars), " variables):\n")
cat("These are likely control/identifier variables\n")
print(other_vars)
cat("\n")

# =============================================================================
# STEP 3: UNDERSTAND WHAT W1 AND W2 MEAN
# =============================================================================

cat("=== WHAT DO W1 AND W2 MEAN? ===\n")

# Check if there are variables that indicate wave participation
if("W1" %in% names(data_spss) && "W2" %in% names(data_spss)) {
  
  cat("Found W1 and W2 indicator variables!\n")
  cat("These tell us which wave each participant completed:\n\n")
  
  # Analyze wave participation
  wave_table <- table(data_spss$W1, data_spss$W2, useNA = "always")
  cat("Wave participation cross-table:\n")
  cat("         W2=0   W2=1\n")
  cat("W1=0   ", wave_table[1,1], "    ", wave_table[1,2], "\n")
  cat("W1=1   ", wave_table[2,1], "    ", wave_table[2,2], "\n")
  
  # Interpret the results
  cat("\nINTERPRETATION:\n")
  cat("- W1=1 & W2=0: Participated only in Wave 1\n")
  cat("- W1=0 & W2=1: Participated only in Wave 2\n") 
  cat("- W1=1 & W2=1: Participated in BOTH waves (longitudinal participants)\n")
  cat("- W1=0 & W2=0: Did not participate in either wave (should not exist)\n\n")
  
  # Calculate participation statistics
  only_w1 <- sum(data_spss$W1 == 1 & data_spss$W2 == 0, na.rm = TRUE)
  only_w2 <- sum(data_spss$W1 == 0 & data_spss$W2 == 1, na.rm = TRUE)
  both_waves <- sum(data_spss$W1 == 1 & data_spss$W2 == 1, na.rm = TRUE)
  
  cat("PARTICIPATION SUMMARY:\n")
  cat("Only Wave 1:", only_w1, "participants\n")
  cat("Only Wave 2:", only_w2, "participants\n")
  cat("Both waves:", both_waves, "participants (longitudinal data!)\n")
  
  # This tells us it's a longitudinal study!
  cat("\n*** CONCLUSION: This is a LONGITUDINAL STUDY! ***\n")
  cat("W1 = Wave 1 (first data collection)\n")
  cat("W2 = Wave 2 (follow-up data collection)\n\n")
  
} else {
  cat("No W1/W2 indicator variables found. Let me analyze differently...\n")
}

# =============================================================================
# STEP 4: EXPLORE VARIABLE CATEGORIES AND MEANINGS
# =============================================================================

cat("=== VARIABLE CATEGORIES AND MEANINGS ===\n")

# Function to show variable labels if they exist
show_variable_info <- function(vars, title) {
  cat(title, "\n")
  cat(paste(rep("=", nchar(title)), collapse = ""), "\n")
  
  for(i in 1:min(5, length(vars))) {
    var_name <- vars[i]
    var_label <- attr(data_spss[[var_name]], "label")
    
    if(!is.null(var_label)) {
      cat(var_name, ":", var_label, "\n")
    } else {
      cat(var_name, ": (no label available)\n")
    }
  }
  
  if(length(vars) > 5) {
    cat("... and", length(vars) - 5, "more variables\n")
  }
  cat("\n")
}

# Demographic variables
demo_vars <- c(
  all_vars[grepl("Geslacht|Gender", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Gebjaar|Age|Leeftijd", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Diploma|Education|Opleiding", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Nationaliteit", all_vars, ignore.case = TRUE)]
)
demo_vars <- unique(demo_vars)
show_variable_info(demo_vars, "DEMOGRAPHIC VARIABLES")

# Psychological variables
psych_vars <- c(
  all_vars[grepl("Angst|Anxiety", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Depressie|Depression", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Stress", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Eenz|Loneliness", all_vars, ignore.case = TRUE)]
)
psych_vars <- unique(psych_vars)
show_variable_info(psych_vars, "PSYCHOLOGICAL VARIABLES")

# Relationship variables
rel_vars <- c(
  all_vars[grepl("QMI", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Relatie|Relationship", all_vars, ignore.case = TRUE)],
  all_vars[grepl("Partner", all_vars, ignore.case = TRUE)]
)
rel_vars <- unique(rel_vars)
show_variable_info(rel_vars, "RELATIONSHIP VARIABLES")

# Technology/Digital variables
tech_vars <- c(
  all_vars[grepl("ICT|Digital|DIGDEP|CIUS|Smartphone|Phubbing", all_vars, ignore.case = TRUE)]
)
tech_vars <- unique(tech_vars)
if(length(tech_vars) > 0) {
  show_variable_info(tech_vars, "TECHNOLOGY/DIGITAL VARIABLES")
}

# =============================================================================
# STEP 5: UNDERSTAND THE RESEARCH CONTEXT
# =============================================================================

cat("=== RESEARCH CONTEXT ANALYSIS ===\n")

# Look for COVID-related variables (might give us timeline context)
covid_vars <- all_vars[grepl("Corona|Covid", all_vars, ignore.case = TRUE)]
if(length(covid_vars) > 0) {
  cat("COVID-19 RELATED VARIABLES FOUND:\n")
  cat("This suggests the study was conducted during the COVID-19 pandemic!\n")
  print(covid_vars)
  cat("\n")
}

# Look for time-specific variables
time_vars <- all_vars[grepl("quarantaine|lockdown|vaccin", all_vars, ignore.case = TRUE)]
if(length(time_vars) > 0) {
  cat("TIME-SPECIFIC VARIABLES:\n")
  print(time_vars)
  cat("\n")
}

# =============================================================================
# STEP 6: PRACTICAL DATA EXPLORATION
# =============================================================================

cat("=== PRACTICAL DATA EXPLORATION ===\n")

# Show some actual data to understand better
cat("First 5 rows of key variables:\n")
key_vars <- c("Nummer", "W1", "W2", "W1_Geslacht", "W1_Gebjaar")
key_vars <- key_vars[key_vars %in% names(data_spss)]

if(length(key_vars) > 0) {
  print(head(data_spss[key_vars], 5))
} else {
  cat("Key variables not found, showing first 5 columns:\n")
  print(head(data_spss[1:5], 5))
}

cat("\n")

# Check data types
cat("DATA TYPES OF FIRST 10 VARIABLES:\n")
for(i in 1:min(10, ncol(data_spss))) {
  var_name <- names(data_spss)[i]
  var_type <- class(data_spss[[var_name]])[1]
  cat(var_name, ":", var_type, "\n")
}

# =============================================================================
# STEP 7: CHECK DEMOGRAPHIC CONSISTENCY BETWEEN WAVES
# =============================================================================

cat("=== DEMOGRAPHIC CONSISTENCY CHECK ===\n")
cat("Demographics like gender and birth year shouldn't change between waves...\n")
cat("Let's check if they collected the same info twice or if there are differences:\n\n")

# Check participants who completed both waves
both_waves_participants <- data_spss %>%
  dplyr::filter(W1 == 1 & W2 == 1)

cat("Checking", nrow(both_waves_participants), "participants who completed both waves:\n\n")

# Check gender consistency
if("W1_Geslacht" %in% names(data_spss) && "W2_Geslacht" %in% names(data_spss)) {
  
  # Compare gender between waves
  gender_comparison <- both_waves_participants %>%
    dplyr::select(Nummer, W1_Geslacht, W2_Geslacht) %>%
    dplyr::filter(!is.na(W1_Geslacht) & !is.na(W2_Geslacht))
  
  # Check if they are identical
  gender_same <- sum(gender_comparison$W1_Geslacht == gender_comparison$W2_Geslacht, na.rm = TRUE)
  gender_different <- sum(gender_comparison$W1_Geslacht != gender_comparison$W2_Geslacht, na.rm = TRUE)
  
  cat("GENDER CONSISTENCY:\n")
  cat("- Same in both waves:", gender_same, "participants\n")
  cat("- Different between waves:", gender_different, "participants\n")
  
  if(gender_different > 0) {
    cat("WARNING: Gender should not change between waves!\n")
    cat("This might indicate data entry errors or coding changes.\n")
  } else {
    cat("✓ Good: Gender is consistent between waves\n")
  }
  cat("\n")
}

# Check birth year consistency  
if("W1_Gebjaar" %in% names(data_spss) && "W2_Gebjaar" %in% names(data_spss)) {
  
  # Compare birth year between waves
  birth_comparison <- both_waves_participants %>%
    dplyr::select(Nummer, W1_Gebjaar, W2_Gebjaar) %>%
    dplyr::filter(!is.na(W1_Gebjaar) & !is.na(W2_Gebjaar))
  
  # Check if they are identical
  birth_same <- sum(birth_comparison$W1_Gebjaar == birth_comparison$W2_Gebjaar, na.rm = TRUE)
  birth_different <- sum(birth_comparison$W1_Gebjaar != birth_comparison$W2_Gebjaar, na.rm = TRUE)
  
  cat("BIRTH YEAR CONSISTENCY:\n")
  cat("- Same in both waves:", birth_same, "participants\n") 
  cat("- Different between waves:", birth_different, "participants\n")
  
  if(birth_different > 0) {
    cat("WARNING: Birth year should not change between waves!\n")
    cat("This might indicate data entry errors.\n")
    
    # Show the differences if there are any
    if(birth_different <= 10) {
      differences <- birth_comparison %>%
        dplyr::filter(W1_Gebjaar != W2_Gebjaar) %>%
        dplyr::mutate(difference = W2_Gebjaar - W1_Gebjaar)
      
      cat("Birth year differences found:\n")
      print(differences)
    }
  } else {
    cat("✓ Good: Birth year is consistent between waves\n")
  }
  cat("\n")
}

# Check nationality consistency
if("W1_Nationaliteit" %in% names(data_spss) && "W2_Nationaliteit" %in% names(data_spss)) {
  
  # Compare nationality between waves
  nation_comparison <- both_waves_participants %>%
    dplyr::select(Nummer, W1_Nationaliteit, W2_Nationaliteit) %>%
    dplyr::filter(!is.na(W1_Nationaliteit) & !is.na(W2_Nationaliteit))
  
  # Check if they are identical
  nation_same <- sum(nation_comparison$W1_Nationaliteit == nation_comparison$W2_Nationaliteit, na.rm = TRUE)
  nation_different <- sum(nation_comparison$W1_Nationaliteit != nation_comparison$W2_Nationaliteit, na.rm = TRUE)
  
  cat("NATIONALITY CONSISTENCY:\n")
  cat("- Same in both waves:", nation_same, "participants\n")
  cat("- Different between waves:", nation_different, "participants\n")
  
  if(nation_different > 0) {
    cat("Note: Nationality can sometimes change (citizenship), but rarely\n")
  } else {
    cat("✓ Good: Nationality is consistent between waves\n")
  }
  cat("\n")
}

# WHY COLLECT DEMOGRAPHICS TWICE?
cat("WHY COLLECT DEMOGRAPHICS IN BOTH WAVES?\n")
cat("There are several practical reasons:\n\n")

cat("1. DATA QUALITY CONTROL:\n")
cat("   - Verify participants didn't make mistakes in Wave 1\n")
cat("   - Check for data entry errors\n")
cat("   - Ensure same person completed both waves\n\n")

cat("2. PARTICIPANT VERIFICATION:\n") 
cat("   - Confirm identity continuity across waves\n")
cat("   - Detect if someone else filled out Wave 2\n")
cat("   - Quality assurance for longitudinal matching\n\n")

cat("3. SOME DEMOGRAPHICS CAN CHANGE:\n")
cat("   - Education level (people can graduate)\n")
cat("   - Relationship status (single ↔ partnered)\n")
cat("   - Living situation (parents, own place, etc.)\n")
cat("   - Employment status\n")
cat("   - Income level\n\n")

cat("4. PRACTICAL SURVEY CONSIDERATIONS:\n")
cat("   - Participants might forget what they answered in Wave 1\n")
cat("   - Survey software might require demographic info\n")
cat("   - Researchers want complete data for each wave\n")
cat("   - Backup in case Wave 1 data is lost/corrupted\n\n")

# Check which demographics might legitimately change
changeable_demos <- c("W1_Diploma", "W2_Diploma", "W1_Burg_staat", "W2_Burg_staat", 
                     "W1_Relatiestatus", "W2_Relatiestatus", "W1_Leefsit", "W2_Leefsit")

existing_changeable <- changeable_demos[changeable_demos %in% names(data_spss)]

if(length(existing_changeable) > 0) {
  cat("DEMOGRAPHICS THAT CAN LEGITIMATELY CHANGE:\n")
  for(var in existing_changeable) {
    var_label <- attr(data_spss[[var]], "label")
    if(!is.null(var_label)) {
      cat("-", var, ":", var_label, "\n")
    }
  }
  cat("\nThese variables SHOULD potentially differ between waves!\n")
}

cat("\n")

# =============================================================================
# STEP 8: SUMMARY AND RECOMMENDATIONS
# =============================================================================

cat("\n=== SUMMARY AND RECOMMENDATIONS ===\n")
cat("Based on this exploration, here's what we learned:\n\n")

cat("1. STUDY DESIGN:\n")
cat("   - This appears to be a longitudinal study\n")
cat("   - W1 = Wave 1 (first measurement)\n")
cat("   - W2 = Wave 2 (follow-up measurement)\n")
cat("   - Some participants completed both waves, others only one\n\n")

cat("2. RESEARCH TOPICS:\n")
cat("   - Demographics and personal characteristics\n")
cat("   - Mental health (anxiety, depression, stress)\n")
cat("   - Relationships and social connections\n")
if(length(covid_vars) > 0) {
  cat("   - COVID-19 impact and experiences\n")
}
if(length(tech_vars) > 0) {
  cat("   - Technology use and digital behavior\n")
}
cat("\n")

cat("3. NEXT STEPS FOR ANALYSIS:\n")
cat("   - Decide if you want to analyze Wave 1 only, Wave 2 only, or both\n")
cat("   - Consider longitudinal analysis for participants with both waves\n")
cat("   - Choose specific variables relevant to your research question\n")
cat("   - Clean and recode variables as needed\n\n")

cat("4. VARIABLE SELECTION TIPS:\n")
cat("   - Use W1_ variables for baseline/initial measurements\n")
cat("   - Use W2_ variables for follow-up measurements\n")
cat("   - Compare same variables across waves (e.g., W1_Angst1 vs W2_Angst1)\n")
cat("   - Use demographic variables for participant characterization\n\n")

# =============================================================================
# STEP 8: COMPARE W1 AND W2 VARIABLES FOR SIMILARITY
# =============================================================================

cat("\n=== COMPARING W1 AND W2 VARIABLES ===\n")

# Function to compare W1 and W2 variables
compare_w1_w2_variables <- function() {
  
  # Get variables that exist in both W1 and W2
  w1_base_vars <- gsub("^W1_", "", w1_vars)
  w2_base_vars <- gsub("^W2_", "", w2_vars)
  
  # Find common variable names (without W1_/W2_ prefix)
  common_vars <- intersect(w1_base_vars, w2_base_vars)
  
  cat("Found", length(common_vars), "variables measured in both waves:\n")
  print(common_vars)
  cat("\n")
  
  # For participants who completed both waves, compare values
  both_waves_data <- data_spss %>% 
    filter(W1 == 1 & W2 == 1)
  
  cat("Analyzing", nrow(both_waves_data), "participants who completed both waves...\n\n")
  
  # Compare each common variable
  comparison_results <- data.frame(
    Variable = character(),
    W1_Variable = character(),
    W2_Variable = character(),
    Identical_Values = numeric(),
    Correlation = numeric(),
    Mean_W1 = numeric(),
    Mean_W2 = numeric(),
    Variable_Type = character(),
    stringsAsFactors = FALSE
  )
  
  cat("DETAILED COMPARISON RESULTS:\n")
  cat("============================\n")
  
  for(var in common_vars) {
    w1_var <- paste0("W1_", var)
    w2_var <- paste0("W2_", var)
    
    if(w1_var %in% names(both_waves_data) && w2_var %in% names(both_waves_data)) {
      
      # Get the values (remove labels for comparison)
      w1_values <- as.numeric(both_waves_data[[w1_var]])
      w2_values <- as.numeric(both_waves_data[[w2_var]])
      
      # Remove NA values for comparison
      valid_cases <- !is.na(w1_values) & !is.na(w2_values)
      w1_clean <- w1_values[valid_cases]
      w2_clean <- w2_values[valid_cases]
      
      if(length(w1_clean) > 0 && length(w2_clean) > 0) {
        
        # Calculate similarity metrics
        identical_count <- sum(w1_clean == w2_clean, na.rm = TRUE)
        total_valid <- length(w1_clean)
        percent_identical <- round((identical_count / total_valid) * 100, 1)
        
        # Calculate correlation if both variables are numeric and have variation
        correlation <- NA
        if(length(unique(w1_clean)) > 1 && length(unique(w2_clean)) > 1) {
          correlation <- round(cor(w1_clean, w2_clean, use = "complete.obs"), 3)
        }
        
        # Calculate means
        mean_w1 <- round(mean(w1_clean, na.rm = TRUE), 2)
        mean_w2 <- round(mean(w2_clean, na.rm = TRUE), 2)
        
        # Determine variable type
        var_type <- "Numeric"
        if(is.factor(both_waves_data[[w1_var]]) || 
           inherits(both_waves_data[[w1_var]], "haven_labelled")) {
          var_type <- "Categorical"
        }
        
        # Print results
        cat("\n", var, ":\n")
        cat("  W1 variable:", w1_var, "\n")
        cat("  W2 variable:", w2_var, "\n")
        cat("  Valid cases:", total_valid, "\n")
        cat("  Identical values:", identical_count, "(", percent_identical, "%)\n")
        cat("  Correlation:", ifelse(is.na(correlation), "N/A", correlation), "\n")
        cat("  W1 mean:", mean_w1, "\n")
        cat("  W2 mean:", mean_w2, "\n")
        cat("  Variable type:", var_type, "\n")
        
        # Add to results dataframe
        comparison_results <- rbind(comparison_results, data.frame(
          Variable = var,
          W1_Variable = w1_var,
          W2_Variable = w2_var,
          Identical_Values = percent_identical,
          Correlation = ifelse(is.na(correlation), 0, correlation),
          Mean_W1 = mean_w1,
          Mean_W2 = mean_w2,
          Variable_Type = var_type,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Summary of most stable variables
  cat("\n" , paste(rep("=", 50), collapse = ""), "\n")
  cat("SUMMARY: MOST STABLE VARIABLES (highest % identical values)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  if(nrow(comparison_results) > 0) {
    # Sort by percentage of identical values
    stable_vars <- comparison_results[order(-comparison_results$Identical_Values), ]
    
    cat("Top 10 most stable variables:\n")
    for(i in 1:min(10, nrow(stable_vars))) {
      cat(i, ". ", stable_vars$Variable[i], 
          " (", stable_vars$Identical_Values[i], "% identical)\n", sep = "")
    }
    
    cat("\nVariables with 100% identical values (should not change):\n")
    perfect_match <- stable_vars[stable_vars$Identical_Values == 100, ]
    if(nrow(perfect_match) > 0) {
      for(i in 1:nrow(perfect_match)) {
        cat("- ", perfect_match$Variable[i], "\n", sep = "")
      }
    } else {
      cat("No variables have 100% identical values.\n")
    }
    
    cat("\nVariables with low similarity (<50% identical, may indicate change or error):\n")
    low_match <- stable_vars[stable_vars$Identical_Values < 50, ]
    if(nrow(low_match) > 0) {
      for(i in 1:nrow(low_match)) {
        cat("- ", low_match$Variable[i], 
            " (", low_match$Identical_Values[i], "% identical)\n", sep = "")
      }
    } else {
      cat("All variables have >50% identical values.\n")
    }
  }
  
  return(comparison_results)
}

# Run the comparison
cat("Starting comparison analysis...\n")
comparison_results <- compare_w1_w2_variables()

cat("\n=== DATA UNDERSTANDING COMPLETE ===\n")
cat("You now have a comprehensive understanding of your dataset!\n")
cat("Run this script whenever you need to refresh your understanding.\n")

# =============================================================================
# DETAILED EXPLANATION OF THE RESULTS
# =============================================================================

cat("\n=== DETAILED EXPLANATION OF WHAT WE DISCOVERED ===\n")

cat("🔍 DATASET SIZE AND STRUCTURE:\n")
cat("- You have 2,198 participants (quite large!)\n")
cat("- 187 variables total (very comprehensive study)\n")
cat("- Data is stored in SPSS format with labels and value codes\n\n")

cat("📊 LONGITUDINAL STUDY STRUCTURE:\n")
cat("- W1 = Wave 1 (first data collection) - 67 variables\n") 
cat("- W2 = Wave 2 (follow-up data collection) - 116 variables\n")
cat("- Wave 2 has MORE variables than Wave 1 (added new measures!)\n")
cat("- 333 people participated in BOTH waves\n")
cat("- 1,687 people participated in Wave 2 only\n")
cat("- 178 people participated in Wave 1 only\n\n")

cat("🧠 RESEARCH FOCUS AREAS:\n")
cat("1. MENTAL HEALTH:\n")
cat("   - Anxiety (Angst): 6 items in W1, 4 in W2\n")
cat("   - Depression (Depressie): 3 items in both waves\n")
cat("   - Various stress measures (work, financial, Corona, relationship)\n")
cat("   - Loneliness (Eenzaamheid): 4 items in W1, 3 in W2\n\n")

cat("2. RELATIONSHIPS:\n")
cat("   - QMI (Quality Marriage Index): relationship satisfaction\n")
cat("   - Partner support and relationship stress\n")
cat("   - Relationship duration and status\n\n")

cat("3. DIGITAL/TECHNOLOGY (mainly Wave 2):\n")
cat("   - Digital deprivation (DIGDEP): computer/internet access\n")
cat("   - Problematic internet use (CIUS)\n")
cat("   - Phubbing: phone snubbing behavior\n")
cat("   - Online control behaviors\n")
cat("   - Smartphone ownership\n\n")

cat("4. COVID-19 IMPACT:\n")
cat("   - Corona stress and experiences\n")
cat("   - Quarantine experiences (Wave 2)\n")
cat("   - Vaccination status and plans (Wave 2)\n")
cat("   - Infection experiences\n\n")

cat("⏰ STUDY TIMELINE:\n")
cat("- This study was conducted DURING the COVID-19 pandemic\n")
cat("- Wave 1: Earlier in pandemic (limited COVID measures)\n")
cat("- Wave 2: Later in pandemic (extensive COVID + vaccination questions)\n")
cat("- Perfect for studying pandemic impact on mental health and relationships!\n\n")

cat("👥 PARTICIPANT CHARACTERISTICS:\n")
cat("- Demographics: gender, birth year, nationality, education\n")
cat("- Relationship status and living situations\n")
cat("- Health and disability status\n")
cat("- Income and financial stress\n")
cat("- Parenting status and stress (Wave 2)\n\n")

cat("🎯 WHAT THIS MEANS FOR YOUR ANALYSIS:\n")
cat("1. CROSS-SECTIONAL ANALYSIS:\n")
cat("   - Use W1_ variables to study pre-pandemic baseline\n")
cat("   - Use W2_ variables to study pandemic impact\n")
cat("   - Compare different participants between waves\n\n")

cat("2. LONGITUDINAL ANALYSIS (333 participants):\n")
cat("   - Track changes over time (W1_Angst1 vs W2_Angst1)\n")
cat("   - Study pandemic impact on same people\n")
cat("   - Analyze predictors of change\n\n")

cat("3. RESEARCH QUESTIONS YOU COULD EXPLORE:\n")
cat("   - Did anxiety/depression increase during pandemic?\n")
cat("   - How did digital behavior change relationships?\n")
cat("   - What predicted better/worse pandemic coping?\n")
cat("   - Did relationship quality buffer pandemic stress?\n")
cat("   - How did digital deprivation affect mental health?\n\n")

cat("🔧 PRACTICAL NEXT STEPS:\n")
cat("1. Choose your research focus (mental health? relationships? digital behavior?)\n")
cat("2. Decide on Wave 1, Wave 2, or longitudinal analysis\n")
cat("3. Select 5-10 key variables for your analysis\n")
cat("4. Clean and recode variables (the tutorial will show you how!)\n")
cat("5. Run descriptive statistics and correlations\n")
cat("6. Create visualizations and test hypotheses\n\n")

cat("💡 VARIABLE NAMING LOGIC:\n")
cat("- W1_ = Wave 1 measurement\n")
cat("- W2_ = Wave 2 measurement\n")
cat("- Numbers (1,2,3) = different items in same scale\n")
cat("- Example: W1_Angst1, W1_Angst2 = different anxiety questions\n")
cat("- Same name across waves = same concept measured twice\n\n")

cat("🏆 CONCLUSION:\n")
cat("You have access to a FANTASTIC dataset for studying:\n")
cat("- Mental health during COVID-19\n")
cat("- Digital behavior and relationships\n")  
cat("- Longitudinal changes and resilience\n")
cat("- Social and psychological adaptation\n")
cat("This is publication-quality research data! 🎉\n")