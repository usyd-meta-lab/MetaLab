pkgname <- "MetaLab"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MetaLab')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_prolific_demographics")
### * add_prolific_demographics

flush(stderr()); flush(stdout())

### Name: add_prolific_demographics
### Title: Add Prolific demographic data to a data frame
### Aliases: add_prolific_demographics

### ** Examples

## Not run: 
##D api_key <- "your_prolific_api_token"
##D 
##D # Basic usage with default column names
##D mydata <- add_prolific_demographics(mydata, api_key)
##D 
##D # Custom column names
##D mydata <- add_prolific_demographics(
##D   df      = mydata,
##D   api_key = api_key,
##D   pid_col = "PROLIFIC_PID",
##D   sid_col = "study_id"
##D )
## End(Not run)




cleanEx()
nameEx("compute_meta_measures")
### * compute_meta_measures

flush(stderr()); flush(stdout())

### Name: compute_meta_measures
### Title: Compute meta-d' and related metacognitive measures (Meta
###   Measures)
### Aliases: compute_meta_measures

### ** Examples

## Not run: 
##D res <- compute_meta_measures(
##D   data = df,
##D   id_col = "participant_id",
##D   stim_col = "stimulus",
##D   resp_col = "response",
##D   conf_col = "confidence",
##D   s1_stim = "S1",
##D   s2_stim = "S2",
##D   s1_resp = "S1",
##D   s2_resp = "S2"
##D )
## End(Not run)



cleanEx()
nameEx("create_MetaLab_project")
### * create_MetaLab_project

flush(stderr()); flush(stdout())

### Name: create_MetaLab_project
### Title: Create a standardized MetaLab project directory structure
### Aliases: create_MetaLab_project

### ** Examples

## Not run: 
##D create_MetaLab_project("Metacognition_Study")
## End(Not run)




cleanEx()
nameEx("find_duplicate_attempts")
### * find_duplicate_attempts

flush(stderr()); flush(stdout())

### Name: find_duplicate_attempts
### Title: Identify participants who uploaded data in more than one source
###   file
### Aliases: find_duplicate_attempts

### ** Examples

## Not run: 
##D library(dplyr)
##D 
##D # toy data --------------------------------------------------------
##D trial_data <- tibble(
##D   participant_id = c("P1", "P1", "P2", "P3", "P3", "P3"),
##D   source_file    = c("fileA.csv", "fileB.csv", "fileA.csv",
##D                      "fileC.csv", "fileC.csv", "fileD.csv"),
##D   rt             = runif(6)  # other columns irrelevant
##D )
##D 
##D # find repeat submissions ----------------------------------------
##D find_duplicate_attempts(trial_data)
##D 
##D # capture silently and filter the main dataset -------------------
##D dups <- find_duplicate_attempts(trial_data, quiet = TRUE)
##D clean_data <- anti_join(trial_data, dups, by = "participant_id")
## End(Not run)




cleanEx()
nameEx("independent_t_test_apa")
### * independent_t_test_apa

flush(stderr()); flush(stdout())

### Name: independent_t_test_apa
### Title: Independent Samples t-test with APA Reporting
### Aliases: independent_t_test_apa

### ** Examples

## Not run: 
##D # Example dataset
##D mydata <- data.frame(
##D   score = c(85, 88, 90, 78, 82, 91, 84, 77, 86, 80),
##D   group = rep(c("Control", "Treatment"), each = 5)
##D )
##D 
##D # Run the function
##D result <- independent_t_test_apa(score ~ group, data = mydata, var.equal = TRUE)
##D cat(result)
## End(Not run)




cleanEx()
nameEx("json_to_long")
### * json_to_long

flush(stderr()); flush(stdout())

### Name: json_to_long
### Title: Convert JSON responses to long format
### Aliases: json_to_long

### ** Examples

## Not run: 
##D df_long <- json_to_long(data, participant_id = "id", response = "response_json")
## End(Not run)




cleanEx()
nameEx("make_dyads_long")
### * make_dyads_long

flush(stderr()); flush(stdout())

### Name: make_dyads_long
### Title: Build dyadic data (male–female) from long records, with optional
###   APIM format
### Aliases: make_dyads_long

### ** Examples

## Not run: 
##D library(dplyr)
##D 
##D df <- tibble::tibble(
##D   Couple_ID = c(1,1,1,1,2,2),
##D   Prolific_ID = c("a","b","a","b","c","d"),
##D   Time = c(1,1,2,2,1,1),
##D   Strategy = c("DA","DA","DA","DA","DB","DB"),
##D   T1A_Demo_Gender = c(1,2,1,2,1,2),  # 1=male, 2=female
##D   intrinsic = c(4.7, 4.9, 4.2, 4.8, 3.5, 3.7),
##D   extrinsic = c(4.5, 4.6, 4.1, 4.4, 3.2, 3.4)
##D )
##D 
##D # Dyad-wide
##D dw <- make_dyads_long(
##D   df,
##D   couple_id    = "Couple_ID",
##D   person_id    = "Prolific_ID",
##D   time_var     = "Time",
##D   within_vars  = "Strategy",
##D   gender_var   = "T1A_Demo_Gender",
##D   male_code    = 1,
##D   female_code  = 2,
##D   measure_vars = c("intrinsic", "extrinsic"),
##D   output       = "dyad_wide"
##D )
##D 
##D # APIM double-entry
##D ap <- make_dyads_long(
##D   df,
##D   couple_id    = "Couple_ID",
##D   person_id    = "Prolific_ID",
##D   time_var     = "Time",
##D   within_vars  = "Strategy",
##D   gender_var   = "T1A_Demo_Gender",
##D   male_code    = 1,
##D   female_code  = 2,
##D   measure_vars = c("intrinsic", "extrinsic"),
##D   output       = "apim_long"
##D )
## End(Not run)




cleanEx()
nameEx("plot_numeric_histograms")
### * plot_numeric_histograms

flush(stderr()); flush(stdout())

### Name: plot_numeric_histograms
### Title: Plot Histograms for All Numeric Variables
### Aliases: plot_numeric_histograms

### ** Examples

df <- data.frame(
  age = c(23, 35, 45, 29, 60),
  score = c(88.5, 92.3, 79.4, 85.0, 90.1),
  income = c(40000, 52000, 61000, 48000, 57000)
)
plot_numeric_histograms(df)




cleanEx()
nameEx("rain_plot")
### * rain_plot

flush(stderr()); flush(stdout())

### Name: rain_plot
### Title: Raincloud-style plots for between-, within-, and mixed-design
###   data
### Aliases: rain_plot

### ** Examples

## Not run: 
##D # Simple between-subjects design
##D rain_plot(
##D   data    = mydata,
##D   dv      = "accuracy",
##D   between = "group"
##D )
##D 
##D # Within-subjects design (e.g., task is repeated within participants)
##D rain_plot(
##D   data   = mydata,
##D   dv     = "confidence",
##D   id     = "participant",
##D   within = "task"
##D )
##D 
##D # Mixed design: within (condition) × between (group)
##D rain_plot(
##D   data    = mydata,
##D   dv      = "rt",
##D   id      = "participant",
##D   within  = "condition",
##D   between = "group"
##D )
##D 
##D # Multiple between factors: facet by rating_condition
##D rain_plot(
##D   data    = mydata,
##D   dv      = "efficacy_rating",
##D   id      = "participant",
##D   between = c("outcome_condition", "rating_condition")
##D )
##D 
##D # Multiple within factors: task × time collapsed onto the x-axis
##D rain_plot(
##D   data   = mydata,
##D   dv     = "confidence",
##D   id     = "participant",
##D   within = c("task", "time")
##D )
## End(Not run)




cleanEx()
nameEx("select_cue_target_sample")
### * select_cue_target_sample

flush(stderr()); flush(stdout())

### Name: select_cue_target_sample
### Title: Select a Random Sample of Cue–Target Pairs by Approximate Mean
###   FSG
### Aliases: select_cue_target_sample

### ** Examples

## Not run: 
##D library(readxl)
##D florida <- read_xlsx("Florida Norms.xlsx")
##D 
##D sample40 <- select_cue_target_sample(
##D   df = florida,
##D   n = 40,
##D   target_mean_fsg = 0.25,
##D   cue_min_chars = 3, cue_max_chars = 10,
##D   target_min_chars = 3, target_max_chars = 10,
##D   attempts = 8000,
##D   seed = 42
##D )
##D 
##D attr(sample40, "achieved_mean_fsg")
##D head(sample40)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
