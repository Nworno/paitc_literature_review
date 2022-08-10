## ---- results='hide'-----------------------------------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(purrr)

source("R_snippets.R")

## ---- echo=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE, echo = FALSE, message = FALSE, warning = FALSE)


## ----message=FALSE, include=FALSE----------------------------------------------------
# responses_old_form <- read_csv("data/pubv1/extraction_articles/test_comparison_responses.csv")
dir_data <- "data/literature_search_pubv1"

extraction_df <- read_csv(
  file.path(dir_data, "/extraction_articles/current_extraction_responses.csv"),
  name_repair = "minimal"
)

list_articles_to_do <- read_csv(file.path(dir_data, "/inclusion_articles/included_articles_final.csv")) %>%
  mutate(PMID = as.character(PMID),
         row_names = 1:n())

## ----Renaming sections---------------------------------------------------------------
source("questions_sections.R")
extraction_df$`Original Timestamp` <- NULL
stopifnot(length(list_questions) == ncol(extraction_df))

# Testing that names from the extraction csv results and names from the renaming list are identical
all(str_trim(names(list_questions)) == str_trim(names(extraction_df)))

# Renaming data.frame questions, numbered questions are only useful to avoid duplicates
names(extraction_df) <- list_questions

# Make list_questions as a data frame
df_list_questions <- tibble(question_names = names(list_questions), question_names_numbered = list_questions)

# questions respective to there sections
df_questions_sections <- questions_sections %>%
  tibble::enframe() %>%
  tidyr::unnest_longer(col = value) %>%
  rename(question_names = value,
         section = name) %>%
  select(-value_id) %>%
  left_join(df_list_questions, by = c("question_names"))


## ----Format DOI----------------------------------------------------------------------
extraction_df <- extraction_df %>%
  mutate(doi = sub("^.*doi\\.org/", replacement = "", x = doi, perl = TRUE)) %>%
  mutate(doi = sub("^.+doi=", replacement = "", x = doi, fixed = FALSE)) %>%
  mutate(doi = sub("%2F", replacement = "/", x = doi, fixed = TRUE))

# Drop rows without n_itc, or with "x"
extraction_df <- extraction_df[!is.na(extraction_df$n_itc) & extraction_df$n_itc != "x", ]
extraction_df$Notes <- NULL

# Create identifier for every row
extraction_df[["row_id"]] <- with(extraction_df, paste(doi, n_itc, reviewer, sep = "_"))

# Manually remove these rows, to take into account the edge case when a reviewer does the same article twice (reviewer is important because second timestamp is actually not unique!!)
extraction_df <- extraction_df %>% filter(
  !(timestamp %in% c("09/08/2022 11:37:14" , "09/08/2022 11:47:21") & reviewer == "BZ")
)


# articles done -----------------------------------------------------------

followup <- extraction_df %>%
  select(doi, reviewer, n_itc) %>%
  filter(!is.na(n_itc)) %>%
  select(!n_itc) %>%
  distinct(doi, reviewer) %>%
  mutate(counter = TRUE) %>%
  pivot_wider(names_from = reviewer, values_from = counter, values_fill = FALSE) %>%
  mutate(both = BZ & ASL)

done <- list_articles_to_do[, c("DOI", "PMID", "row_names")] %>%
  mutate(reviewer = rep_len(c("DH", "JL"), length.out = n())) %>%
  full_join(followup, by = c("DOI" = "doi")) %>%
  mutate(both = ifelse(is.na(both), FALSE, both))


## ----pivot_longer doi reviewer n_itc-------------------------------------------------

id_names <- c("n_itc", "doi", "reviewer", "row_id")
long_extraction_df <- extraction_df %>%
  select(-timestamp, -url) %>%
  mutate(across(everything() & !all_of(id_names),
                .fns = as.character)
  ) %>%
  pivot_longer(
    cols = everything() & !all_of(id_names),
    names_to = "questions",
    values_to = "answers"
  ) %>%
  left_join(df_questions_sections[, c("section", "question_names_numbered")],
            by = c("questions" = "question_names_numbered")) %>%
  filter(!is.na(answers))

# Format checks
stopifnot(nrow(filter(long_extraction_df, is.na(section))) == 0)


## ----Splitting data.frames------------------------------------------------------
general_information <- long_extraction_df %>% filter(section == "general_information")
study_information <- long_extraction_df %>% filter(section == "study_information")
methodology <- long_extraction_df %>% filter(section == "methodology")
results <- long_extraction_df %>% filter(section == "results")


# General Information DM --------------------------------------------------
## Removing eventual duplicated: 1. removing missing answers, 2. keeping first line in case of duplication

# Counting number of missing answers
print(general_information$answers %>% is.na() %>% sum())

# Manually selecting the duplicates to filter out
rows_id_to_discard_general_info <- c(
  "10.2217/cer-2021-0216_3_BZ",
  "10.1016/j.ejcsup.2021.06.002_3_BZ",
  "10.1016/j.ejcsup.2021.06.002_4_BZ",
  "10.1016/j.ejcsup.2021.06.002_5_BZ",
  "10.1016/j.ejcsup.2021.06.002_6_BZ",
  "10.2147/CMAR.S325043_2_BZ",
  "10.1210/clinem/dgab905_2_BZ",
  "10.2217/cer-2021-0216_3_BZ"
)

# Looking at remaining duplicated answers
## CHECK 1
general_information %>%
  filter(! row_id %in% rows_id_to_discard_general_info) %>%
  get_all_duplicated_pipe(all_of(c("doi", "reviewer", "questions", "answers"))) %>%
  pivot_wider(id_cols = id_names, names_from = questions, values_from = answers) %>%
  View()


general_information_dm <- general_information %>%
  filter(!row_id %in% rows_id_to_discard_general_info) %>%
  select(!n_itc)

# Data management responses general_information

# study_information -------------------------------------------------------

## Assigning study number: relies on the fact that the order of the rows (questions respective to a given form) hasn't change!!
study_info_with_num <- study_information %>%
  select(all_of(c(id_names, "section", "answers", "questions"))) %>%
  separate(questions,
           into = c("questions", "number"),
           sep = "_(?=[0-9]$)",
           fill = "right") %>%
  mutate(study_number_raw = ifelse(questions == "study_number", answers, NA),
         # to visualy check, because `fill` cannot be used within a mutate statement
         study_number = study_number_raw) %>%
  group_by(row_id) %>%
  fill(study_number, .direction = "down") %>%
  filter(study_number != "x") %>%
  ungroup()

## Identifying duplicates ==> manually discard duplicates by marking them with x
### CHECK 2
get_all_duplicated_pipe(data = study_info_with_num, doi, reviewer, study_number, questions) %>%
  View()

# Once visual checking is done, remove study number tagged as 'x' and the questions "study_number", "n_itc", "number"
study_info_with_num <- study_info_with_num %>%
  filter(study_number != "x",
         questions != "study_number") %>%
  select(!all_of(c("n_itc", "row_id", "study_number_raw", "number")))

## Identifying misaligned study information sections
study_info_aligned <- study_info_with_num %>%
  pivot_wider(names_from = reviewer,
              values_from = answers) %>%
  inner_join(done[done$both, "DOI"], by = c("doi" = "DOI"))

# IDENTIFY MISALIGNED INDIVIDUAL STUDIES
## Based on NCT that is supposed to be the same, then on all other questions. Cannot really check automatically for studies without NCT
### CHECK 3
View(study_info_aligned %>%
       filter(questions == "NCT (only for clinical trial registered on clinicaltrials.gov)") %>%
       mutate(identical = BZ == ASL) %>%
       filter(identical != TRUE | is.na(identical)) %>%
       arrange(doi, study_number))

# methodology -------------------------------------------------------------

## CHECK 4
methodology %>%
  get_all_duplicated_pipe(id_names, questions) %>%
  View()




# results -----------------------------------------------------------------
## CHECK 5
results %>%
  get_all_duplicated_pipe(id_names, questions) %>%
  View()


# combining sections ------------------------------------------------------

order_sections <- data.frame(
  order_sections = c(1, 2, 3, 4),
  section = c("general_information", "study_information", "methodology", "results")
)

data_manage_answers <- function(long_data) {
  question_names <- unique(long_data$questions)
  long_data <- mutate(long_data, rows_order = 1:nrow(long_data))

  free_text_questions <- c(
    "Medical Condition of Interest Name",
    "Countries of first author affiliations",
    "Countries of last author affiliations",
    "NCT (only for clinical trial registered on clinicaltrials.gov)",
    "Country where the clinical trial/observational study was conducted (international if more than one)",
    "EudraCT (only for clinical trials registered on clinicaltrialsregister.eu",
    "Data source name (only if observational study or clinical trial without NCT)",
    "Treatment name 1",
    "Study 'number(s)' for treatment 1",
    "Treatment name 2",
    "Study 'number(s)' for treatment 2",
    "Type of population-adjusted indirect comparisons performed",
    "Primary Outcome Name (first one mentioned in the text of the results part if no primary outcome defined)",
    "Sample size of the population of interest in the non IPD treatment arm",
    "Initial sample size of the population of interest in the IPD treatment arm",
    "Sample size in the IPD treatment arm used in the indirect comparison, ie effective sample size after reweighting for MAIC; or sample size used in the regression model for STC",
    "Number of covariates adjusted for/matched on",
    "Primary outcome: treatment effect contrast",
    "Direction of the treatment effect contrast: IPD treatment is:",
    "Primary outcome: unadjusted treatment effect",
    "p-value for the unadjusted treatment effect (or 95 CI if pvalue is not provided, written as [X.XX-Y.YY])",
    "Primary outcome: adjusted treatment effect",
    "p-value for the adjusted treatment effect (or 95 CI if pvalue is not provided, written as [X.XX;Y.YY])",
    "If anchored comparison, sample size of the population of interest in the non IPD treatment anchor arm",
    "If anchored comparison, initial sample size of the population of interest in the IPD anchor arm",
    "If anchored comparison, effective sample size after reweighting for MAIC; or sample size used in the regression model for STC in the IPD anchor arm"
  )
  apply_specific_answers_dm <- function(wide_data) {
    wide_data %>%
      mutate(across(.cols = all_of(free_text_questions),
                    .fns = function(x) str_squish(str_to_lower(x)))) %>%
    mutate(`Positions of study investigators (for any authors of the article, any that applies)` = sub(
      "Pharmaceutical Industry / Medical device company",
      "Pharmaceutical Industry",
      x = `Positions of study investigators (for any authors of the article, any that applies)`,
      fixed = TRUE),
      `Study 'number(s)' for treatment 1` = sub(
        "[\\s,;]",
        ";",
        x = `Study 'number(s)' for treatment 1`
      ),
      `Study 'number(s)' for treatment 2` = sub(
        "[\\s,;]",
        ";",
        x = `Study 'number(s)' for treatment 2`
      ),
      `Justification for selecting variables to be included in the adjustment model (in the main text)` = sub(
        "(ie pvalue",
        "(eg pvalue",
        x = `Justification for selecting variables to be included in the adjustment model (in the main text)`,
        fixed = TRUE
      ),
      `Covariates adjusted for/matched on in the indirect comparison` = sub(
        "Performance score (ECOG PS, Karnofsky, Lansky, OMS, Ranson, ...)",
        "Performance score",
        `Covariates adjusted for/matched on in the indirect comparison`,
        fixed = TRUE
      )
    )
  }
  # Pivot, apply answer transformation, and unpivot, and check for strict equalities of the input and output (beside the answer column)
  long_data_dm <- long_data %>%
    select(!rows_order) %>%
    pivot_wider(names_from = questions, values_from = answers) %>%
    apply_specific_answers_dm() %>%
    pivot_longer(cols = all_of(question_names),
                 names_to = "questions",
                 values_to = "answers") %>%
    drop_na(answers) %>%
    full_join(long_data[, c("doi", "n_itc", "study_number", "questions", "reviewer", "rows_order")]) %>%
    select(all_of(names(long_data))) %>%
    arrange(rows_order)
  stopifnot(identical(long_data %>% select(!answers),
                      long_data_dm %>% select(!answers))
  )
  return(long_data_dm %>% select(!rows_order))
}

long_results_dm <- bind_rows(general_information_dm, study_info_with_num, methodology, results) %>%
  select(!row_id) %>%
  select(doi, n_itc, study_number, section, questions, reviewer, answers) %>%
  data_manage_answers() %>%
  pivot_wider(names_from = reviewer, values_from = answers) %>%
  mutate(identical = ASL == BZ,
         decision = if_else(identical, BZ, NA_character_)) %>%
  replace_na(replace = list(n_itc = "", study_number = "", BZ = "", ASL = "", identical = FALSE, decision =  "")) %>%
  rename(`Individual Studies Number` = study_number,
         `ITC Number` = n_itc
  ) %>%
  left_join(done[, c("DOI", "PMID", "both", "reviewer", "row_names")],
            by = c("doi" = "DOI")) %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, `ITC Number`) %>%
  select(!order_sections, !row_names) %>%
  select(doi, PMID, section, `ITC Number`, `Individual Studies Number`, everything(), -order_sections)


# Export results ----------------------------------------------------------

long_results_dm %>% write_delim(paste0("data/literature_search_pubv1/extraction_articles/comparison_answers",
                                       as.Date(lubridate::now(), format = "yyMMDD"),
                                       ".tsv"),
                                delim = "\t")

for (initials in c("DH", "JL")) {
  dir_results <- file.path(dir_data, "extraction_articles", initials)
  if (!dir.exists(dir_results)) dir.create(dir_results)
  result_reviewer <- long_results_dm %>%
    filter(reviewer == initials & both)
  for (pmid in unique(result_reviewer$PMID)) {
    data <- result_reviewer %>%
      filter(PMID == pmid)
    row_name <- as.character(unique(data$row_names))
    file_name <- file.path(dir_results, paste0(str_pad(row_name, width = 3, pad = "0"), "_", pmid, ".tsv"))
    if (!file.exists(file_name))
      data %>%
      select(-both, -reviewer, -row_names) %>%
      write_tsv(file = file_name)
  }
  ## writing batch zip file every 10 new articles
  file_infos <- file.info(list.files(dir_results, full.names = TRUE))
  file_infos$path <- row.names(file_infos)
  file_infos %>%
    mutate(day_creation = lubridate::date(ctime)) %>%
    arrange(day_creation, path) %>%
    mutate(n_row = 1:nrow(.),
           batch = (n_row - 1) %/% 10 + 1) %>%
    group_by(batch) %>%
    group_walk(.f = function(df, group_name_df) {
      paths <- pull(df, path)
      group_name <- pull(group_name_df, batch)
      if (length(paths) < 10) {
        cat("returning", as.character(group_name), "\n")
        return()
      }
      zip_name <- file.path(dir_results,
                            paste0(group_name, "_", initials, ".zip")
      )
      # j option flag to avoid nested directory, other flags are just default values
      if (! file.exists(zip_name)) zip(zip_name, paths, flags = '-r9Xj')
    })
}


## Question/réponse à harmoniser encore
# Question  'Justification for selecting variables to be included in the adjustment model (in the main text) '
    ## Réponse "statistical based" séparé en 2 mi-juillet --> à reprendre à la main
# Effectifs initiaux : changement de définition au cours de l'inclusion
