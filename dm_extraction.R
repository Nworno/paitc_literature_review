library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)

source("R_snippets.R")

dir_data <- "data"

extraction_df <- read_csv(
  file.path(dir_data, "/extraction/current_extraction.csv"),
  name_repair = "minimal"
)

full_list_articles_to_do <- read_csv(file.path(dir_data, "/articles_selection/included_articles.csv")) %>%
  mutate(PMID = as.character(PMID),
         `Create Date` = as_date(`Create Date`, format = "%d/%m/%Y")) %>%
  arrange(`Create Date`)

## ----Renaming sections---------------------------------------------------------------
source("questions_sections.R")

extraction_df$`Original Timestamp` <- NULL
extraction_df$url <- NULL
stopifnot(length(list_questions) == ncol(extraction_df))

# Testing that names from the extraction csv results and names from the renaming list are identical
all(str_trim(names(list_questions)) == str_trim(names(extraction_df)))

# Renaming data.frame questions, numbered questions are only useful to avoid duplicates
names(extraction_df) <- list_questions

# Make list_questions as a data frame
df_list_questions <- tibble(question_names = names(list_questions), question_names_numbered = list_questions)

# questions respective to their sections
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
  mutate(doi = sub("%2F", replacement = "/", x = doi, fixed = TRUE)) %>%
  mutate(doi = tolower(doi)) %>%
  # Manually correct DOI
  mutate(doi = ifelse(doi == "03007995.2016.1248380",
                      "10.1080/03007995.2016.1248380",
                      doi)
  )

list_articles_to_do <- full_list_articles_to_do %>%
  rename(doi = DOI) %>%
  mutate(doi = tolower(doi)) %>%
  filter(!included %in% c("XX"))

# manual corrections ------------------------------------------------------

# Manually remove these rows, to take into account the edge case when a reviewer does the same article twice (reviewer is important because second timestamp is actually not unique!!)
extraction_df <- extraction_df %>% filter(
  !(timestamp %in% c("01/09/2022 14:47:10", "01/09/2022 14:47:55") & reviewer == "BZ")
)

## checking completeness of the extraction ---------------------------------
extraction_df <- extraction_df %>%
  # excel suppresses the seconds if saving to csv, so may need to adapt format "%d/%m/%Y %H:%M")
  mutate(timestamp = as_datetime(timestamp, format = "%d/%m/%Y %H:%M:%S")) %>%
  filter(timestamp >= as_date("2022-07-01"))

to_do <- list_articles_to_do %>%
  filter(!included %in% c("XX")) %>%
  distinct(Title, doi, date, PMID)

done_ASL <- extraction_df %>%
  filter(reviewer == "ASL") %>%
  distinct(doi)

done_BZ <- extraction_df %>%
  filter(reviewer == "ASL") %>%
  distinct(doi)

stopifnot("some included articles are missing from the review" = nrow(anti_join(to_do, done_ASL, by = "doi")) == 0)
stopifnot("some included articles are missing from the review" = nrow(anti_join(to_do, done_BZ, by = "doi")) == 0)

reviewed_excluded_ASL <- anti_join(done_ASL, to_do, by = "doi")
reviewed_excluded_BZ <- anti_join(done_BZ, to_do, by = "doi")

# filter on the included articles -----------------------------------------

included_df <- extraction_df %>%
  inner_join(to_do[, "doi"], by = "doi")

# check that the n_itc column is filled for every articles
stopifnot(included_df %>% filter(is.na(n_itc)) %>% nrow() == 0)

# Drop rows without n_itc, or with "x"
subset_included_df <- included_df %>%
  filter(!n_itc %in% c("x", "xx"))

# Create identifier for every row
subset_included_df[["row_id"]] <- with(subset_included_df, paste(doi, n_itc, reviewer, sep = "_"))

# articles done -----------------------------------------------------------

followup <- subset_included_df %>%
  select(doi, reviewer, n_itc) %>%
  filter(!is.na(n_itc)) %>%
  select(!n_itc) %>%
  distinct(doi, reviewer) %>%
  mutate(counter = TRUE) %>%
  pivot_wider(names_from = reviewer, values_from = counter, values_fill = FALSE) %>%
  mutate(both = BZ & ASL)

done <- list_articles_to_do[, c("doi", "PMID")] %>%
  mutate(row_names = 1:n(),
         reviewer = rep_len(c("JL", "DH"), length.out = n())) %>%
  full_join(followup, by = c("doi")) %>%
  mutate(both = ifelse(is.na(both), FALSE, both))

stopifnot(all(followup$both))

## ---- pivot_longer doi reviewer n_itc-------------------------------------------------

id_names <- c("n_itc", "doi", "reviewer", "row_id")
long_extraction_df <- subset_included_df %>%
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
notes <- long_extraction_df %>% filter(section == "Notes")

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
  # filter(!timestamp != as_datetime("09/08/2022 11:37:14"))
  get_all_duplicated_pipe(all_of(c("doi", "reviewer", "questions", "answers"))) %>%
  pivot_wider(id_cols = all_of(id_names), names_from = questions, values_from = answers) %>%
  View()


general_information_dm <- general_information %>%
  filter(!row_id %in% rows_id_to_discard_general_info) %>%
  select(!n_itc)

# Data management responses general_information

# study_information -------------------------------------------------------

## Assigning study number: relies on the fact that the order of the rows (questions respective to a given form) hasn't change!!
row_id_to_discard_study_info <- c("10.1210/clinem/dgab905_2_BZ")

study_info_with_num <- study_information %>%
  select(all_of(c(id_names, "section", "answers", "questions"))) %>%
  separate(questions,
           into = c("questions", "number"),
           sep = "_(?=[0-9]$)",
           fill = "right") %>%
  mutate(study_number_raw = ifelse(questions == "study_number", answers, NA),
         # to visually check, because `fill` cannot be used within a mutate statement
         study_number = study_number_raw) %>%
  group_by(row_id) %>%
  fill(study_number, .direction = "down") %>%
  filter(!study_number %in% c("x", "xx")) %>%
  ungroup() %>%
  filter(!row_id %in% row_id_to_discard_study_info)

## Identifying duplicates ==> manually discard duplicates by marking them with x
### CHECK 2
get_all_duplicated_pipe(data = study_info_with_num, doi, reviewer, study_number, questions) %>%
  View()

# Once visual checking is done, remove the questions "study_number", "n_itc", "number"
study_info_with_num <- study_info_with_num %>%
  filter(questions != "study_number") %>%
  select(all_of(c("doi", "reviewer", "section", "study_number", "questions", "answers")))

## Identifying misaligned study information sections
study_info_aligned <- study_info_with_num %>%
  pivot_wider(names_from = reviewer,
              values_from = answers) %>%
  inner_join(done[done$both, "doi"], by = "doi" )

# IDENTIFY MISALIGNED INDIVIDUAL STUDIES
## Based on NCT that is supposed to be the same, then on all other questions.
### CHECK 3
View(study_info_aligned %>%
       filter(questions == "NCT (only for clinical trial registered on clinicaltrials.gov)") %>%
       mutate(identical = BZ == ASL) %>%
       filter(identical != TRUE | is.na(identical)) %>%
       arrange(doi, study_number))

### CHECK 4  EUDRA-CT
View(study_info_aligned %>%
       filter(questions == "EudraCT (only for clinical trials registered on clinicaltrialsregister.eu") %>%
       mutate(identical = BZ == ASL) %>%
       filter(identical != TRUE | is.na(identical)) %>%
       arrange(doi, study_number))


### CHECK 5 on data source name
View(study_info_aligned %>%
       filter(questions == "Data source name (only if observational study or clinical trial without NCT)") %>%
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


# Parsing questions -----------------------------------------------

## Free text questions
free_text_questions <- c(
  "Medical Condition of Interest Name",
  "Countries of first author affiliations",
  "Countries of last author affiliations",
  "Data source name (only if observational study or clinical trial without NCT)",
  "Country where the clinical trial/observational study was conducted (international if more than one)",
  "Treatment name 1",
  "Study 'number(s)' for treatment 1",
  "Treatment name 2",
  "Study 'number(s)' for treatment 2",
  "Primary Outcome Name (first one mentioned in the text of the results part if no primary outcome defined)",
  "unadjusted_ttt_effect",
  "unadjusted_pvalue_CI",
  "adjusted_ttt_effect",
  "adjusted_pvalue_CI",
  "If anchored comparison, sample size of the population of interest in the non IPD treatment anchor arm",
  "If anchored comparison, initial sample size of the population of interest in the IPD anchor arm",
  "If anchored comparison, effective sample size after reweighting for MAIC; or sample size used in the regression model for STC in the IPD anchor arm"
)

# Multiple choices questions
qcm <- c(
  "Positions of study investigators (for any authors of the article, any that applies)",
  "Mentioned sources of funding",
  "Phase of the clinical trial (clinical trial only)",
  "Form of the indirect comparison",
  "Justification for selecting variables to be included in the adjustment model (in the main text)",
  "Covariates adjusted for/matched on in the indirect comparison"
)

others <- c(
  "Timestamp",
  "DOI of the article"
)

## Unique choice questions
qcu <- c(
  "Initials of the reviewer filling the form",
  "itc_number",
  "At least one author affiliated with a department of Biostatistics, Epidemiology, Public Health, Pharmacoepidemiology, or a Clinical Research unit, or private data analysis company",
  "Ties with pharmaceutical industry mentionned in competing interest/conflict of interest/disclaimer, or any equivalent section in the article",
  "Mention of a systematic review to find the studies to compare treatments of interest",
  "study_number",
  "NCT (only for clinical trial registered on clinicaltrials.gov)",
  "EudraCT (only for clinical trials registered on clinicaltrialsregister.eu",
  "Patient-level data used",
  "Clinical Trial",
  "Number of treatment arms (clinical trial only)",
  "Type of population-adjusted indirect comparisons performed",
  "Anchored comparison?",
  "Form of the indirect comparison",
  "Definition of a single primary outcome for the indirect comparison",
  "Primary outcome: variable type",
  "Inclusion of prognostic factors in the adjustment/matching model",
  "Inclusion of treatment-effect modifiers in the adjustment/matching model",
  "Mention of the MAIC weights estimation model / STC adjustment model details in the main text **or supplemental materials** (ie matching on first moment, second moment, including interaction term, etc)",
  "Discussion of the choice of the scale for the outcome in the main text (ie natural outcome scale vs transformed outcome scale)",
  "Reporting of a weights' distribution evaluation (MAIC)",
  "Reporting of the list of the covariates adjusted for/matched on",
  "Number of covariates adjusted for/matched on",
  "Primary outcome: treatment effect contrast",
  "Direction of the treatment effect contrast: IPD treatment is:",
  "Sample size of the population of interest in the non IPD treatment arm",
  "Initial sample size of the population of interest in the IPD treatment arm",
  "Sample size in the IPD treatment arm used in the indirect comparison, ie effective sample size after reweighting for MAIC; or sample size used in the regression model for STC"
)


# combining sections ------------------------------------------------------

order_sections <- data.frame(
  order_sections = c(1, 2, 3, 4),
  section = c("general_information", "study_information", "methodology", "results")
)


data_manage_answers <- function(long_data) {
  question_names <- unique(long_data$questions)
  long_data <- mutate(long_data, rows_order = 1:nrow(long_data))

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
        `Covariates adjusted for/matched on in the indirect comparison` = str_replace(
          `Covariates adjusted for/matched on in the indirect comparison`,
          c(fixed("Performance score (ECOG PS, Karnofsky, Lansky, OMS, Ranson, ...)"),
            fixed("Performance score (ECOG PS, Karnofsky, Lansky)")
          ),
          "Performance score"
        )
      ) %>%
      mutate(across(starts_with("Treatment name"),
                    ~ str_replace(.x, "(\\s?\\+\\s?)|(\\splus\\s)|(and)", " + ")
      ))

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


# DM pvalues --------------------------------------------------------------

# test <- data_manage_answers(long_data)
#
# test <- bind_rows(general_information_dm, study_info_with_num, methodology, results) %>%
#   select(!row_id) %>%
#   select(doi, n_itc, study_number, section, questions, reviewer, answers) %>%
#   data_manage_answers()
#
# test %>%
#   filter(questions == "unadjusted_pvalue_CI") %>%
#   select(answers) %>%
#   mutate(pvalue = ifelse(str_detect(answers, "^\\["), NA, answers),
#          ci = ifelse(str_detect(answers, "^\\["), answers, NA)) %>%
#   View()
#
#   unique()
#
# test %>%
#   filter(questions == "unadjusted_pvalue_CI") %>%
#   pull(answers) %>%
#   unique()
# -----------

assert_names <- function(df, columns_to_review) {
  # Check that the questions to review names are indeed present in the question column
  # To avoid discarding eventually important question to review because of some name/rename mismatch
  stopifnot(
    "some question_to_review names are not present in the 'questions' column" =
      all(columns_to_review %in% unique(df$questions))
  )
  df
}

long_results_dm <- bind_rows(general_information_dm, study_info_with_num, methodology, results) %>%
  select(!row_id) %>%
  select(doi, n_itc, study_number, section, questions, reviewer, answers) %>%
  data_manage_answers() %>%
  pivot_wider(names_from = reviewer, values_from = answers) %>%
  assert_names(columns_to_review) %>%
  replace_na(replace = list(n_itc = "", study_number = "", BZ = "", ASL = "")) %>%
  mutate(identical = ASL == BZ,
         decision = ifelse(identical, BZ,
                           ifelse(questions %in% columns_to_review, "", "XXXX")
         )
  ) %>%
  mutate(across(c(ASL, BZ),
                .fns = ~ if_else(ASL == BZ, "", .x))) %>%
  rename(`Ind studies num` = study_number,
         `ITC num` = n_itc,
         "reviewer 1" = "ASL",
         "reviewer 2" = "BZ"
  ) %>%
  mutate(questions = replace_in_vec(questions, swap_names_values(list_questions))) %>%
  left_join(done[, c("doi", "PMID", "reviewer", "row_names")],
            by = "doi") %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, `ITC num`, `Ind studies num`) %>%
  select(-order_sections, -row_names) %>%
  select(doi, PMID, section, `ITC num`, `Ind studies num`, questions, `reviewer 1`, `reviewer 2`, decision, identical, reviewer)

# adding notes ------------------------------------------------------------
notes_dm <- notes %>%
  select(n_itc, doi, reviewer, answers) %>%
  pivot_wider(names_from = reviewer, values_from = answers,
              values_fn = function(...) paste(..., collapse = "   ")) %>%
  rename("Notes reviewer 1" = "ASL", "Notes reviewer 2" = "BZ")

long_results_w_notes <- long_results_dm %>%
  left_join(notes_dm, by = c("doi", "ITC num" = "n_itc")) %>%
  group_by(doi, `ITC num`) %>%
  mutate(across(all_of(c("Notes reviewer 1", "Notes reviewer 2")),
         .fns = ~ replace(.x, duplicated(.x), "")
  ))


# count identical ---------------------------------------------------------

counts <- long_results_w_notes %>%
  group_by(doi) %>%
  summarise(n_questions = n(),
            same = sum(identical),
            differences = sum(!identical),
            to_review = sum(decision == "")
  )

summarise(counts, across(-doi, .fns = sum))

done_info <- left_join(done, counts, by = "doi") %>%
  right_join(full_list_articles_to_do[, c("Title", "PMID", "Create Date", "notes")],
             by = c("PMID")) %>%
  mutate(included = !is.na(n_questions)) %>%
  select(row_names, PMID, doi, Title, `Create Date`, included, notes, n_questions, same, differences, to_review) %>%
  arrange(row_names, `Create Date`)


# done_info %>% group_by(reviewer) %>% summarise(counts = sum(to_review))
write_excel_csv2(done_info,
                "data/literature_search_pubv1/extraction_articles/results_summary.csv")


# export code pour review par BL et ASL -----------------------------------

to_review_objective <- long_results_w_notes %>%
  mutate(decision = ifelse(is.na(decision), "ZZZZ",
                           ifelse(decision == "XXXX", NA_character_, decision)
  ))
to_review_objective %>% filter(reviewer == "DH") %>% write_csv2("data/extraction/to_review/to_review_BZ.csv")
to_review_objective %>% filter(reviewer == "JL") %>% write_csv2("data/extraction/to_review/to_review_ASL.csv")

# Export results for third reviewer ----------------------------------------------------------

long_results_w_notes %>% write_excel_csv2(paste0(
  "data/literature_search_pubv1/extraction_articles/comparison_answers",
  as.Date(lubridate::now(), format = "yyMMDD"),
  ".csv"),
  eol = "\r\n")

for (initials in c("JL", "DH")) {
  dir_results <- file.path(dir_data, "extraction_articles", initials)
  if (!dir.exists(dir_results)) dir.create(dir_results)
  result_reviewer <- long_results_w_notes %>%
    filter(reviewer == initials)

  for (pmid in unique(result_reviewer$PMID)) {
    data <- result_reviewer %>%
      filter(PMID == pmid)
    row_name <- as.character(unique(done_info[done_info$PMID == pmid, "row_names"]))
    file_name <- file.path(dir_results, paste0(str_pad(row_name, width = 3, pad = "0"), "_", pmid, ".csv"))
    # file_name <- file.path(dir_results, paste0(str_pad(row_name, width = 3, pad = "0"), "_", pmid, ".csv"))
    # if (!file.exists(file_name))
    data %>%
      select(-reviewer) %>%
      write_excel_csv2(file = file_name)
  }
  ## writing batch zip file every 10 new articles
  # file_infos <- file.info(list.files(dir_results, full.names = TRUE))
  # file_infos$path <- row.names(file_infos)
  # file_infos %>%
  #   mutate(day_creation = lubridate::date(ctime)) %>%
  #   arrange(day_creation, path) %>%
  #   mutate(n_row = 1:nrow(.)) %>%
  #          # batch = (n_row - 1) %/% 10 + 1) %>%
  #   # group_by(batch) %>%
  #   group_walk(.f = function(df, group_name_df) {
  #     paths <- pull(df, path)
  #     group_name <- pull(group_name_df, batch)
  #     if (length(paths) < 10) {
  #       cat("returning", as.character(group_name), "\n")
  #       return()
  #     }
  #     zip_name <- file.path(dir_results,
  #                           paste0(group_name, "_", initials, ".zip")
  #     )
  #     # j option flag to avoid nested directory, other flags are just default values
  #     if (! file.exists(zip_name)) zip(zip_name, paths, flags = '-r9Xj')
  #   })
}


# create temp results file ------------------------------------------------

if (2 + 2 == 3) {
  long_results_w_notes %>%
    mutate(answer = ifelse(decision == "XXXX", "", decision),
           answer = ifelse(answer == "", ifelse(`reviewer 1` != "", `reviewer 1`, `reviewer 2`), answer)) %>%
    select(doi, PMID, section, `ITC num`, `Ind studies num`, questions, answer) %>%
    write_excel_csv2("data/extraction/extraction_results.csv")
}


# unique molecule names ---------------------------------------------------

list_replacing_ttt_manual <- c(
  "etoposide-ifosfamide" = "etoposide - ifosfamide",
  "bendamustine- brentuximab" = "bendamustine - brentuximab",
  "vinblastine-procarbazine" = "vinblastine - procarbazine",
  "brentuximab-bendamustine" = "brentuximab - bendamustine",
  "vinblastine-procarbazine" = "vinblastine - procarbazine",
  "methotrexate + pl mtx-hydrocortancyl" = "mtx - hydrocortancyl",
  "mogamulizumab + pl infiltree triple (aracytine, mtx, hydrocortancyl)" = "mogamulizumab - aracytine - mtx - hydrocortancyl",
  "rituximab-gemcitabine-oxaliplatine" = "rituximab - gemcitabine - oxaliplatine"
)
str_transformation <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    str_squish() %>%
    replace_in_vec(named_list = list_replacing_ttt_manual) %>%
    str_split("(\\+)|(\\s-\\s)|(\\set\\s)")
}

sort_alphabetically <- function(x) {
  x[str_order(x)]
}


unique_ttt_names <- long_results_w_notes %>%
  filter(questions %in% c("Treatment name 1", "Treatment name 2")) %>%
  pivot_longer(cols = c("reviewer 1", "reviewer 2", "decision"),
               names_to = "answer_type",
               values_to = "answer") %>%
  filter(! answer %in% c("", "XXXX")) %>%
  pull(answer) %>%
  unique() %>%
  str_transformation() %>%
  unlist() %>%
  c() %>%
  str_squish() %>%
  unique() %>%
  sort_alphabetically()

df_ttt <- tibble(ttt_name = unique_ttt_names, simplified = c(""), category_1 = c(""), category_2 = c(""))
write_excel_csv2(df_ttt, "data/extraction/ttt_names.csv")

unique_condition_names <- long_results_w_notes %>%
  filter(questions %in% c("Medical Condition of Interest Name")) %>%
  pivot_longer(cols = c("reviewer 1", "reviewer 2", "decision"),
               names_to = "answer_type",
               values_to = "answer") %>%
  pull(answer) %>%
  unique() %>%
  str_squish() %>%
  unique() %>%
  sort_alphabetically()

df_condition <- tibble(condition_name = unique_condition_names,
                       simplified = c(""),
                       category_1 = c(""),
                       category_2 = c(""))

write_excel_csv2(df_condition, "data/extraction/condition_names.csv")

# unique treatment names --------------------------------------------------


