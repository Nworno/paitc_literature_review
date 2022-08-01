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
  mutate(PMID = as.character(PMID))


# articles done -----------------------------------------------------------

followup <- extraction_df %>%
  select(`DOI of the article`, `Initials of the reviewer filling the form`, `Original Timestamp`, itc_number) %>%
  filter(!is.na(itc_number)) %>%
  select(!itc_number) %>%
  rename(doi = `DOI of the article`,
         reviewer = `Initials of the reviewer filling the form`) %>%
  distinct(doi, reviewer) %>%
  mutate(counter = TRUE) %>%
  pivot_wider(names_from = reviewer, values_from = counter, values_fill = FALSE) %>%
  mutate(both = BZ & ASL) %>%
  filter(both)

done <- list_articles_to_do[, c("DOI", "PMID")] %>%
  mutate(reviewer = rep_len(c("DH", "JL"), length.out = n())) %>%
  full_join(followup, by = c("DOI" = "doi"))

View(done)

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

# Looking at duplicates answers
general_information %>%
  get_all_duplicated_pipe(all_of(c("doi", "reviewer", "questions", "answers"))) %>%
  pivot_wider(id_cols = id_names, names_from = questions, values_from = answers) %>%
  View()

# Manually selecting the duplicates to filter out
rows_id_to_discard <- c(
  "10.2217/cer-2021-0216_3_BZ",
  "10.1016/j.ejcsup.2021.06.002_3_BZ",
  "10.1016/j.ejcsup.2021.06.002_4_BZ",
  "10.1016/j.ejcsup.2021.06.002_5_BZ",
  "10.1016/j.ejcsup.2021.06.002_6_BZ",
  "10.2147/CMAR.S325043_2_BZ",
  "10.1210/clinem/dgab905_2_BZ"
)

general_information_dm <- general_information %>%
  filter(!row_id %in% rows_id_to_discard) %>%
  select(!n_itc)

# study_information -------------------------------------------------------

wide_study_information <- study_information %>%
  select(all_of(c(id_names, "section", "answers", "questions"))) %>%
  separate(questions,
           into = c("questions", "number"),
           sep = "_(?=[0-9]$)",
           fill = "right") %>%
  pivot_wider(names_from = "questions", values_from = "answers")

wide_study_information %>%
  get_all_duplicated_pipe(doi, reviewer, study_number) %>%
  arrange(doi, reviewer, study_number) %>%
  View()

rows_id_to_discard <- c(
  "10.1210/clinem/dgab905_2_BZ",
  "10.1016/j.ejcsup.2021.06.002_3_BZ",
  "10.2147/CMAR.S325043_2_BZ"
)

wide_study_information %>%
  filter(!row_id %in% rows_id_to_discard) %>%
  get_all_duplicated_pipe(doi, reviewer, study_number) %>%
  arrange(doi, reviewer, study_number) %>%
  View()


study_information_dm <- wide_study_information %>%
  filter(!row_id %in% rows_id_to_discard) %>%
  select(!number) %>%
  pivot_longer(cols = !all_of(c(id_names, "study_number", "section")),
               names_to = "questions",
               values_to = "answers"
  ) %>%
  select(!n_itc)


# methodology -------------------------------------------------------------

methodology %>% get_all_duplicated_pipe(id_names, questions)


# results -----------------------------------------------------------------

results %>% get_all_duplicated_pipe(id_names, questions)


# combining sections ------------------------------------------------------

order_sections <- data.frame(order_sections = 1:4,
                             section = c("general_information", "study_information", "methodology", "results"))

long_results_dm <- bind_rows(general_information_dm, study_information_dm, methodology, results) %>%
  select(!row_id) %>%
  select(doi, n_itc, study_number, section, questions, section, everything()) %>%
  mutate(answers = str_squish(str_to_lower(answers))) %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, n_itc) %>%
  select(!order_sections) %>%
  pivot_wider(names_from = reviewer, values_from = answers) %>%
  mutate(identical = ASL == BZ,
         decision = if_else(identical, BZ, NA_character_)) %>%
  replace_na(replace = list(n_itc = "", study_number = "", BZ = "", ASL = "", decision =  "", identical = "")) %>%
  rename(`Individual Studies Number` = study_number,
         `ITC Number` = n_itc
  ) %>%
  filter() %>%
  left_join(done[, c("DOI", "PMID", "both", "reviewer")],
            by = c("doi" = "DOI")) %>%
  select(doi, PMID, everything())


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
    result_reviewer %>%
      filter(PMID == pmid) %>%
      select(!both) %>%
      select(!reviewer) %>%
      write_tsv(file = file.path(dir_results, paste0(pmid, ".tsv")))
  }
}

