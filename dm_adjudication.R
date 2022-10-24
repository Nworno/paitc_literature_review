library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)

source("R_snippets.R")

WRITE <- FALSE
dir_data <- "data"


# DM adjudication first part ----------------------------------------------

first_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part_ASL.csv"),
                           col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                           col_types = c("cccccc"))
first_part_BZ <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part_BZ.csv"),
                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                          col_types = c("cccccc"))
first_part <- bind_rows(first_part_ASL, first_part_BZ)


# DM adjudication: second_part

## Belkacem results
files_BZ <- list.files(file.path(dir_data, "extraction/adjudicated/second_part_BZ"), full.names = TRUE)
second_part_BZ <- do.call(bind_rows, lapply(X = files_BZ,
                                          FUN = read_csv2,
                                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                                          col_types = c("cccccc")))
## Arnaud's results
second_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/second_part_ASL.csv"),
                            col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                            col_types = c("cccccc"))

TEMPORARY <- TRUE
if (TEMPORARY) {
  second_part_ASL_BZ <- bind_rows(second_part_ASL, second_part_BZ)
  temp_results_second_part <- read_csv2("data/extraction/comparison_answers2022-09-21.csv") %>%
    mutate(across(.fns = as.character)) %>%
    mutate(decision = ifelse(decision == "XXXX", "", decision),
           decision = ifelse(decision == "", ifelse(`reviewer 2` != "", `reviewer 2`, `reviewer 1`), decision)) %>%
    select(doi, PMID, section, `ITC num`, `Ind studies num`, questions, decision) %>%
    anti_join(second_part_ASL_BZ, by = c("doi")) %>%
    anti_join(first_part, by = c("doi", "section", "Ind studies num", "questions"))
  second_part <- bind_rows(second_part_ASL_BZ, temp_results_second_part)
} else {
## Jerome's results
  files_JL <- list.files(file.path(dir_data, "extraction/adjudicated/second_part_JL"), full.names = TRUE)
  second_part_JL <- do.call(bind_rows, lapply(X = files_JL,
                                              FUN = read_csv2,
                                              col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                                              col_types = c("cccccc")))
  ## David's results
  files_DH <- list.files(file.path(dir_data, "extraction/adjudicated/second_part_DH"), full.names = TRUE)
  second_part_DH <- do.call(bind_rows, lapply(X = files_DH,
                                              FUN = read_csv2,
                                              col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                                              col_types = c("cccccc")))

  second_part <- bind_rows(second_part_ASL, second_part_BZ, second_part_DH, second_part_JL)
}

second_part_subset <- anti_join(second_part, first_part, by = c("doi", "ITC num", "Ind studies num", "questions", "decision"))

results_dm <- bind_rows(first_part, second_part) %>%
  arrange(doi, section, `Ind studies num`, `ITC num`) %>%
  write_tsv("data/extraction/results_dm.tsv")

# Merge sections ----------------------------------------------------------



# create temp results file ------------------------------------------------

if (2 + 2 == 5) {
  long_results_w_notes %>%
    mutate(answer = ifelse(decision == "XXXX", "", decision),
           answer = ifelse(answer == "", ifelse(`reviewer 1` != "", `reviewer 1`, `reviewer 2`), answer)) %>%
    rename(answer = decision,
           `ITC num` = "n_itc",
           `Ind studies num` = "study_number") %>%
    select(doi, PMID, section, n_itc, study_number, questions, answer) %>%
    write_excel_csv("data/extraction/extraction_results.csv")
}
write_excel_csv2(df_ttt, "data/extraction/ttt_names.csv")
write_excel_csv2(df_condition, "data/extraction/condition_names.csv")




# Integrating reviews BZ and ASL into results -------------------------------

review_ASL <- read_csv("data/extraction/to_review/to_review_ASL_done.csv",
                       col_types = list(identical = "l",
                                        PMID = "c",
                                        `Ind studies num` = "c",
                                        `ITC num` = "c"))
review_BZ <- read_csv("data/extraction/to_review/to_review_BZ_done.csv",
                      col_types = list(identical = "l",
                                       PMID = "c",
                                       `Ind studies num` = "c",
                                       `ITC num` = "c"))
results_objective_adj <- bind_rows(review_ASL, review_BZ) %>%
  select(doi, PMID, section, `ITC num`, `Ind studies num`, questions, decision)



# Integrating reviews DH and JL into results --------------------------------
if (FALSE) {
  review_DH <- read_csv("data/extraction/to_review/to_review_DH_done.csv",
                        col_types = list(identical = "l",
                                         PMID = "c",
                                         `Ind studies num` = "c",
                                         `ITC num` = "c"))
  review_JL <- read_csv("data/extraction/to_review/to_review_JL_done.csv",
                        col_types = list(identical = "l",
                                         PMID = "c",
                                         `Ind studies num` = "c",
                                         `ITC num` = "c"))

  results_adjudication <- bind_rows(review_DH, review_JL) %>%
    select(doi, PMID, section, `ITC num`, `Ind studies number`, questions, decision)
} else {
  results_adjudication <- long_results_w_notes %>%
    filter(section %in% c("methodology", "results")) %>%
    mutate(decision = ifelse(decision == "XXXX", "", decision),
           decision = ifelse(decision == "", ifelse(`reviewer 1` != "", `reviewer 1`, `reviewer 2`), decision)) %>%
    select(doi, PMID, section, `ITC num`, `Ind studies num`, questions, decision)
}
long_results_final <- bind_rows(results_objective_adj, results_adjudication) %>%
  rename(answer = "decision",
         n_itc = "ITC num",
         study_number = "Ind studies num") %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, n_itc, study_number) %>%
  select(-order_sections)


