library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)

source("R_snippets.R")
source("questions_sections.R")

WRITE <- FALSE
dir_data <- "data"


# DM adjudication first part ----------------------------------------------

first_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part/ASL.csv"),
                           col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                           col_types = c("cccccc"))
first_part_BZ <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part/BZ.csv"),
                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                          col_types = c("cccccc"))
first_part <- bind_rows(first_part_ASL, first_part_BZ)


#  DM adjudication: second_part -------------------------------------------

## Loading Belkacem adjudication -----------------------------------------------------

files_BZ <- list.files(file.path(dir_data, "extraction/adjudicated/second_part/BZ"), full.names = TRUE)
second_part_BZ <- do.call(bind_rows, lapply(X = files_BZ,
                                          FUN = read_csv2,
                                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                                          col_types = c("cccccc")))


## Loading Arnaud adjudication -------------------------------------------

second_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/second_part/ASL.csv"),
                            col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                            col_types = c("cccccc"))


# Loading David adjudication ----------------------------------------------
files_DH <- list.files(file.path(dir_data, "extraction/adjudicated/second_part/DH"),
                       full.names = TRUE,
                       recursive = TRUE,
                       pattern = ".csv")

second_part_DH <- lapply(X = files_DH,
                         FUN = read_csv2,
                         col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                         col_types = c("cccccc"))

# PLACEHOLDER: Loading Jerome adjudication ----------------------------------------------
# files_JL <- list.files(file.path(dir_data, "extraction/adjudicated/second_part/JL"),
#                        full.names = TRUE,
#                        recursive = TRUE,
#                        pattern = ".csv")
#
# second_part_JL <- lapply(X = files_JL,
#                          FUN = read_csv2,
#                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
#                          col_types = c("cccccc"))

second_part <- bind_rows(second_part_ASL, second_part_BZ, second_part_DH
                         # , second_part_JL
                         )
# Bring together respective questions for first and second part -----------

second_part_subset <- anti_join(second_part, first_part, by = c("doi", "ITC num", "Ind studies num", "questions", "decision"))

# DM steps:
# - Retrieve supplementary adjudicated columns (total sample size, included covariates)
# - Check for answers with xxxx
# - Check for comparisons with xx

sup_columns <- read_csv(file.path(dir_data,
                                  "extraction/adjudicated/second_part/sup_columns_adjudicated.csv"),
                        col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                        col_types = c("cccccc"))

long_results_final <- bind_rows(first_part, second_part_subset, sup_columns) %>%
  rename(answer = "decision",
         n_itc = "ITC num",
         study_number = "Ind studies num") %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, n_itc, study_number) %>%
  select(-order_sections)

long_results_final %>% write_tsv("data/extraction/data_managed/long_results_final.tsv")


# Treatments and ATC ------------------------------------------------------
ttt_df <- read_tsv("data/mapping/treatment_mapping.tsv") %>%
  mutate("CAR-T cells" = ifelse(!is.na(`CAR-T cells`), TRUE, FALSE)) %>%
  select(ttt_name, `ATC code`, others, `CAR-T cells`, others) %>%
  distinct()

## Handling multiple ATC code per lines ----------------------------------
handling_multiple_codes <- ttt_df[, "ATC code"] %>%
  mutate("ATC code parsed" = str_split(`ATC code`, ";")) %>%
  unnest_longer(`ATC code parsed`) %>%
  distinct()
ttt_df <- ttt_df %>% left_join(handling_multiple_codes, by = "ATC code") %>%
  select(-`ATC code`) %>%
  rename(`ATC code` = `ATC code parsed`)

atc_ancestors <- read_tsv("data/mapping/classification_ATC/CONCEPT_ANCESTOR.csv")
atc_classification <- read_tsv("data/mapping/classification_ATC/CONCEPT.csv") %>%
  select(all_of(c("concept_id", "concept_code", "concept_class_id", "concept_name")))
ttt_mapping <- ttt_df %>%
  select(`ATC code`) %>%
  drop_na(`ATC code`) %>%
  distinct() %>%
  left_join(atc_classification,
                     by = c("ATC code" = "concept_code")) %>%
  left_join(atc_ancestors[c("ancestor_concept_id", "descendant_concept_id")],
            by = c("concept_id" = "descendant_concept_id")) %>%
  left_join(atc_classification,
            by = c("ancestor_concept_id" = "concept_id"),
            suffix = c("", "_ancestor")) %>%
  # Last ancestor level seems useless, but allows to keep the codes without ancestor
  # filter(concept_class_id_ancestor != "ATC 5th") %>%
  rename(concept_code_ancestor = concept_code) %>%
  select(-ancestor_concept_id, concept_name_ancestor) %>%
  distinct() %>%
  pivot_wider(names_from = "concept_class_id_ancestor",
              values_from = c("concept_code_ancestor", "concept_name_ancestor")
  ) %>%
  select(!ends_with(c("5th", "NA"))) %>%
  select(-concept_id)


## manually inserting 2023 incoming atc codes ------------------------------
L01EX <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "L01EX")) %>%
  mutate(concept_name = ifelse(`ATC code` == "L01EX25", "umbralisib: oral", `ATC code`)) %>%
  fill(everything(), .direction = "down")

N06AX29 <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "N06AX29")) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "N",
    `concept_code_ancestor_ATC 2nd` = "N06",
    `concept_code_ancestor_ATC 3rd` = "N06A",
    `concept_code_ancestor_ATC 4th` = "N06AX",
    `concept_name_ancestor_ATC 1st` = "NERVOUS SYSTEM",
    `concept_name_ancestor_ATC 2nd` = "PSYCHOANALEPTICS",
    `concept_name_ancestor_ATC 3nd` = "ANTIDEPRESSANTS",
    `concept_name_ancestor_ATC 4nd` = "OTHER ANTIDEPRESSANTS"
  )

L04AA54 <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "L04AA54")) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "L",
    `concept_code_ancestor_ATC 2nd` = "L04",
    `concept_code_ancestor_ATC 3rd` = "L04A",
    `concept_code_ancestor_ATC 4th` = "L04AA",
    `concept_name_ancestor_ATC 1st` = "NERVOUS ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
    `concept_name_ancestor_ATC 2nd` = "IMMUNOSUPPRESSANTS",
    `concept_name_ancestor_ATC 3nd` = "IMMUNOSUPPRESSANTS",
    `concept_name_ancestor_ATC 4nd` = "Selective immunosuppressants"
  )


car_t_cells <- ttt_mapping %>%
  semi_join(filter(ttt_df, `CAR-T cells` == TRUE)) %>%
  filter(is.na(concept_class_id)) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "L",
    `concept_code_ancestor_ATC 2nd` = "L01",
    `concept_code_ancestor_ATC 3rd` = "L01X",
    `concept_code_ancestor_ATC 4th` = "L01XL",
    `concept_name_ancestor_ATC 1st` = "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
    `concept_name_ancestor_ATC 2nd` = "ANTINEOPLASTIC AGENTS",
    `concept_name_ancestor_ATC 3nd` = "OTHER ANTINEOPLASTIC AGENTS",
    `concept_name_ancestor_ATC 4nd` = "ANTINEOPLASTIC CELL AND GENE THERAPY"
  )

curated <- bind_rows(L01EX, N06AX29, L04AA54, car_t_cells)
atc_mapping_final <- ttt_mapping %>%
  anti_join(curated, by = "ATC code") %>%
  bind_rows(curated)
ttt_df_mapped <- ttt_df %>% left_join(atc_mapping_final, by = "ATC code") %>%
  distinct()

# Checking that all ttt names found in extraction have either ATC ancestors mapped or something written in 'others' column
stopifnot(!is.na(ttt_df_mapped$others) | !is.na(ttt_df_mapped$`concept_code_ancestor_ATC 2nd`))

ttt_df_mapped_final <- ttt_df_mapped %>%
  mutate(final_classification_code = case_when(
    !is.na(`concept_code_ancestor_ATC 4th`) ~ `concept_code_ancestor_ATC 4th`,
    !is.na(`concept_code_ancestor_ATC 3rd`) ~ `concept_code_ancestor_ATC 3rd`,
    !is.na(`concept_code_ancestor_ATC 2nd`) ~ `concept_code_ancestor_ATC 2nd`,
    TRUE ~ NA_character_
    ),
    final_classification_name = case_when(
      !is.na(`concept_name_ancestor_ATC 4th`) ~ `concept_name_ancestor_ATC 4th`,
      !is.na(`concept_name_ancestor_ATC 3rd`) ~ `concept_name_ancestor_ATC 3rd`,
      !is.na(`concept_name_ancestor_ATC 2nd`) ~ `concept_name_ancestor_ATC 2nd`,
      !is.na(others) ~ others,
      TRUE ~ NA_character_
    )) %>%
  distinct(ttt_name, final_classification_code, final_classification_name, `CAR-T cells`)

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
# Function modified as compared to the one used in dm_extraction to simplify ttt names from extraction
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    str_squish() %>%
    replace_in_vec(named_list = list_replacing_ttt_manual) %>%
    str_split("(\\+)|(\\s-\\s)|(\\set\\s)|(\\sor\\s)|(\\sand\\s)|(/)|(followed\\sby)")
}

wide_ttt_parsed <- second_part_subset %>%
  filter(questions %in% c("Treatment name 1", "Treatment name 2")) %>%
  distinct(decision) %>%
  rename(ttt_name = decision) %>%
  mutate(ttt_parsed = str_transformation(ttt_name)) %>%
  unnest_wider(ttt_parsed) %>%
  rename("first" = "...1", "second" = "...2", "third" = "...3") %>%
  mutate(across(.cols = all_of(c("first", "second", "third")),
                .fns = str_squish)
  ) %>%
  distinct()

long_ttt_parsed <- wide_ttt_parsed %>%
  pivot_longer(cols = c("first", "second", "third"),
               names_to = "instance",
               values_to = "ttt_parsed") %>%
  filter(!is.na(ttt_parsed))

ttt_atc_mapped <- long_ttt_parsed %>%
  left_join(ttt_df_mapped_final, by = c("ttt_parsed" = "ttt_name")) %>%
  filter(!is.na(final_classification_name)) %>%
  pivot_wider(id_cols = "ttt_name",
              names_from = "instance",
              values_from = c("final_classification_name", "CAR-T cells")) %>%
  rowwise() %>%
  mutate(car_t_cells = any(`CAR-T cells_first`, `CAR-T cells_second`, `CAR-T cells_third`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(first = final_classification_name_first,
         second = final_classification_name_second,
         third = final_classification_name_third) %>%
  select(ttt_name, first, second, third, car_t_cells)

decision_atc_mapped <- second_part_subset %>%
  filter(questions %in% c("Treatment name 1", "Treatment name 2")) %>%
  left_join(ttt_atc_mapped, by = c("decision" = "ttt_name"))

decision_atc_mapped %>% write_tsv(file.path(dir_data, "extraction/data_managed/decision_atc_mapped.tsv"))


# ICD mapping -------------------------------------------------------------


icd <- read_tsv("data/mapping/classification_ICDWHO/CONCEPT.csv") %>%
  select(all_of(c("concept_id", "concept_code", "concept_class_id", "concept_name")))
concept_relationship <- read_tsv("data/mapping/classification_ICDWHO/CONCEPT_RELATIONSHIP.csv")
conditions_mapping <- read_tsv("data/mapping/conditions_mapping.tsv")

icd <- icd %>%
  filter(str_starts(concept_class_id, "ICD10") & !str_starts(concept_name, "Invalid ICD10"))

# Not possible to use CONCEPT_ANCESTOR.csv because for some reason concept.csv doesn't map to it --> use of concept_relationship somehow
conditions_mapped <- conditions_mapping %>%
  left_join(icd[, c("concept_code", "concept_id")], by = c("ICD-10 code" = "concept_code")) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("concept_id"  = "concept_id_2")) %>%
  rename(ancestor_concept_id_1 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_1" = "concept_id_2")) %>%
  rename(ancestor_concept_id_2 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_2" = "concept_id_2")) %>%
  rename(ancestor_concept_id_3 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_3" = "concept_id_2")) %>%
  rename(ancestor_concept_id_4 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_4" = "concept_id_2")) %>%
  rename(ancestor_concept_id_5 = concept_id_1) %>%
  select(condition_name,
         `ICD-10 code`,
         concept_id,
         ancestor_concept_id_1,
         ancestor_concept_id_2,
         ancestor_concept_id_3,
         ancestor_concept_id_4,
         ancestor_concept_id_5) %>%
  pivot_longer(cols = contains("concept_id"), values_to = "concept_id", names_to = "level") %>%
  filter(!is.na(concept_id)) %>%
  left_join(icd[, c("concept_id", "concept_class_id", "concept_code", "concept_name")], by = c("concept_id")) %>%
  select(-level, -concept_id, -`ICD-10 code`) %>%
  distinct() %>%
  pivot_wider(names_from = c("concept_class_id"),
              values_from = c("concept_code", "concept_name"),
              values_fn = function(...) paste(..., collapse = "; "))


conditions_mapped <- first_part %>%
  filter(questions %in% c("Medical Condition of Interest Name")) %>%
  left_join(conditions_mapped[, c("condition_name", "concept_code_ICD10 Hierarchy")],
            by = c("decision" = "condition_name"))
conditions_mapped %>% write_tsv("data/extraction/data_managed/decision_icd_mapped.tsv")



# ARCHIVE -----------------------------------------------------------------

## create temp results file ------------------------------------------------

TEMPORARY <- FALSE
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



if (2 + 2 == 5) {
  long_results_w_notes %>%
    mutate(answer = ifelse(decision == "XXXX", "", decision),
           answer = ifelse(answer == "", ifelse(`reviewer 1` != "", `reviewer 1`, `reviewer 2`), answer)) %>%
    rename(answer = decision,
           `ITC num` = "n_itc",
           `Ind studies num` = "study_number") %>%
    select(doi, PMID, section, n_itc, study_number, questions, answer) %>%
    write_excel_csv("data/extraction/extraction_results.csv")
  write_excel_csv2(df_ttt, "data/extraction/ttt_names.csv")
  write_excel_csv2(df_condition, "data/extraction/condition_names.csv")
}



## Integrating reviews BZ and ASL into results -------------------------------

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


## Integrating reviews DH and JL into results --------------------------------
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


results_dm <- bind_rows(first_part, second_part_subset) %>%
  arrange(doi, section, `Ind studies num`, `ITC num`) %>%
  write_tsv("data/extraction/results_dm.tsv")

