library("readr")
library("tidyr")
library("dplyr")
library("stringr")
library("purrr")
library("data.table")

folder_raw_extraction <- "C:/Users/aserret-larmande/Documents/these/systematic_review_implementation/implementation/extractions/pub_finale_v1/"

folder_input_rayyan <- "data/pubv1"


# Formatting file from pubmed to rayyan format ----------------------------
file_raw <- fread(file = file.path(folder_raw_extraction, "pubmed-indirecttr-set_abstract.txt"),
                  encoding = "UTF-8",
                  header = FALSE,
                  fill = TRUE,
                  strip.white = FALSE,
                  sep=""
)

file_sep <- file_raw %>% separate(col = V1,
                          into = c("field", "value"),
                          sep= 6,
                          extra = "merge")

file_sep$field <- stringr::str_extract(file_sep$field, pattern = "\\w+")

## Finding articles
n_articles <- sum(file_sep$field == "PMID", na.rm = TRUE)
indices_begin_articles <- which(file_sep$field == "PMID") # list of limits (first row) between articles
indices_end_articles <- c(indices_begin_articles[-1], nrow(file_sep) + 1)
span_articles <- indices_end_articles - indices_begin_articles
indices_articles <- rep(1:n_articles, span_articles)
file_sep$n_article <- indices_articles

# removing blank lines
bool_blank_lines <- is.na(file_sep$field) & file_sep$value == ""
file_sep <- file_sep[!bool_blank_lines,]


# Replace field Na with previous values in the column field
file_sep <- fill(data = file_sep, field, .direction = "down" )

formatted <- file_sep %>% pivot_wider(
  id_cols = n_article,
  names_from = field,
  values_from = value,
  values_fn = function(x) paste(x, collapse = " ") # To refine this, could use collapse = "~" to paste values in one column, and then
  # replace this character by blank spaces in text fields and ";" or  anything relevant in structured fields
)

write_csv(formatted, file.path(folder_input_rayyan, "extraction_pubv1_wide_format.csv"))

# Converting to Rayyan csv format
formatted_rayyan <- formatted %>%
  rename(key = n_article,
         title = TI,
         authors = AU,
         journal = JT,
         date = DP,
         pubmed_id = PMID,
         abstract = AB
) %>%
  mutate(volume = NA,
         issue = NA,
         issn = NA,
         pages = NA,
         day = NA,
         month = NA,
         year = NA,
         publisher = NA,
         pmc_id = NA,
         url = NA,
         notes = NA) %>%
  select(key ,
         title,
         authors,
         journal,
         issn,
         volume,
         issue,
         pages,
         day,
         month,
         year,
         publisher,
         pmc_id,
         pubmed_id,
         url,
         abstract,
         notes)
formatted_rayyan %>%
  write_csv(file.path(folder_input_rayyan, "extraction_pubv1_formatted_rayyan_input.csv"))



# Merging pubmed format and rayyan output ---------------------------------

rayyan_output_folder <- "export_rayyan_220428"
rayyan_output <- read_csv(file.path(folder_input_rayyan, rayyan_output_folder, "articles.csv"),
                          col_types = cols_only(pubmed_id = "c",
                                                key = "c",
                                                notes = "c")
) %>%
  rename(rayyan_key = key)

merged <- left_join(formatted, rayyan_output, by = c("PMID" = "pubmed_id"))

subset <- merged %>%
  filter(!is.na(notes)) %>%
  select(PMID, TI, notes)

# Parsing rayyan export format
list_strings <- subset$notes %>%
  str_split("\\|") %>%
  map(str_trim) %>%
  map(str_split, pattern = ":", n = 2) %>%
  map_depth(2, .f = function(x) {
    values <- c(x[[2]])
    names(values) <- x[[1]]
    return(values)
  }
  ) %>%
  map(.f = unlist) %>%
  tibble() %>%
  unnest_wider(col = ".") %>%
  rename(inclusion = "RAYYAN-INCLUSION",
         labels = "RAYYAN-LABELS",
         exclusion = "RAYYAN-EXCLUSION-REASONS",
         user_notes = "USER-NOTES") %>%
  mutate(inclusion = str_extract(inclusion, "(Maybe)|(Included)|(Excluded)")) %>%
  mutate(across(.cols = c(labels, exclusion, user_notes), str_trim))


stopifnot(nrow(list_strings) == nrow(subset))
df_rayyan_notes <- bind_cols(subset, list_strings)


df_rayyan_notes %>%
  filter(inclusion == "Maybe") %>%
  `[`(,"PMID", drop = TRUE) %>%
  cat(file = file.path(folder_input_rayyan, rayyan_output_folder, "pmids_maybe.txt"), sep = "\n")

