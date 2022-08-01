library(magrittr)
library(dplyr)

get_all_duplicated_ind <- function(data) {
  dup <- duplicated(data)
  dup_from_last <- duplicated(data, fromLast = TRUE)
  dup | dup_from_last
}

# Get all the duplicated rows of a data frame for a subset of column, but returning all the columns
get_all_duplicated_pipe <- function(data, ...) {
  indices_dup <- select(data, ...) %>% get_all_duplicated_ind()
  filter(data, indices_dup)
}

drop_element_vector <- function(vec, elements){
  vec[!vec %in% elements]
}

drop_dup_vec <- function(vec) {
  vec[!duplicated(vec)]
}

order_vec <- function(vec, ...) {
  vec[order(vec, ...)]
}

replace_in_vec <- function(vec, named_list) {
  indices_to_rename <- which(vec %in% names(named_list))
  vec[indices_to_rename] <- named_list[vec[indices_to_rename]]
  return(vec)
}


rename_w_named_list <- function(df, named_list) {
  indices_to_rename <- which(names(df) %in% names(named_list))
  names(df)[indices_to_rename] <- named_list[names(df)[indices_to_rename]]
  return(df)
}

replace_na_vec <- function(vec, rep) {
  ifelse(is.na(vec) | is.infinite(vec), rep, vec)
}


binary_as_factor <- function(variable) {
  # It has been checked that it always take the lowest value of a numerical
  # as the first level
  if (length(unique(variable[!is.na(variable)])) %in% c(1, 2)) {
    variable <- as.factor(variable)
  }
  variable
}

as_classes <- function(variable, type, format_date = NULL) {
  switch(EXPR = type,
         logical = as.logical(variable),
         numeric = as.numeric(as.character(variable)),
         character = as.character(variable),
         factor = as.factor(variable),
         Date = as.Date(variable, format = format_date),
         stop())
}

transform_df_to_ascii <- function(df) {
  class_df <- ifelse(class(df)[[1]] %in% c("tbl_df", "spec_tbl_df"),
                     "tibble",
                     ifelse(class(df)[[1]] == "data.frame",
                            "data.frame",
                            error())
  )
  names_ascii <- stringi::stri_trans_general(names(df), "latin-ascii")
  cols_ascii <- lapply(df,
                       function(x) {
                         if (is.character(x)) {
                           x <- stringi::stri_trans_general(x, "latin-ascii")
                         } else if (is.factor(x)) {
                           levels(x) <- stringi::stri_trans_general(levels(x), "latin-ascii")
                         }
                         return(x)
                       }
  )
  df_ascii <- if (class_df == "tibble") {
    as_tibble(cols_ascii)
  } else if (class_df == "data.frame") {
    as.data.frame(cols_ascii)
  } else {
    error()
  }
  names(df_ascii) <- names_ascii
  return(df_ascii)
}

# Named list to data frame
named_list_df <- list(
  "general_information" = c(
    "Medical Condition of Interest Name",
    "Initials of the reviewer filling the form",
    "Mention of a systematic review to find the studies to compare treatments of interest"
  ), "study_information" = c(
    "Patient-level data used",
    "Number of treatment arms (clinical trial only)"
  ), "methodology" = c(
    "Treatment name 1",
    "Discussion of the choice of the scale for the outcome in the main text (ie natural outcome scale vs transformed outcome scale)"
  )
) %>%
  tibble::enframe() %>%
  tidyr::unnest_longer(col = value)


FitFlextableToPage <- function(ft, pgwidth = 8){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


# Using variable from environment in tidyverse language
## Reference about programming in tidyverse is here: https://dplyr.tidyverse.org/articles/programming.html
### Mostly adresses the parsing of strings as arguments from functions, moslty using embrace with {{}}
my_summarise3 <- function(data, mean_var, sd_var) {
  data %>%
    summarise(mean = mean({{ mean_var }}), sd = sd({{ sd_var }}))
}

## For the common case of using a symbol from the environment to avoid repetition, use !!rlang::sym(my_variable) (found here https://stackoverflow.com/questions/46865718/convert-string-to-symbol-accepted-by-dplyr-in-function)
### For more information, see ?topic-defuse
miles_per_gallon <- "mpg"
mtcars %>% dplyr::mutate(miles_per_liters = !!rlang::sym(miles_per_gallon) / 3.785)
mtcars %>% dplyr::mutate(miles_per_liters = mpg / 3.785)

