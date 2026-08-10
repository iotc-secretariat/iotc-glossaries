library(readr)
library(dplyr)
library(stringr)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

input_file  <- "./inputs/iotc-glossary.csv"
output_file <- "./rmd/iotc-glossary-reporting.Rmd"

# Column controlling which terms are included in the glossary
glossary_column <- "glossary_reporting"

# -------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------

make_anchor <- function(term, suffix = NULL) {

  x <- str_squish(term)

  # Remove punctuation, keeping spaces
  x <- str_replace_all(x, "[^[:alnum:] ]", " ")

  words <- str_split(x, "\\s+", simplify = TRUE)
  words <- words[words != ""]

  if (length(words) == 0)
    return("")

  anchor <- paste0(
    str_to_lower(words[1]),
    paste0(
      str_to_upper(str_sub(words[-1], 1, 1)),
      str_to_lower(str_sub(words[-1], 2))
    ),
    collapse = ""
  )

  if (!is.null(suffix) && !is.na(suffix) && str_trim(suffix) != "") {
    suffix <- str_squish(suffix)
    suffix <- str_replace_all(suffix, "[^[:alnum:] ]", " ")
    suffix_words <- str_split(suffix, "\\s+", simplify = TRUE)
    suffix_words <- suffix_words[suffix_words != ""]

    anchor <- paste0(
      anchor,
      paste0(
        str_to_upper(str_sub(suffix_words, 1, 1)),
        str_to_lower(str_sub(suffix_words, 2))
      ),
      collapse = ""
    )
  }

  anchor
}

# Add a reference in parentheses if one exists
add_reference <- function(text, reference) {

  if (is.na(text) || str_trim(text) == "")
    return("")

  text <- str_trim(text)

  if (!is.na(reference) && str_trim(reference) != "") {
    paste0(text, " (", str_trim(reference), ").")
  } else {
    paste0(text, ".")
  }
}


# Remove a final full stop before adding a reference
clean_definition <- function(x) {

  x <- str_trim(x)

  if (x == "")
    return(x)

  str_remove(x, "\\s*\\.$")
}


# -------------------------------------------------------------------------
# Read glossary
# -------------------------------------------------------------------------

glossary <- read_csv(
  input_file,
  na = c("", "NA"),
  show_col_types = FALSE
)


# -------------------------------------------------------------------------
# Check requested column
# -------------------------------------------------------------------------

if (!glossary_column %in% names(glossary)) {
  stop(
    "Column '", glossary_column,
    "' does not exist in the glossary CSV."
  )
}


# -------------------------------------------------------------------------
# Filter glossary
# -------------------------------------------------------------------------

glossary <- glossary %>%
  filter(
    .data[[glossary_column]] == TRUE
  ) %>%
  mutate(
    term = str_squish(term)
  ) %>%
  filter(
    !is.na(term),
    term != ""
  ) %>%
  arrange(
    str_to_lower(term)
  )


# -------------------------------------------------------------------------
# Create Rmd
# -------------------------------------------------------------------------

lines <- c(
  "---",
  "title: \"IOTC Glossary\"",
  "output: github_document",
  "---",
  ""
)


# -------------------------------------------------------------------------
# Generate entries
# -------------------------------------------------------------------------

current_letter <- NULL

for (i in seq_len(nrow(glossary))) {

  row <- glossary[i, ]

  term <- row$term

  # First letter for alphabetical section
  letter <- str_to_upper(str_sub(term, 1, 1))

  # New alphabetical section
  if (!identical(letter, current_letter)) {

    lines <- c(
      lines,
      paste0("### ", letter, " {#", letter, "}"),
      ""
    )

    current_letter <- letter
  }


  # Anchor
  anchor <- make_anchor(term)


  # Optional acronym
  acronym <- row$acronym

  heading <- term

  if (!is.na(acronym) && str_trim(acronym) != "") {
    heading <- paste0(heading, " (", str_trim(acronym), ")")
  }


  # Main heading
  lines <- c(
    lines,
    paste0("#### ", heading, " {#", anchor, "}"),
    ""
  )


  # -----------------------------------------------------------------------
  # Main definition
  # -----------------------------------------------------------------------

  definition <- row$definition
  reference  <- row$reference

  if (!is.na(definition) && str_trim(definition) != "") {

    definition <- clean_definition(definition)

    if (!is.na(reference) && str_trim(reference) != "") {
      definition <- paste0(
        definition,
        " (",
        str_trim(reference),
        ")."
      )
    } else {
      definition <- paste0(definition, ".")
    }

    lines <- c(
      lines,
      definition,
      ""
    )
  }


  # -----------------------------------------------------------------------
  # Alternative definition
  # -----------------------------------------------------------------------

  alt_definition <- row$alternative_definition
  alt_reference  <- row$reference_alternative_definition

  if (!is.na(alt_definition) && str_trim(alt_definition) != "") {

    alt_definition <- clean_definition(alt_definition)

    if (!is.na(alt_reference) && str_trim(alt_reference) != "") {
      alt_definition <- paste0(
        alt_definition,
        " (",
        str_trim(alt_reference),
        ")."
      )
    } else {
      alt_definition <- paste0(
        alt_definition,
        "."
      )
    }

    lines <- c(
      lines,
      "",
      alt_definition,
      ""
    )
  }
}


# -------------------------------------------------------------------------
# Write Rmd
# -------------------------------------------------------------------------

writeLines(
  lines,
  output_file,
  useBytes = TRUE
)

message(
  "Glossary written to: ",
  normalizePath(output_file, mustWork = FALSE)
)
