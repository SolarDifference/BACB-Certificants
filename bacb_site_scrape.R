library(rvest)
library(plyr)
library(XML)
library(tidyverse)
library(tsibble)
library(datasets)
library(countrycode)

# Scrape the BACB data site for current monthly certificant numbers
# These data show the total cumulative certificants per subregion in the United States
# Once data are imported, they're converted into a tibble

url <- "https://www.bacb.com/services/o.php?page=101134"



# grab page
doc <- read_html(url)

# get all visible text (preserves the order the page presents)
page_txt <- doc |> html_text2()

# -------- helper to parse a block of "name + five numbers" rows --------
parse_name_5nums <- function(block_txt, parent_region, type_label) {
  # One row looks like:
  # <Name>\n<BCBA-D>\n<BCBA>\n<BCaBA>\n<RBT>\n<Total>
  # We'll match those six consecutive fields repeatedly.
  rx <- "(?m)^([\\p{L}0-9 .,&’'()\\-/]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)\\s+([0-9,]+)"
  mats <- stringr::str_match_all(block_txt, rx)[[1]]
  if (is.null(mats) || nrow(mats) == 0) {
    return(tibble(
      parent_region = character(0),
      name = character(0),
      bcba_d = double(0),
      bcba = double(0),
      bcaba = double(0),
      rbt = double(0),
      total = double(0),
      type = character(0),
      scraped_at = with_tz(now(), "America/Denver")
    ))
  }
  tibble(
    parent_region = parent_region,
    name  = mats[, 2] |> str_squish(),
    bcba_d = mats[, 3] |> str_remove_all(",") |> as.numeric(),
    bcba   = mats[, 4] |> str_remove_all(",") |> as.numeric(),
    bcaba  = mats[, 5] |> str_remove_all(",") |> as.numeric(),
    rbt    = mats[, 6] |> str_remove_all(",") |> as.numeric(),
    total  = mats[, 7] |> str_remove_all(",") |> as.numeric(),
    type   = type_label,
    scraped_at = with_tz(now(), "America/Denver")
  )
}

# -------- (A) region totals (top 5 rows) --------
pluck_region <- function(txt, region_label) {
  pat <- paste0("(?s)\\b", stringr::fixed(region_label),
                "\\s*([0-9,]+)\\s*([0-9,]+)\\s*([0-9,]+)\\s*([0-9,]+)\\s*([0-9,]+)")
  m <- str_match(txt, pat)
  if (is.na(m[1, 1])) {
    return(tibble(
      region = region_label,
      bcba_d = NA_real_, bcba = NA_real_, bcaba = NA_real_, rbt = NA_real_, total = NA_real_,
      scraped_at = with_tz(now(), "America/Denver")
    ))
  }
  nums <- m[1, 2:6] |> str_remove_all(",") |> as.numeric()
  tibble(
    region = region_label,
    bcba_d = nums[1],
    bcba   = nums[2],
    bcaba  = nums[3],
    rbt    = nums[4],
    total  = nums[5],
    scraped_at = with_tz(now(), "America/Denver")
  )
}

# List of states
states <- state.name

# List of countries
countries <- countrycode::codelist$country.name.en 

# Full list of locations to scrape
regions <- c(states, countries)

# Apply scraping function to all regions of interest
bacb_totals_tbl <- purrr::map_dfr(regions, ~pluck_region(page_txt, .x)) 

# Simple step converting date to date from dttm
bacb_totals_modified <- bacb_totals_tbl |> 
  mutate(date = as_date(paste0(as.character(year(today())), '-',
                               as.character(month(today())), '-01')))

# Import previous data
previous_data <- readxl::read_xlsx('C:/Users/zacha/Proton Drive/My files/R/BACB Site Scraping/data/bacb_certificant_data.xlsx') |> 
  mutate(date = as_date(date))
  
# Join old and new data together
new_data <- rbind(bacb_totals_modified, previous_data) 

# Update old excel file with new data
writexl::write_xlsx(new_data, path = 'C:/Users/zacha/Proton Drive/My files/R/BACB Site Scraping/data/bacb_certificant_data.xlsx')