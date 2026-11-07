# Download Sri Lanka weekly dengue updates (weeks 1-43, 2026) and tidy Table 2 (page 2)
# Packages: pdftools, dplyr, purrr, stringr, tidyr, readr
# install.packages(c("pdftools","dplyr","purrr","stringr","tidyr","readr"))

library(pdftools); library(dplyr); library(purrr)
library(stringr);  library(tidyr);  library(readr)


dir.create("dengue_pdfs", showWarnings = FALSE)
year  <- 2026
weeks <- 1:43
base  <- "https://www.dengue.health.gov.lk/wp-content/uploads"

# ---- 1. Download -----------------------------------------------------------
# The upload month folder differs per week, so try each month until one works.
# Week N is normally uploaded in the month it ends or the following month.
try_download <- function(wk) {
  dest <- file.path("dengue_pdfs", sprintf("Week-%02d.pdf", wk))
  if (file.exists(dest) && file.size(dest) > 10000) return(dest)
  fname <- sprintf("Weekly-Dengue-Update-%d-Week-%02d.pdf", year, wk)
  # start with the most likely month (approx. week -> month), then try the rest
  guess  <- min(12, max(1, ceiling(wk / 4.35)))
  months <- unique(c(guess, guess + 1, guess - 1, 1:12))
  months <- months[months >= 1 & months <= 12]
  for (m in months) {
    url <- sprintf("%s/%d/%02d/%s", base, year, m, fname)
    ok <- tryCatch({
      suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE))
      file.exists(dest) && file.size(dest) > 10000 &&
        identical(readBin(dest, "raw", 4), charToRaw("%PDF"))
    }, error = function(e) FALSE)
    if (ok) return(dest)
    if (file.exists(dest)) file.remove(dest)
  }
  # Some weeks may use a slightly different file name (e.g. Week-5 instead of Week-05)
  fname2 <- sprintf("Weekly-Dengue-Update-%d-Week-%d.pdf", year, wk)
  for (m in months) {
    url <- sprintf("%s/%d/%02d/%s", base, year, m, fname2)
    ok <- tryCatch({
      suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE))
      file.exists(dest) && file.size(dest) > 10000 &&
        identical(readBin(dest, "raw", 4), charToRaw("%PDF"))
    }, error = function(e) FALSE)
    if (ok) return(dest)
    if (file.exists(dest)) file.remove(dest)
  }
  warning("Could not download week ", wk); NA_character_
}
files <- set_names(map_chr(weeks, try_download), weeks)

# ---- 2. Read Table 2 on page 2 --------------------------------------------
# Inspect one page first to confirm the layout:
#   cat(pdf_text(files[["42"]])[2])

parse_table2 <- function(path, wk) {
  if (is.na(path)) return(NULL)
  txt   <- pdf_text(path)[2]
  lines <- str_split(txt, "\n")[[1]] |> str_trim()
  lines <- lines[lines != ""]
  
  # keep only the block that starts at "Table 2" and stops at "Table 3"/"Figure"
  start <- which(str_detect(lines, regex("^Table\\s*2", ignore_case = TRUE)))[1]
  if (is.na(start)) return(NULL)
  end <- which(str_detect(lines, regex("^(Table\\s*3|Figure)", ignore_case = TRUE)))
  end <- end[end > start][1]
  blk <- lines[(start + 1):(if (is.na(end)) length(lines) else end - 1)]
  
  # data rows = a label followed by 2+ numbers
  rows <- blk[str_detect(blk, "^[A-Za-z].*\\s\\d[\\d,\\.]*(\\s+[\\d,\\.%-]+)+$")]
  out <- map(rows, function(l) {
    m <- str_match(l, "^(.*?)\\s+((?:[\\d,\\.%-]+\\s*)+)$")
    nums <- str_split(str_trim(m[3]), "\\s+")[[1]]
    tibble(label = str_trim(m[2]), idx = seq_along(nums), value = nums)
  }) |> bind_rows(.id = "row")
  
  mutate(out, week = wk)
}

raw <- map2(files, weeks, parse_table2) |> bind_rows()

# ---- 3. Tidy ---------------------------------------------------------------
# Columns are named V1, V2, ... -- rename them once you see the real headers
# in Table 2 (e.g. c("cases_this_week","cases_cum_2026","cases_cum_2025")).
tidy <- raw |>
  mutate(value = parse_number(value),
         col   = paste0("V", idx)) |>
  select(week, label, col, value) |>
  pivot_wider(names_from = col, values_from = value) |>
  arrange(week)

# col_names <- c("district", ...)  # adjust after checking the table header
write_csv(tidy, "dengue_table2_tidy.csv")
print(tidy, n = 20)

#####################
library(tidyverse)

parsed <- tidy |>
  mutate(label = str_squish(label)) |>
  # drop province/district header rows
  filter(!str_detect(label, "^[A-Z ]+$"), !str_detect(label, "District")) |>
  mutate(m = str_match(
    label,
    "^([A-Za-z .'-]+?)(?:\\s+(\\d+))?(?:\\s+(\\d+))?(?:\\s+([A-Za-z].*))?$"
  )) |>
  transmute(
    week,
    left_area  = m[, 2],
    left_1     = as.numeric(m[, 3]),
    left_2     = as.numeric(m[, 4]),
    right_area = m[, 5],
    V1, V2
  )

# Left panel: numbers are in the label, or in V1/V2 if there is no right panel
left <- parsed |>
  transmute(week, area = left_area,
            c1 = if_else(is.na(right_area), V1, left_1),
            c2 = if_else(is.na(right_area), V2, left_2))

# Right panel: numbers are always V1/V2
right <- parsed |>
  filter(!is.na(right_area)) |>
  transmute(week, area = right_area, c1 = V1, c2 = V2)

tidy_final <- bind_rows(left, right) |>
  filter(!is.na(area))
write_csv(tidy_final, "tidy_final_2026.csv")
