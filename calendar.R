# ...existing code...
#!/usr/bin/env Rscript
# Convert CSV of date ranges into a months x days (1..31) grid of 0/1
# Usage: Rscript calendar.R input.csv output.csv [start_col end_col date_format]
# Defaults: start_col="Out", end_col="Return", accepts common date formats (DD/MM/YYYY, YYYY-MM-DD, etc.)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript calendar.R input.csv output.csv [start_col end_col date_format]")
}
infile <- args[1]
outfile <- args[2]
start_col <- if (length(args) >= 3) args[3] else "Out"
end_col   <- if (length(args) >= 4) args[4] else "Return"
date_fmt  <- if (length(args) >= 5) args[5] else ""

df <- read.csv(infile, stringsAsFactors = FALSE, check.names = FALSE)

if (!(start_col %in% names(df) && end_col %in% names(df))) {
  stop(sprintf("Columns not found in input CSV: %s, %s", start_col, end_col))
}

parse_dates_vec <- function(x, fmt = "") {
  parse_with <- function(vec, f) {
    res <- as.Date(vec, format = f)
    res
  }
  if (fmt != "") {
    res <- parse_with(x, fmt)
    return(res)
  }
  formats <- c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d-%m-%Y")
  for (f in formats) {
    res <- parse_with(x, f)
    if (any(!is.na(res))) {
      return(res)
    }
  }
  as.Date(x)
}

starts_all <- parse_dates_vec(df[[start_col]], date_fmt)
ends_all   <- parse_dates_vec(df[[end_col]], date_fmt)

valid_idx <- which(!is.na(starts_all) & !is.na(ends_all))
if (length(valid_idx) == 0) stop("No valid start/end date pairs found in input.")

starts <- starts_all[valid_idx]
ends   <- ends_all[valid_idx]

swap_idx <- which(starts > ends)
if (length(swap_idx) > 0) {
  tmp <- starts[swap_idx]
  starts[swap_idx] <- ends[swap_idx]
  ends[swap_idx] <- tmp
}

min_date <- min(starts)
max_date <- max(ends)

first_month <- as.Date(format(min_date, "%Y-%m-01"))
last_month  <- as.Date(format(max_date, "%Y-%m-01"))
months_seq <- seq(first_month, last_month, by = "month")

days <- 1:31
n_months <- length(months_seq)
grid <- matrix(0L, nrow = n_months, ncol = length(days))
colnames(grid) <- sprintf("%02d", days)
row_names <- format(months_seq, "%Y-%m")

for (i in seq_along(months_seq)) {
  m <- months_seq[i]
  yr <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m"))
  for (d in days) {
    dt <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", yr, mo, d), format = "%Y-%m-%d"), error = function(e) NA)
    if (!is.na(dt)) {
      if (any(dt >= starts & dt <= ends, na.rm = TRUE)) grid[i, d] <- 1L
    }
  }
}

out_df <- data.frame(month = row_names, stringsAsFactors = FALSE)
out_df <- cbind(out_df, as.data.frame(grid, stringsAsFactors = FALSE))

write.csv(out_df, file = outfile, row.names = FALSE, quote = FALSE)
cat(sprintf("Wrote grid to %s (months: %d, days: 1..31)\n", outfile, n_months))
# ...existing code...