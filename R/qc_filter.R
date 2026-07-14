# IOOS QARTOD aggregate QC flags treated as usable data.
QC_GOOD_FLAGS <- c(1L, 2L)

QC_CORE_COLUMNS <- c(
  "sea_water_pressure",
  "sea_water_temperature",
  "sea_water_salinity"
)

qc_column_for <- function(data_col, column_names) {
  agg <- paste0(data_col, "_qc_agg")
  if (agg %in% column_names) {
    return(agg)
  }

  plain <- paste0(data_col, "_qc")
  if (plain %in% column_names) {
    return(plain)
  }

  NULL
}

data_columns_with_qc <- function(df) {
  qc_cols <- grep("_qc(_agg)?$", names(df), value = TRUE)
  if (length(qc_cols) == 0) {
    return(character())
  }

  data_cols <- unique(sub("_qc(_agg)?$", "", qc_cols, perl = TRUE))
  data_cols[data_cols %in% names(df)]
}

is_qc_good <- function(qc_values, good_flags = QC_GOOD_FLAGS) {
  qc_num <- suppressWarnings(as.integer(qc_values))
  !is.na(qc_num) & qc_num %in% good_flags
}

apply_qc_filter <- function(
    df,
    good_flags = QC_GOOD_FLAGS,
    core_columns = QC_CORE_COLUMNS,
    drop_qc_columns = FALSE
) {
  if (nrow(df) == 0) {
    return(df)
  }

  out <- df
  data_cols <- data_columns_with_qc(out)

  for (col in data_cols) {
    qc_col <- qc_column_for(col, names(out))
    if (is.null(qc_col)) {
      next
    }
    good <- is_qc_good(out[[qc_col]], good_flags = good_flags)
    out[[col]][!good] <- NA
  }

  core_present <- intersect(core_columns, names(out))
  if (length(core_present) > 0) {
    keep <- rep(TRUE, nrow(out))
    for (col in core_present) {
      qc_col <- qc_column_for(col, names(out))
      if (!is.null(qc_col)) {
        keep <- keep & is_qc_good(out[[qc_col]], good_flags = good_flags)
      }
      keep <- keep & !is.na(out[[col]])
    }
    out <- out[keep, , drop = FALSE]
  }

  if (drop_qc_columns) {
    out <- remove_qc_columns(out)
  }

  out
}

remove_qc_columns <- function(df) {
  qc_cols <- grep("_qc", names(df), value = TRUE)
  if (length(qc_cols) == 0) {
    return(df)
  }
  df[, setdiff(names(df), qc_cols), drop = FALSE]
}

count_qc_pressure_rows <- function(df) {
  if (!"sea_water_pressure" %in% names(df)) {
    return(0L)
  }
  sum(!is.na(df$sea_water_pressure))
}
