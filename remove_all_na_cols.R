remove_all_na_col <- function(df){df[, !apply(is.na(df), 2, all)]}
