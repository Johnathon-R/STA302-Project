impute_allocation <- function(data, proportion_cols) {
  
  # Count missing proportions in each row
  missing_count <- rowSums(is.na(data[proportion_cols]))
  
  # Calculate the sum of reported proportions
  reported_sum <- rowSums(data[proportion_cols], na.rm = TRUE)
  
  # Identify rows where reported proportions sum to 1
  sum_is_one <- abs(reported_sum - 1) <= 1e-3
  
  # If reported proportions sum to 1, set remaining missing values to 0
  complete_missing_rows <- missing_count > 0 & sum_is_one
  
  for (col in proportion_cols) {
    target_rows <- complete_missing_rows & is.na(data[[col]])
    data[target_rows, col] <- 0
  }
  
  # If exactly one proportion is missing, calculate the remainder
  single_missing_rows <- missing_count == 1
  
  for (col in proportion_cols) {
    other_cols <- setdiff(proportion_cols, col)
    
    target_rows <- single_missing_rows &
      is.na(data[[col]]) &
      !sum_is_one
    
    if (any(target_rows)) {
      data[target_rows, col] <- 1 - rowSums(
        data[target_rows, other_cols, drop = FALSE],
        na.rm = TRUE
      )
    }
  }
  
  return(data)
}