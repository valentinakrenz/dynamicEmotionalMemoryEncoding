# Apply rounding rounding specifically for the p-value column #########
#round_p <- function(table_df, p_col = ncol(table_df)) {
#  table_df[, p_col] <- sapply(table_df[, p_col], function(x) {
#    if (x >= 0.0005) {
#      result <- round(x, 3)
#      #} else if (x >= 0.00005) {
#      #  result <- round(x, 4)
#    } else if (x < 0.0005) {
#      result <- "< 0.001"
#    } else {
#      result <- x
#    }
#    result
#  })
#  return(table_df)
#}

round_p <- function(table_df, p_col) {
  # Vector to store the new p values
  new_p_values <- numeric(length(table_df[[p_col]]))
  
  # Loop over each p value
  for (i in seq_along(table_df[[p_col]])) {
    x <- table_df[[p_col]][i]
    # Perform the check on a single value of x
    if (is.na(x)) {
      new_p_values[i] <- NA
    } else if (x < 0.0005) {
      new_p_values[i] <- "< 0.001"
    } else {
      new_p_values[i] <- round(x, 3)
    }
  }
  
  # Replace the original p column with the new values
  table_df[[p_col]] <- new_p_values
  return(table_df)
}

# round 3 spaces####
round_three <- function(table_df=table_df, start_col = 2, end_col = ncol(table_df)-1){
  for (j in start_col:end_col) { # Exclude the variable, d, and p-value columns
    table_df[, j] <- sapply(table_df[, j], function(x) {
      if (!is.nan(x)) { # Check if the value is not NaN
        abs_x <- abs(x)
        if (abs_x >= 0.0005) {
          result <- round(x, 3)
        } else if (abs_x >= 0.00005) {
          result <- round(x, 4)
        } else if (abs_x == 0) {
          result <- "0.000"
        } else if (abs_x < 0.00005) {
          result <- "< 0.001"
        }
      } else { # If the value is NaN, retain it as is
        result <- x
      }
      result # return the result for each element inside the sapply function
    })
  }
  return(table_df) # return the entire data frame outside the loop
}
