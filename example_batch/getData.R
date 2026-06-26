getData <- function(batch_value, n_rows = 10) {
  print(paste('getting data for', batch_value))
  # seed based on batch_value so the data is reproducible
  set.seed(sum(as.integer(charToRaw(batch_value))))
  
  data <- data.frame(
    id = 1:n_rows,
    value = runif(n_rows, min = 0, max = 100),
    category = sample(c("A", "B", "C"), n_rows, replace = TRUE),
    timestamp = seq(as.POSIXct("2024-01-01"), by = "day", length.out = n_rows)
  )
  
  return(data)
}