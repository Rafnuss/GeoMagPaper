get_cols <- function(n, palette = "romaO", rotate = TRUE) {
  if (length(n) > 1) {
    name <- n
    n <- length(n)
  } else {
    name <- seq_len(n)
  }

  x <- scico::scico(n, palette = palette)
  # rotate
  if (rotate) {
    i <- floor(n / 2)
    x <- c(x[(i + 1):n], x[1:i])
  }
  # Name with stap
  setNames(x, name)
}
