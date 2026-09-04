# Distinct pastel backgrounds for Sessions table rows (black text stays readable).
# Hues are spaced so nearby Prolific IDs remain separable without looking washed out.
random_rgb <- function(n) {
  if (is.null(n) || length(n) == 0 || is.na(n) || n <= 0) {
    return(character())
  }
  n <- as.integer(n)

  # Golden-angle steps keep colors separable even for small n.
  hue0 <- sample.int(360, 1) - 1
  hues <- (hue0 + (seq_len(n) - 1) * 137.508) %% 360

  # Very light pastels — barely tinted, still hue-spaced so groups differ.
  chroma <- rep_len(c(7, 9, 6, 8), n)
  lightness <- rep_len(c(97, 96.5, 97.5, 96.8), n)

  grDevices::hcl(h = hues, c = chroma, l = lightness, fixup = TRUE)
}
