# Altura manométrica do sistema

#' @examples
#' system_hm(q = 1:5, ds = 0.018, hs = 1, ls = 8, dr = 0.013, hr = 26, lr = 10)

system_hm <- function(q, ds, hs, ls, dr, hr, lr, rc = 1e-5, unit = "m3/h") {
  if (unit == "m3/h") unit_div <- 3600

  hfs <- purrr::map_dbl(q, ~ head_loss(d = ds, q = .x / unit_div, l = ls, rc = rc)$hf)
  hfr <- purrr::map_dbl(q, ~ head_loss(d = dr, q = .x / unit_div, l = lr, rc = rc)$hf)
  hm <- hs + hr + hfs + hfr

  c <- lm(hm ~ poly(q, 2, raw = TRUE))

  return(c)
}
