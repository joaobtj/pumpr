# Ajustar curva característca da bomba: Hm ~ Q

curve_hm <- function(hm, q) {
  c <- lm(hm ~ q + I(q^2))

  return(list(
    model = c,
    coefficients = broom::tidy(c)
  ))
}
