#' @examples
#' qb <- c(4.5, 4.3, 4.2, 4.1, 3.9, 3.7, 3.6, 3.4, 3.2, 3.0, 2.8, 2.5, 2.3, 2.0, 1.6, 1.2)
#' hb <- 2:17
#' pump <- pump_hm(hb, qb)
#' system <- system_hm(q = seq(1.5, 5.5, 1), ds = 0.018, hs = 1, ls = 8, dr = 0.013, hr = 06, lr = 10)
#' operating_point(pump, system)
operating_point <- function(pump, system) {
  int <- coef(pump) - coef(system)
  f <- function(x) int[1] + int[2] * x + int[3] * x^2

  z=NULL
    e <- simpleError("Não foi possível encontrar a interseção das curvas")
    tryCatch( z <- uniroot(f, c(0, 100))$root,
              error = function(e) print("Não foi possível encontrar a interseção das curvas")

 )

  #plot
  max.len <- max(length(predict(pump)), length(predict(system)))
  data.frame(
    pump = c(predict(pump), rep(NA, max.len - length(predict(pump)))),
    system = c(predict(system), rep(NA, max.len - length(predict(system))))
  )

  hb <- pump$model$hm
  qb <- pump$fitted.values

  qs <- system$model$`poly(q, 2, raw = TRUE)`[, 1]
  hs <- system$fitted.values

  library(ggplot2)
  p <- ggplot(NULL) +
    geom_line(aes(x = qb, y = hb)) +
    geom_line(aes(x = qs, y = hs))

return(z)
}

#
#
#
#  ## tentativa 1
#  qb <- c(4.5, 4.3, 4.2, 4.1, 3.9, 3.7, 3.6, 3.4, 3.2, 3.0, 2.8, 2.5, 2.3, 2.0, 1.6, 1.2)
#  hb <- 2:17
#
#  qs=seq(.1,2,.1)
#  hs <- purrr::map_dbl(qs, ~system_hm(q=.x, ds=0.018, hs=1, ls=8, dr=0.013, hr=1, lr=10))
#
#  ggplot(NULL)+
#    geom_line(aes(x=qb, y=hb))+
#    geom_line(aes(x=qs, y=hs))
#
#  (pump <-  lm(hb~poly(qb,2, raw = TRUE)))
#
#  system <- lm(hs~poly(qs,2, raw=TRUE))
#
#  predict(pump, data.frame(qb=1.75))
#
# (int <- -coef(pump)+coef(system))
#
#  f <- function(x) int[1]+int[2]*x+int[3]*x^2
#  f(c(0:10))
#
#  uniroot(f , c(0,100))$root
#
#
#
