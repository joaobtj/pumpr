# Ajustar curva característca da bomba: Hm ~ Q

#' @examples
#' q <- c(4.5, 4.3, 4.2, 4.1, 3.9, 3.7, 3.6, 3.4, 3.2, 3.0, 2.8, 2.5, 2.3, 2.0, 1.6, 1.2)
#' hm <- 2:17
#' pump_hm(hm,q)

pump_hm <- function(hm, q) {
  c <- lm(hm ~ poly(q,2, raw=TRUE))

  return(c)
}


# library(ggplot2)
# ggplot(NULL, aes(x=q, y=hm))+
#   geom_point()+
#   geom_smooth(method = "lm", formula=y ~ x + I(x^2), se=FALSE)

