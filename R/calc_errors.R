# Calculate the absolute difference between type I and II errors
calc_errors <- function(threshold, data, adjust = 5) {
  densityLow      <- density(data$IR[data$pressure %in% "low"],      adjust = adjust)
  densityImpaired <- density(data$IR[data$pressure %in% "impaired"], adjust = adjust)

  pdfLow      <- approxfun(x = densityLow$x,
                           y = densityLow$y)
  pdfImpaired <- approxfun(x = densityImpaired$x,
                           y = densityImpaired$y)

  typeI <- integrate(f = pdfLow, lower = threshold, upper = 1)$value /
    integrate(f = pdfLow, lower = 0, upper = 1)$value

  typeII <- integrate(f = pdfImpaired, lower = 0, upper = threshold)$value /
    integrate(f = pdfImpaired, lower = 0, upper = 1)$value

  abs(diff(c(typeI, typeII)))

}
