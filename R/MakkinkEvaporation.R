makkink <- function(Tg, Q) {
  # http://projects.knmi.nl/hawa/pdf/Handboek_H10.pdf onder 5.Herleiding parameters
  # Tg = mean daily temperature [oC] Q = global radiation [kJ/m2]
  rho    <- 1000                              # water mass density  [kg/m3]
  vps    <- 6.107 * 10^((7.5*Tg)/(237.3+Tg))  # saturated vapor pressure [hPa] #nolint
  delta  <- ((7.5*237.3)/(237.3+Tg)^2) * log(10) * vps # vps gradient [hPa/K] #nolint
  gamma  <- PsychrometricConstant(Tg) #0.646 + 0.0006*Tg          # psychrometric constant [hPa/K]
  lambda <- WaterVaporizationEnthalpy(Tg) #1000 * (2501-2.38*Tg)  # Enthalpy of vaporization [J/kg]
  evmk   <- 1000*Q * 1000*0.65*delta / ((delta+gamma)*rho*lambda) # [mm/day] #nolint
  return(evmk)
}

# makkink2 <- function(Tg, Q) {
#   # obtained from https://nl.wikipedia.org/wiki/Referentie-gewasverdamping
#   Q      <- Q * 1e3
#
#   gamma    <- PsychrometricConstant(Tg)
#   lambda   <- WaterVaporizationEnthalpy(Tg)
#   s        <- SaturatedVaporPressureGradient(Tg)
#   DeBruinC <- 6.5e-1
#
#   DeBruinC * s / (s + gamma) * Q / lambda
# }
#
# SaturatedVaporPressureGradient <- function(tg) {
#   # obtained from https://nl.wikipedia.org/wiki/Referentie-gewasverdamping
#   constA <- 6.1078 # mbar
#   constB <- 17.294
#   constC <- 237.73
#   s      <- constA * constB * constC / (constC + tg)^2
#   s * exp(constB * tg / (constC + tg))
# }

PsychrometricConstant <- function(tg) {
  # normally depends on much more than the temperature
  # not clear where this simple linear fit comes from
  6.46e-1 + 6e-4 * tg
}

WaterVaporizationEnthalpy <- function(tg) {
  # This linear fit is not checked
  # and is definitely not valid for higher temperatures
  2.501e6 - 2.38e3 * tg
}
