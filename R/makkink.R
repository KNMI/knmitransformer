makkink <- function(Tg,Q) {
  # Tg = mean daily temperature [oC] Q = global radiation [J/m2]
  rho  	 <- 1000		                          # water mass density   			[kg/m3]
  vps    <- 6.107 * 10^((7.5*Tg)/(237.3+Tg))  # saturated vapor pressure [hPa]
  delta  <- ((7.5*237.3)/(237.3+Tg)^2) * log(10) * vps	# vps gradient   [hPa/K]
  gamma  <- PsychrometricConstant(Tg) #0.646 + 0.0006*Tg					        # psychrometric constant    [hPa/K]
  lambda <- WaterVaporizationEnthalpy(Tg) #1000 * (2501-2.38*Tg)					    # Enthalpy of vaporization  [J/kg]

  evmk   <- (1000*Q) * ( (1000*0.65*delta) /
                    ((delta+gamma)*rho*lambda) )	# [mm/day]
  return(evmk)
}

makkink2 <- function(Tg, Q) {
  Q      <- Q * 1000
  a      <- 6.1078 # mbar
  b      <- 17.294
  smallC <- 237.73
  gamma  <- PsychrometricConstant(Tg)
  lambda <- WaterVaporizationEnthalpy(Tg)

  s <- a * b * smallC / (smallC + Tg)^2
  s <- s * exp(b * Tg / (smallC + Tg))

  DeBruinC <- 0.65

  DeBruinC * s / (s + gamma) * Q / lambda
}

PsychrometricConstant <- function(tg) {
  # normally depends on much more than the temperature
  # not clear where this simple linear fit comes from
  0.646 + 0.0006 * tg
}

WaterVaporizationEnthalpy <- function(tg) {
  # This linear fit is not checked
  # and is definitely not valid for higher temperatures
  1000 * (2501 - 2.38 * tg)
}
