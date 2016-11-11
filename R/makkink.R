makkink <- function(Tg,Q) {
  # Tg = mean daily temperature [oC] Q = global radiation [J/m2]
  rho  	 <- 1000		                          # water mass density   			[kg/m3]
  vps    <- 6.107 * 10^((7.5*Tg)/(237.3+Tg))  # saturated vapor pressure [hPa]
  delta  <- ((7.5*237.3)/(237.3+Tg)^2) * log(10) * vps	# vps gradient   [hPa/K]
  gamma  <- 0.646 + 0.0006*Tg					        # psychrometric constant    [hPa/K]
  lambda <- 1000 * (2501-2.38*Tg)					    # Enthalpy of vaporization  [J/kg]

  evmk   <- Q * ( (1000*0.65*delta) /
                    ((delta+gamma)*rho*lambda) )	# [mm/day]
  return(evmk)
}