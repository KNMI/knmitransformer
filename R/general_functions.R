# bndf
# makkink
# daynumber
# Angot
# surface.distance
# daynumber2datestring
# datestring2day
# max.pos.cumsum: maximum van cumulatieve som, die niet negatief kan zijn

bndf <- function(x,mx,mn) {x[x>mx]=mx; x[x<mn]=mn; return(x)}
  vp <- function(T) 0.6108*exp((17.27*T)/(T+237.3)) # FAO

makkink <- function(Tg,Q) {
  # Tg = daggemiddelde temperatuur [oC] Q = globale straling [J/m2]
  rho  	 <- 1000		                          # mass density water  			[kg/m3]
  vps    <- 6.107 * 10^((7.5*Tg)/(237.3+Tg))  # verzadigingsdampspanning [hPa]
  delta  <- ((7.5*237.3)/(237.3+Tg)^2) * log(10) * vps	# vps gradient   [hPa/K]
  gamma  <- 0.646 + 0.0006*Tg					        # psychrometerconstante    [hPa/K]
  lambda <- 1000 * (2501-2.38*Tg)					    # verdampingswarmte water  [J/kg]

  evmk   <- Q * ( (1000*0.65*delta) /
                  ((delta+gamma)*rho*lambda) )	# [mm/day]
  return(evmk)
}

daynumber <- function(dt) {
  dpm <- c(0,31,59,90,120,151,181,212,243,273,304,334)
  id <- floor(dt%%100)
  im <- floor((dt%%10000)/100)
  iy <- floor(dt/10000)%%4
  dnr <- dpm[im] + id + (iy==0 & im >2)
  return(dnr)
}

Angot <- function(dt,lat) {
  Gsc <- 0.0820 # solar constant [MJ/m2/min]
  phi <- pi*lat/180
  J <- daynumber(dt)
  dr <- 1 + 0.033*cos(2*pi*J/365)
  delta <- 0.409*sin(2*pi*J/365-1.39)
  omega <- acos(-tan(phi)*tan(delta))
  Ra <- (24*60/pi) * Gsc * dr*(omega *sin(phi)*sin(delta) + sin(omega)*cos(phi)*cos(delta))
  return(Ra)
}


surface.distance <- function(lon1,lat1,lon2,lat2) {
  # determines distance between two points and earth surface by caculating angle between points
    R  = 6367 # [km] => in mijl 3956 mi
    lon1=pi*lon1/180
    lon2=pi*lon2/180
    lat1=pi*lat1/180
    lat2=pi*lat2/180
    dlon=abs(lon2-lon1)
    dlat=abs(lat2-lat1)
    a=(sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
    c=2*asin(min(1,sqrt(a)))
    d=R*c
    return(d)
}

day2datestring <- function(ids,calendar,dd1) {
  dates <- ids
  for(j in 1:length(ids)) {
    id <- ids[j] + 1
  #id <- id + 1 # 1 jan = 1 ipv 1 jan = 0
  if(calendar == "noleap" | calendar == "365_day" | calendar == "360_day") {
    if(calendar == "360_day") {
      dpm <- c(30,30,30,30,30,30,30,30,30,30,30,30)
      dpy <- 360
    } else {
      dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      dpy <- 365
    }
    iy <- id %/% dpy + dd1  		# year
    id <- id - (iy-dd1) * dpy
    im <- 1
    while(id > dpm[im]) {
      id <- id - dpm[im]        # month
      im <- im + 1              # day
    }
  } else {
    dp4y <- 4 * 365.25
    dpy <- c(365,365,365,365)
    dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    for(i in 1:4) {
      if((dd1+i-1)%%4 == 0) {
        dpy[i] <- 366
      }
    }
    i4y <- (id-1) %/% dp4y          # nth 4-year cluster (0,1,...)
    id  <- id - i4y*dp4y
    iy=1
    while(id > dpy[iy]) {
      id <- id - dpy[iy]
      iy <- iy + 1
    }
    iy <- dd1 + 4*i4y + (iy-1)   # year
    if(iy%%4 == 0) {
      dpm[2] <- 29
    }
    im <- 1
    while(id > dpm[im]) {
      id <- id - dpm[im]        # month
      im <- im + 1              # day
    }
  } 
  dates[j] <- iy*10000 + im * 100 + id
  }
  return(dates)
}

datestring2day <- function(dt,calendar,dd1) {
  dpm <- c(0,31,59,90,120,151,181,212,243,273,304,334)
  dpy <- 365
  if(calendar == "360_day") {
    dpm <- c(0,30,60,90,120,150,180,210,240,270,300,330)
    dpy <- 360
  }
  id <- floor(dt%%100)
  im <- floor((dt%%10000)/100)
  iy <- floor(dt/10000)-dd1
  dnr <- iy*dpy + dpm[im] + id - 1
  if(calendar != "noleap" & calendar != "365_day" | calendar == "360_day") {
    dnr <- dnr + (floor(dt/10000)%%4==0 & im >2)
    if(iy > 0) {
      for(i in 0:(iy-1)) {
        dnr <- dnr + ((i+dd1)%%4 == 0)
      }
    }
  }
  return(dnr)
}

max.pos.cumsum <- function(x) {
  n              <- length(x)
  i              <- 1:n
  y              <- cumsum(x)
  while(min(y)<0) {
    neg            <- which(y<0)[1]
    pos            <- which(i>neg & x>0)[1]
    if(is.na(pos)) {
      y[neg:n]       <- 0
    } else {
      y[neg:(pos-1)] <- 0
      y[pos:n]       <- cumsum(x[pos:n])
    }
  }
  return(max(y))
}

read.xlstable <- function(ifile,colnames=F) {
  library(RODBC)
  xfile <- odbcConnectExcel(ifile)
  sheet <- as.character(sqlTables(xfile)["TABLE_NAME"])
  tabel <- sqlFetch(xfile,sheet,colnames=F)
  odbcClose(xfile)
  return(tabel)
}

  
recoderFunc <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}
