      subroutine gemsea ( temp, dewpt, selv, staprs, seaprs )

c     subroutine gemsea.f - uses GEMPAK algorithms to compute sea level
c                           pressure

c     Required input:
c       temperature in degrees Fahrenheit
c       dewpoint in degrees Fahrenheit
c       station elevation in meters
c       station pressure in millibars

c     Returned parameters:
c       sea level pressure in millibars
c
c     Written by Ronald A. Murdock

c     Creation date:  10APR92
c
c     Log of program changes:

c     10APR92 - Converted gempak.f interactive program into
c               this subroutine.

c

      real pres
      real staprs
      real seaprs
      real g       !acceleration of gravity = 9.80616 m/sec/sec
      real selv    !station elevation in meters
      real r       !gas constant for dry air 287.04 J/deg kg
      real tvave   !average virtual temperature between station and
c                   sea level
c                   tvave = (tvrk + (tvrk - deltv)) / 2
      real temp    !temperature in Fahrenheit
      real tvrk    !virtual temperature
      real tmpk    !temperature at station in degrees Kelvin
      real mixr    !mixing ratio
      real deltv   !gamma * selv / 1000.
      real gamma   !lapse rate standard atmosphere 6.5 K/km
      real dewpt   !dew point in Fahrenheit
      real pmsl    !mean sea level pressure
      real kappa
      real vapr, e, thta, dwpc

      real celsus, cc2kel
      external celsus, cc2kel

      g = 9.80616
      r = 287.04


c     write(6,'('' Station elevation in meters is '',f5.0)' ) selv

      tmpk = celsus(temp)
      tmpk = cc2kel(tmpk)
c     write(6,'('' station temp in Kelvin is '',f8.3)' ) tmpk

c     write(6,'('' Dew point in Fahrenheit is '',f7.2)' ) dewpt
      dwpc = celsus(dewpt)
c     write(6,'('' Dew point in Celsius is '',f7.2)' ) dwpc


c     WRITE(6,
c    *'('' STATION PRESSURE IN MILLIBARS IS '',F8.3)' ) staprs
      
      pres = staprs  !pressure in millibars

      gamma = 6.5  !lapse rate standard atmosphere 6.5 K/km

      deltv = gamma * selv / 1000.

      vapr = 6.112 * exp ((17.67 * dwpc) / (dwpc + 243.5))
c     write(6,'('' vapor pressure = '',f7.2)' ) vapr
      e    = vapr * (1.001 + (pres - 100.) / 900. * .0034)
c     write(6,'('' e = '',f7.2)' ) e
      mixr = .62197 * ( e / ( pres - e)) * 1000.
c     write(6,'('' mixing ratio = '',f7.2)' ) mixr

c     tvrk = tmpk * (1. + (.001 * MIXR) / .62197)) / (1. + .001 * MIXR)
      tvrk = tmpk * ((1. + (.001 * MIXR) / .62197) / (1. + .001 * MIXR))
c     write(6,'('' virtual temperature = '',f7.2)' ) tvrk

      tvave = (tvrk + (tvrk - deltv)) / 2
c     write(6,'('' average virtual temperature = '',f7.2)' ) tvave

      kappa = 2. / 7.
      thta = tmpk * (1000. / pres)**kappa
      pres = 1000. * (tmpk / thta)**(1. / kappa)
c     write(6,'(/,'' Adjusted station pressure = '',f7.2)' ) pres



c     write out parameters immediately before equation

c     write(6,'(//,'' station pressure = '',F7.2)' ) pres
c     write(6,'('' gravity = '',F7.5)' ) g
c     write(6,'('' station elevation = '',F7.2)' ) selv
c     write(6,'('' gas constant = '',F7.2)' ) r
c     write(6,'('' average virtual temperature = '',F7.2)' ) tvave

      pmsl = pres * exp (( g * selv) / (r * tvave))

      seaprs = pmsl
c     write(6,
c    *'('' GEMPAK-calculated sea level pressure is '',F7.2,
c    *'' millibars'',//)' ) seaprs


9999  return
      end
