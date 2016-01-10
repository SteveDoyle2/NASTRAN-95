      SUBROUTINE NASTIM (IHR, IMN, ISC, CPUSEC)
      REAL ARRAY(2), TOTIME
      CALL ETIME(ARRAY, TOTIME)
      SECS   = ARRAY(2)
      IHR    = SECS / 3600.
      IMN    = ( SECS - 3600.*IHR ) / 60.
      ISC    = SECS - ( 3600.*IHR ) - ( 60.*IMN )
      CPUSEC = SECS
      RETURN
      END
