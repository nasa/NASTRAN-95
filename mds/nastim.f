      SUBROUTINE NASTIM (IHR, IMN, ISC, CPUSEC)
      REAL ARRAY(2)
CDME  19 JAN 2016
CDME  D. Everhart
CDME  Changed to conform to GFORTRAN implementation of ETIME subroutine.
      REAL TIME
      CALL ETIME(ARRAY,TIME)
CDME  CALL ETIME(ARRAY)
      SECS   = ARRAY(2)
      IHR    = SECS / 3600.  
      IMN    = ( SECS - 3600.*IHR ) / 60.
      ISC    = SECS - ( 3600.*IHR ) - ( 60.*IMN )
      CPUSEC = SECS
      RETURN
      END
