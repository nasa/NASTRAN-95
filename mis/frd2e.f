      SUBROUTINE FRD2E (IN,IO,NLOAD,NFREQ)
C
      INTEGER         MA(7),MB(7)
      COMMON /SYSTEM/ ISYS
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /ZZZZZZ/ Z(1)
C
C     MAKE UHDF FROM IN
C
      INCR1 = 1
      MA(1) = IN
      MB(1) = IO
      IB1   = KORSZ(Z) - ISYS
      IB2   = IB1 - ISYS
      CALL RDTRL (MA)
      IOUT  = MA(5)
      CALL GOPEN (IN,Z(IB1),0)
      CALL GOPEN (IO,Z(IB2),1)
      CALL MAKMCB (MB,IO,MA(3),MA(4),IOUT)
      DO 30 J = 1,NLOAD
      CALL SKPREC (IN,J-1)
      DO 10 I = 1,NFREQ
      CALL CYCT2B (IN,IO,1,Z,MB)
      IF (I .NE. NFREQ) CALL SKPREC (IN,NLOAD-1)
   10 CONTINUE
      CALL REWIND (IN)
      CALL SKPREC (IN,1)
   30 CONTINUE
      CALL CLOSE (IN,1)
      CALL CLOSE (IO,1)
      CALL WRTTRL (MB)
      RETURN
      END
