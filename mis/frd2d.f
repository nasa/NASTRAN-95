      SUBROUTINE FRD2D (IN,IO,IP)
C
      INTEGER         SYSBUF,MA(7),MB(7)
      COMMON /SYSTEM/ SYSBUF
      COMMON /UNPAKX/ IOUT,INN,MNN,INCR1
      COMMON /ZZZZZZ/ Z(1)
C
C     ADD IN TO END OF IO
C
      INCR1 = 1
      MA(1) = IN
      MB(1) = IO
      CALL RDTRL (MA)
      IOUT  = MA(5)
      NC    = KORSZ(Z)
      IB1   = NC  - SYSBUF
      IB2   = IB1 - SYSBUF
      CALL GOPEN (IN,Z(IB1),0)
      IF (IP .NE. 0) GO TO 10
      CALL GOPEN (IO,Z(IB2),1)
      CALL MAKMCB (MB,IO,MA(3),2,IOUT)
      GO TO 20
   10 CALL GOPEN (IO,Z(IB2),3)
      CALL RDTRL (MB)
   20 N = MA(2)
      CALL CYCT2B (IN,IO,N,Z,MB)
      CALL CLOSE  (IN,1)
      CALL CLOSE  (IO,3)
      CALL WRTTRL (MB)
      CALL DMPFIL (-IN,Z,NC)
      RETURN
      END
