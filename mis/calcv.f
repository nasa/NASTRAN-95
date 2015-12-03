      SUBROUTINE CALCV (PVACT,SET1,SUB1,SUB2,CORE)
C
      EXTERNAL        ANDF
      INTEGER         A1,PVECT,SET1,SUB1,SUB2,USET,SYSBUF,PVACT,ANDF,
     1                CORE(1)
C
      COMMON /SYSTEM/ SYSBUF
      COMMON /TWO   / TWO1(32)
      COMMON /PATX  / LC,N,NO,N3,USET,PVECT(7)
      COMMON /ZBLPKX/ B1(4),N1
C
C
      N  = 0
      N3 = 0
      NO = 0
      N1 = 0
      CALL MAKMCB (PVECT,PVACT,0,2,1)
      LCORE = LC - SYSBUF
      CALL GOPEN (USET,CORE(LCORE+1),0)
      LCORE = LCORE - SYSBUF
      CALL GOPEN (PVACT,CORE(LCORE+1),1)
      CALL BLDPK (1,1,PVACT,0,0)
   20 CALL READ (*90,*90,USET,A1,1,0,FLAG)
      IF (ANDF(TWO1(SET1),A1) .EQ. 0) GO TO 20
      N1 = N1 + 1
      IF (ANDF(TWO1(SUB1),A1) .EQ. 0) GO TO 50
      N  = N  + 1
      GO TO 20
   50 IF (ANDF(TWO1(SUB2),A1) .EQ. 0) GO TO 60
      NO = NO + 1
      B1(1) = 1.0
      GO TO 70
   60 B1(1) = 2.0
      N3 = N3 + 1
   70 CONTINUE
      CALL ZBLPKI
      GO TO 20
   90 CONTINUE
      CALL BLDPKN (PVACT,0,PVECT)
      PVECT(3) = N1
      CALL WRTTRL (PVECT)
      CALL CLOSE  (USET,1)
      CALL CLOSE  (PVACT,1)
      RETURN
      END
