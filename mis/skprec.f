      SUBROUTINE SKPREC(IFILE,K)
C
      INTEGER NAME(2)
C
      DATA NAME /4HSKPR,2HEC /
C
C ----------------------------------------------------------------------
C
      IF( K ) 10,30,20
C
   10 M=IABS(K)
      DO 15 I=1,M
      CALL BCKREC(IFILE)
   15 CONTINUE
      GO TO 30
C
   20 DO 25 I=1,K
      CALL FWDREC(*40,IFILE)
   25 CONTINUE
C
   30 RETURN
C
   40 CALL MESAGE(-2,IFILE,NAME)
      GO TO 30
C
      END
