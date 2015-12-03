      SUBROUTINE DPLTST
C
      IMPLICIT INTEGER (A-Z)
      INTEGER         ERRTTL(32)
      COMMON /BLANK / NGP,NSETS,SKP1(8),PCDB,EQEXIN,ECT,SKP2(7),
     1                MERR,PARM,GPSET,ELSET,SKP3(6),MSET,PECT
      COMMON /SYSTEM/ BUFSIZ
      COMMON /ZZZZZZ/ X(1)
      DATA    OUTREW, REW / 1,1 /
      DATA    ERRTTL/ 8*2H  ,4HERRO,4HR ME,4HSSAG,4HES F,4HROM ,
     1                4HTHE ,4HPLOT,4H SET,4H DEF,4HINIT,4HION ,
     2                4HMODU,4HLE (,4HPLTS,4HET) ,9*1H  /
C
      NSETS  = 0
      PCDB   = 101
      EQEXIN = 102
      ECT    = 103
      EPT    = 104
      MERR   = 201
      PARM   = 202
      GPSET  = 203
      ELSET  = 204
      MSET   = 301
      PECT   = 302
      CALL TOTAPE (1,X(1))
C
      X(1) = EQEXIN
      CALL RDTRL (X)
      I2   = 2
      I3   = 3
      NGP  = X(I2) - X(I3)
      I1   = KORSZ(X) - BUFSIZ + 1
      CALL GOPEN (MERR,X(I1),OUTREW)
      CALL WRITE (MERR,-4,1,0)
      CALL WRITE (MERR,ERRTTL,32,0)
      CALL SETINP
      IF (NSETS .NE. 0) GO TO 150
      NSETS = -1
      GO TO 200
  150 I1 = NSETS + 1
      I2 = I1 + NGP
      I3 = KORSZ(X) - 4*BUFSIZ + 1
      CALL COMECT (X(I2),I3-I2)
      CALL CNSTRC (X(I1),X(I2),X(I3),I3-I2)
C
  200 CALL CLSTAB (MERR,REW)
      RETURN
      END
