      SUBROUTINE FNAME (FILE,NAME)
C*******
C     GIVEN A FILE NO., FNAME WILL RETURN THE BCD DESCRIPTOR
C*******
      INTEGER FIAT,FILE,FIST,NAME(2)
      COMMON /XFIST / FIST(2)
      COMMON /XFIAT / FIAT(1)
      DATA    NBLANK/ 4H    /
      DATA    NON1  , NON2  / 4H (NO,4HNE) /
C*******
C     SEARCH THE FIST FOR THE FILE
C*******
      N = FIST(2)*2 + 2
      DO 10 J=3,N,2
      IF (FILE .EQ. FIST(J)) GO TO 20
   10 CONTINUE
C*******
C     FILE DOES NOT EXIST, RETURN -(NONE)-
C*******
      NAME(1) = NON1
      NAME(2) = NON2
      RETURN
   20 K = FIST(J+1)
      IF (K) 21,21,30
   21 CONTINUE
C*******
C     RETURN BCD DESCRIPTOR
C*******
      NAME(1) = FILE
      NAME(2) = NBLANK
      RETURN
C
   30 IX = FIST(J+1) + 2
      NAME(1) = FIAT(IX  )
      NAME(2) = FIAT(IX+1)
      RETURN
      END
