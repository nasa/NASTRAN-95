      SUBROUTINE BUNPAK (IG,I,NJ,JG)        
C        
C     THIS ROUTINE WORKS SIMILARLY AS BUNPK EXCEPT IT UNPACKS A WHOLE   
C     (I-TH) ROW OF GRID NUMBERS (1 THRU NJ) FROM IG TABLE, AND SAVES   
C     THE UNPACKED DATA IN JG ARRAY.        
C     (BUNPK UNPACKS ONLY AN ELEMENT OF GRID NUMBER IN IG TABLE)        
C        
C     THIS ROUTINE GREATLY INCREASES BANDIT INTERNAL EFFICIENCY        
C     WRITTEN BY G.CHAN/UNISYS,    MAY 1988        
C        
      IMPLICIT INTEGER (A-Z)        
C        
CDC   NEXT 2 LINES FOR CDC AND UNIVAC ONLY        
C     EXTERNAL         ANDF,     RSHIFT        
C     INTEGER          ANDF,     RSHIFT  ,IG(1)        
C        
C     NEXT LINE FOR IBM, VAX, AND MACHINES THAT HAVE INTEGER*2        
      INTEGER*2        IG(1)        
C        
      INTEGER          JG(1),    NAM(2)        
      COMMON /SYSTEM/  IBUF,     NOUT        
      COMMON /BANDB /  NBIT,     DUM3B(3), IPASS,    NW        
      COMMON /BANDS /  DUM4S(4), II1,      MAXDEG,   DUM4(4),  MASK     
      DATA    NAM   /  4HUNPA  , 4HK       /        
C        
      IF (NJ .LE. MAXDEG) GO TO 20        
      WRITE  (NOUT,10) NJ,MAXDEG        
 10   FORMAT ('0 *** BUNPAK .GT. MAXDEG',2I7)        
      CALL ERRTRC (NAM)        
C        
 20   IPASS = IPASS+NJ        
      N1 = I        
C        
C     ********************************************        
C     UNIVAC AND CDC MACHINES        
C     ********************************************        
C        
C     DO 40 N=1,NJ,NW        
C     N2 = IG(N1)        
C     N3 = N+NW-1        
C     DO 30 M=1,NW        
C     JG(N3) = ANDF(N2,MASK)        
C     IF (M .EQ. NW) GO TO 40        
C     N2 = RSHIFT(N2,NBIT)        
C  30 N3 = N3-1        
C  40 N1 = N1+II1        
C     RETURN        
C        
C     ********************************************        
C     IBM AND VAX MACHINES        
C     ********************************************        
C        
      DO 50 N=1,NJ        
      JG(N) = IG(N1)        
   50 N1 = N1+II1        
      RETURN        
      END        
