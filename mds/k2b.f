      SUBROUTINE K2B (K,B,N)        
C        
C     MOVE ONLY THE APPROPRIATE PORTION OF THIS ROUTINE TO THE MDS GROUP
C        
C     VAX, IBM, UNIX AND UNIVAC VERSION        
C     =================================        
C        
C     TO CONVERT CHARACTER STRING TO BCD WORDS, ONE CHARACTER PER WORD  
C     AND BLANK FILLED. SAME RESULT AS        
C                READ (Kn,10) B        
C             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n        
C        
C     (NOTE - THE INTERNAL FILE READ AND WRITE ARE SLOW IN MOST MACHINES
C             AND IS EXTREMELY SLOW IN CDC)        
C        
      INTEGER     B(1),A        
      CHARACTER*1 K(1),C        
      CHARACTER*4 C4,D4        
C     CHARACTER*n C4,D4        
C         Where n = 4 for IBM, VAX and UNIVAC, 8 for 64-BIT UNIX MACHINE
      EQUIVALENCE (A,C,C4)        
      DATA   D4 / '    ' /        
C        
      C4 = D4        
      I  = 1
 10   C  = K(I)        
      B(I) = A        
      I  = I + 1
      IF (I .LE. N) GO TO 10
      RETURN        
C        
      ENTRY B2K (B,K,N)        
C     =================        
C        
C     TO MERGE FROM ONE-CHARACTER BCD WORDS TO A CHARACTER STRING       
C     SAME RESULT AS        
C                WRITE (Kn,10) B        
C             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n        
C        
      I = 1
 20   A = B(I)        
      K(I) = C        
      I = I + 1
      IF (I .LE. N) GO TO 20
      RETURN        
C        
C        
C     SUBROUTINE K2B (K,B,N)        
C        
C     CDC VERSION        
C     ===========        
C        
C     TO CONVERT CHARACTER STRING TO BCD WORDS, ONE CHARACTER PER WORD  
C     AND BLANK FILLED. SAME RESULT AS        
C                READ (Kn,10) B        
C             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n        
C        
C     (NOTE - THE INTERNAL FILE READ AND WRITE ARE SLOW IN MOST MACHINES
C             AND IS EXTREMELY SLOW IN CDC)        
C        
C     INTEGER K(1),B(1)        
C     DATA    NBPC,NCPW,NCPWP1 / 6, 10, 11  /        
C     DATA    MASK /O"77000000000000000000" /        
C        
C     IE = 1 + N/NCPW        
C     KK = 0        
C     DO 40 I = 1,IE        
C     KI = K(I)        
C     DO 30 J = 1,NCPW        
C     KK = KK + 1        
C     IF (KK .GT. N) GO TO 50        
C     B(KK) = AND(KI,MASK)        
C     KI = SHIFT(KI,NBPC)        
C30   CONTINUE        
C40   CONTINUE        
C50   RETURN        
C        
C        
C     ENTRY B2K (B,K,N)        
C     =================        
C        
C     TO MERGE FROM ONE-CHARACTER BCD WORDS TO A CHARACTER STRING       
C     SAME RESULT AS        
C                WRITE (Kn,10) B        
C             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n        
C        
C     IE = 1 + N/NCPW        
C     KK = 0        
C     DO 70 I = 1,IE        
C     KK = KK + 1        
C     KI = AND(B(KK),MASK)        
C     DO 60 J = 2,NCPW        
C     KI = SHIFT(KI,NBPC)        
C     KK = KK + 1        
C     IF (KK .GT. N) GO TO 80        
C     KI = OR(KI,AND(B(KK),MASK))        
C60   CONTINUE        
C70   K(I) = SHIFT(KI,NBPC)        
C     GO TO 90        
C80   J = NCPWP1 - MOD(KK,NCPW)        
C     IF (J .GT. NCPW) J = 1        
C     K(IE) = SHIFT(KI,J*NBPC)        
C90   RETURN        
C        
      END        
