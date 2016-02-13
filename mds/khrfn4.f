      INTEGER FUNCTION KHRFN4 (WORD)        
C        
C     REVERSE BYTES FOR SORTING (USED MAINLY BY THE VAX MACHINE)        
C        
      INTEGER      WORD(1),  W1,    W2        
      CHARACTER*1  C1(4),    C2(4)        
      EQUIVALENCE (C1(1),W1),(C2(1),W2)        
C        
      W1=WORD(1)        
      C2(1)=C1(4)        
      C2(2)=C1(3)        
      C2(3)=C1(2)        
      C2(4)=C1(1)        
      KHRFN4=W2        
      RETURN        
C        
C     CDC VERSION        
C     ===========        
C        
C     CHARACTER*1 WORD(10),C2(10)        
C        
C     C2(1)=WORD(4)        
C     C2(2)=WORD(3)        
C     C2(3)=WORD(2)        
C     C2(4)=WORD(1)        
C     DO 10 J=5,10        
C 10  C2(J)=WORD(J)        
C     KHRFN4=ISWAP(C2)        
C        
      END        
