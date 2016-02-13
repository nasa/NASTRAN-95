      INTEGER FUNCTION BUNPK (IG,I,J)        
C        
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE        
C        
C     UNPACK INTEGER GRID NO. FROM IG TABLE.   SEE BPACK FOR PACKING    
C     USE APPROP. PORTION OF THIS ROUTINE FOR DIFFERENT TYPE OF MACHINE.
C        
      INTEGER*2        IG(1)        
C        
      COMMON /BANDB /  NBIT,     DUM3B(3), IPASS,    NW,       DUM1B,   
     1                 NBPW        
      COMMON /BANDS /  DUM4S(4), II1,      DUM5S(5), MASK        
C        
      IPASS=IPASS+1        
      LOC  =J-1        
C        
C     ********************************************        
C     UNIVAC AND CDC MACHINES        
C     ********************************************        
C     INTEGER          RSHIFT,   ANDF        
C        
C     N1 =II1*(LOC/NW)+I        
C     N2 =MOD(LOC,NW)*NBIT+NBIT        
C     LOC=RSHIFT(IG(N1),NBPW-N2)        
C     BUNPK=ANDF(LOC,MASK)        
C     RETURN        
C        
C     ********************************************        
C     IBM AND VAX MACHINES        
C     (IG IS SET TO INTEGER*2 IN BPACK AND BUNPK, ELSEWHERE INTEGER*4)  
C     INTEGER*2        IG(1)        
C     ********************************************        
C        
      N1=II1*LOC+I        
      BUNPK=IG(N1)        
      RETURN        
      END        
