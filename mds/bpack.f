      SUBROUTINE BPACK (IG,I,J,L)        
C        
      IMPLICIT INTEGER (A-Z)        
C        
CDC   NEXT 2 LINES FOR CDC AND UNIVAC ONLY        
C     EXTERNAL         ORF,      LSHIFT        
C     INTEGER          IG(1)        
C        
C     NEXT LINE FOR IBM, VAX, AND MACHINES THAT HAVE INTEGER*2        
      INTEGER*2        IG(1)        
C        
      COMMON /BANDB /  NBIT,     DUM3B(3), IPASS,    NW,       DUM1B,   
     1                 NBPW        
      COMMON /BANDS /  DUM4S(4), II1,      DUM5S(5), MASK        
C        
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE        
C        
C     PACK INTERNAL GRID NO. INTO IG TABLE.  SEE BUNPK FOR UNPACKING    
C     TABLE IG IS PACKED COLUMN-WISE.        
C     USE APPROP. PORTION OF THIS ROUTINE FOR DIFFERENT TYPE OF MACHINE.
C     IPASS=COUNTER ON NUMBER OF CALLS TO PACK/UNPACK        
C        
C     NOTE - THIS ROUTINE DOES NOT CHECK NOR ZERO OUT THE PACKING SLOT  
C            BEFORE PACKING.        
C            L IS ASSUMED TO BE A POSITIVE INTEGER, NBIT BITS OR LESS   
C        
      IPASS=IPASS+1        
      LOC  =J-1        
C        
C     ********************************************        
C     UNIVAC AND CDC MACHINES        
C     (IG SHOULD BE IN INTEGER*4 HERE)        
C     ********************************************        
C        
C     N1 =II1*(LOC/NW)+I        
C     N2 =MOD(LOC,NW)*NBIT+NBIT        
C     LOC=ORF(IG(N1),LSHIFT(L,NBPW-N2))        
C     IG(N1)=LOC        
C        
C     RETURN        
C        
C     ********************************************        
C     IBM AND VAX MACHINES        
C     (IG IS SET TO INTEGER*2 IN BPACK AND BUNPK, ELSEWHERE INTEGER*4)  
C     INTEGER*2     IG(1)        
C     ********************************************        
C        
      N1=II1*LOC+I        
      IG(N1)=L        
      RETURN        
      END        
