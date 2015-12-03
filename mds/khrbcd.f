      SUBROUTINE KHRBCD (KHR80,BCD4)        
C        
C     MOVE ONLY THE APPROPRIATE PORTION OF THIS ROUTINE TO THE MDS GROUP
C        
C     VAX, IBM, AND UNIVAC VERSION        
C     ============================        
C        
C     THESE GROUP OF ROUTINES ARE MAINLY USED BY XREAD, RCARD2, AND     
C     XRCARD IN LINK1        
C        
C     THESE GROUP OF ROUTINES CONVERT CHARACTER STRING (IN KHR100,      
C     KHR80, KHR2), TO BCD4 ARRAY, OF 4 BYTES EACH WORD.        
C     SAME OPERATION AS:        
C        
C           READ (KHRi,15) BCD4        
C        15 FORMAT (20A4),  or (25A4),  or (2A4)        
C        
C     (THE READ OPERATION IS I/O BOUND, AND IS SLOW IN MOST MACHINES)   
C        
C        
      INTEGER         B4(2) ,BCD4(2)        
      CHARACTER*100   KHR100,K100        
      CHARACTER*80    KHR80 ,K80        
      CHARACTER*72    KHR72 ,K72        
      CHARACTER*8     KHR8  ,K8        
      EQUIVALENCE     (K100,K80,K72,K8,B4(1))        
C        
C     ROUTINE KHRBCD (KAR80,BCD4)        
C     ===========================        
C     A80 ---> 20A4        
C        
      K80=KHR80        
      DO 10 I=1,20        
 10   BCD4(I)=B4(I)        
      RETURN        
C        
C        
      ENTRY KHRBC1 (KHR100,BCD4)        
C     ==========================        
C     A100 ---> 25A4        
C        
      K100=KHR100        
      DO 20 I=1,25        
 20   BCD4(I)=B4(I)        
      RETURN        
C        
C        
      ENTRY KHRBC2 (KHR8,BCD4)        
C     ========================        
C     A8 ---> 2A4        
C        
      K8=KHR8        
      BCD4(1)=B4(1)        
      BCD4(2)=B4(2)        
      RETURN        
C        
C        
C     THE FOLLOWING ROUTINES, BCDKHi, CONVERT FROM BCD ARRAY TO        
C     CHARACTER STRING.   SAME OPERATION AS:        
C        
C           WRITE (KHRi,25) BCD4        
C        25 FORMAT (20A4),  or (18A4), or (2A4)        
C        
C     WHERE KHRi IS KHR80, KHR72, OR KHR8 ACCORDINGLY        
C     (THE WRITE OPERATION IS I/O BOUND, AND IS SLOW IN MOST MACHINES)  
C        
C        
      ENTRY BCDKH8 (BCD4,KHR80)        
C     =========================        
C     20A4 ---> A80        
C        
      DO 30 I=1,20        
 30   B4(I)=BCD4(I)        
      KHR80=K80        
      RETURN        
C        
C        
      ENTRY BCDKH7 (BCD4,KHR72)        
C     =========================        
C     18A4 ---> A72        
C        
      DO 40 I=1,18        
 40   B4(I)=BCD4(I)        
      KHR72=K72        
      RETURN        
C        
C        
      ENTRY BCDKH2 (BCD4,KHR8)        
C     ========================        
C     2A4 ---> A8        
C        
      B4(1)=BCD4(1)        
      B4(2)=BCD4(2)        
      KHR8=K8        
      RETURN        
C     END        
C        
C        
C        
C     SUBROUTINE KHRBCD (KHR,BCD4)        
C        
C     CDC VERSION        
C     ===========        
C        
C     THIS GROUP OF ROUTINES ARE CALLED BY XREAD, RCARD2, AND XRCARD    
C        
C     THESE GROUP OF ROUTINES CONVERT CHARACTER STRINGS TO BCD ARRAY,   
C     4 BYTES EACH WORD, AND VISE VERSA.     SIMILARY TO -        
C        
C        METHOD 1:        
C        --------        
C           READ (KHR80,10) BCD4     and       WRITE (KHR72,20) BCD4    
C        10 FORMAT (20A4)                   20 FORMAT (18A4)        
C        
C        METHOD 2:        
C        --------        
C           I2=0                     and       I2=0        
C           DO 10 I=1,NWDS                     DO 20 I=1,NWDS        
C           I1=I2+1                            I1=I2+1        
C           I2=I2+4                            I2=I2+4        
C        10 BCD4(I)(1:4)=KHR(I1:I2)         20 KHR(I1:I2)=BCD4(I)(1:4)  
C        
C     HOWEVER THE INTERNAL-FILE READ AND WRITE (METHOD 1) AND THE       
C     CHARACTER MANIPULATION (METHOD 2) ARE EXTREMELY SLOW IN CDC       
C     (METHOD 1 IS ABOUT 18 TIMES SLOWER THAN SHIFT/AND/OR OPERATIONS   
C     THAT ACCOMPLISH THE SAME THING. METHOD 2 IS 2 TO 4 TIMES SLOWER)  
C        
C     THE CALLING ROUTINES ACTUALLY PASS THE KHR ARGUMENTS IN CHARACTER 
C     STRINGS (CHARACTER*100, CHARACTER*80, CHARACTER*2), WHEREAS, THEY 
C     ARE PICKED UP HERE IN THIS ROUTINE AS INTEGER-BCD ARRAYS, 10 BYTES
C     EACH WORDS. ONLY THE FIRST 4 BYTES ARE USED IN NASTRAN.        
C        
C     (THE FOLLOWING CODE ASSUMES NO BREAK ON THE 1ST AND 4TH BCD WORDS 
C     IN A GROUP OF 5)        
C        
C     INPUT  - KHR  = CHARACTER STRING IN CHARACTER*80, CHARACTER*100,  
C                     AND CHARACTER*2        
C     OUTPUT - BCD4 = BCD ARRAYS (OF DIMENSION NWDS)        
C                     EACH BCD4 WORD HOLDS ONLY 4 BYTES OF DATA        
C        
C     INTEGER       BCD4(1),KHR(1),BLANK,BLK90        
C                                     1 2 3 4 5 6 7 8 9 10        
C     DATA          M1234,M12,M34 / O"77777777000000000000",        
C    1                              O"77770000000000000000",        
C    2                              O"00007777000000000000"/        
C     DATA          M5678,M90     / O"00000000777777770000",        
C    1                              O"00000000000000007777"/        
C     DATA          M3456,M7890   / O"00007777777700000000",        
C    1                              O"00000000000077777777"/        
C     DATA          BLANK,BLK90   / O"00000000555555555555",        
C    1                              O"00000000000000005555"/        
C                                     1 2 3 4 5 6 7 8 9 10        
C        
C     SUBROUTINE KHRBCD (KHR,BCD4)        
C     ============================        
C     A80 ----> 20A4        
C        
C     NWDS = 20        
C     GO TO 40        
C        
C        
C     ENTRY KHRBC1 (KHR,BCD4)        
C     =======================        
C     A100 ----> 25A4        
C        
C     NWDS = 25        
C     GO TO 40        
C        
C        
C     ENTRY KHRBC2 (KHR,BCD4)        
C     =======================        
C     A8 ----> 2A4        
C        
C     NWDS = 2        
C        
C40   I   =-5        
C     II  = 0        
C50   I   = I+5        
C     IF (I .GE. NWDS) GO TO 80        
C     II  = II+1        
C     NW1 = KHR(II)        
C     NN  = AND(NW1,M1234)        
C     BCD4(I+1) = OR(NN,BLANK)        
C     NW1 = SHIFT(NW1,24)        
C     NN  = AND(NW1,M1234)        
C     BCD4(I+2) = OR(NN,BLANK)        
C     IF (I+2 .GE. NWDS) GO TO 80        
C     NW1 = SHIFT(NW1,24)        
C     II=II+1        
C     NW2 = KHR(II)        
C     IF (I+3 .LT. NWDS) GO TO 60        
C     NW2 = SHIFT(NW2,-12)        
C     GO TO 70        
C60   NW2 = SHIFT(NW2,12)        
C     NN  = AND(NW2,M1234)        
C     BCD4(I+4) = OR(NN,BLANK)        
C     NW2 = SHIFT(NW2,24)        
C     NN  = AND(NW2,M1234)        
C     BCD4(I+5) = OR(NN,BLANK)        
C     NW2 = SHIFT(NW2,12)        
C70   NW1 = AND(NW1,M12)        
C     NW2 = AND(NW2,M34)        
C     NN  = OR(NW1,NW2)        
C     BCD4(I+3) = OR(NN,BLANK)        
C     GO TO 50        
C80   CONTINUE        
C     GO TO 140        
C        
C        
C     ENTRY BCDKH8 (BCD4,KHR)        
C     =======================        
C     20A4 ----> A80        
C        
C     INPUT  - BCD4 = BCD ARRAYS (OF DIMENSION NWDS). BCD DATA ARE IN   
C                     A4 FORMAT        
C     OUTPUT - KHR  = CHARACTER STRING IN CHARACTER*80, CHARACTER*100,  
C                     AND CHARACTER*2        
C        
C     NWDS = 20        
C     GO TO 100        
C        
C     ENTRY BCDKH7 (BCD4,KHR)        
C     =======================        
C     18A4 ----> A72        
C        
C     NWDS = 18        
C     GO TO 100        
C        
C     ENTRY BCDKH2 (BCD4,KHR)        
C     =======================        
C     2A4 ----> A8        
C        
C     NWDS = 2        
C        
C100  I   =-5        
C     II  = 0        
C110  I   = I+5        
C     IF (I .GE. NDWS) GO TO 140        
C     II  = II+1        
C     NW1 = AND(BCD4(I+1),M1234)        
C     NW2 = SHIFT(BCD4(I+2),-24)        
C     NW2 = AND(NW2,M5678)        
C     KHR(II) = OR(NW1,NW2)        
C     IF (I+2 .GE. NWDS) GO TO 120        
C     NW3 = SHIFT(BCD4(I+3),12)        
C     NW2 = AND(NW3,M90)        
C     KHR(II) = OR(KHR(II),NW2)        
C     II  = II+1        
C     KHR(II) = AND(NW3,M12)        
C     NW3 = SHIFT(BCD4(I+4),-12)        
C     NW1 = AND(NW3,M3456)        
C     NM3 = SHIFT(BCD4(I+5),-36)        
C     NW2 = AND(NW3,M7890)        
C     NW3 = OR(NW1,NW2)        
C     KHR(II) = OR(KHR(II),NW3)        
C     GO TO 110        
C120  KHR(II) = OR(KHR(II),BLK90)        
C        
C140  RETURN        
C     END        
C        
C        
C     SUBROUTINE KHRBCD (KHR80,BCD4)        
C        
C     64-BIT MACHINE, UNIX VERSION        
C     ============================        
C        
C     THIS GROUP OF ROUTINES ARE CALLED BY XREAD, RCARD2, AND XRCARD    
C        
C     CHARACTER*100 KHR100, KDUM        
C     CHARACTER*80  KHR80 , K80        
C     CHARACTER*72  KHR72 , K72        
C     CHARACTER*8   KHR8  , K8,   BCD4(1)        
C        
C     EQUIVALENCE   (KDUM,K100,K80,K72,K8)        
C        
C        
C     SUBROUTINE KHRBCD (KHR80,BCD4)        
C     ==============================        
C     A80 ----> 20A4        
C        
C     K80  = KHR80        
C     NWDS = 20        
C     GO TO 100        
C        
C        
C     ENTRY KHRBC1 (KHR100,BCD4)        
C     ==========================        
C     A100 ----> 25A4        
C        
C     KDUM = KHR100        
C     NWDS = 25        
C     GO TO 100        
C        
C        
C     ENTRY KHRBC2 (KHR8,BCD4)        
C     ========================        
C     A8 ----> 2A4        
C        
C     K8   = KHR8        
C     NWDS = 2        
C        
C 100 I2 = 0        
C     DO 200 I = 1,NWDS        
C     I1 = I2 + 1        
C     I2 = I2 + 4        
C     BCD4(I) = KDUM(I1:I2)        
C 200 CONTINUE        
C     GO TO 800        
C        
C        
C     ENTRY BCDKH8 (BCD4,KHR80)        
C     =========================        
C     20A4 ----> A80        
C        
C     NWDS  = 20        
C     GO TO 300        
C        
C        
C     ENTRY BCDKH7 (BCD4,KHR72)        
C     =========================        
C     18A4 ----> A72        
C        
C     NWDS = 18        
C     GO TO 300        
C        
C        
C     ENTRY BCDKH2 (BCD4,KHR8)        
C     ========================        
C     2A4 ----> A8        
C        
C     NWDS = 2        
C        
C 300 I2 = 0        
C     DO 400 I = 1,NWDS        
C     I1 = I2 + 1        
C     I2 = I2 + 4        
C     KDUM(I1:I2) = BCD4(I)        
C 400 CONTINUE        
C     IF (NWDS-18) 500,600,700        
C        
C 500 KHR8 = K8        
C     GO TO 800        
C        
C 600 KHR72 = K72        
C     GO TO 800        
C        
C 700 KHR80 = K80        
C 800 RETURN        
C        
      END        
