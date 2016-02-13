      INTEGER FUNCTION KHRFN1 (WORD1,I,WORD2,J)        
C        
C     CHARACTER-FUNCTIONS 1,2,3,4, AND 5 WERE WRITTEN BY G.CHAN/UNISYS  
C     TO STANDARDIZE NASTRAN BCD-WORD BYTE PROCESSING.        
C        
C     NOTE - THE INPUT WORD(S) ARE INTEGERS OR REALS, HOLDING BCD TYPE  
C            DATA. (NOT CHARACTER)        
C            BYTE COUNTS FROM LEFT TO RIGHT        
C        
C     THESE FIVE CHARACTER FUNCTIONS ARE COMPLETELY MACHINE INDEPENDENT 
C        
C     KHRFN1 REPLACES THE I-TH BYTE OF WORD 1 BY THE J-TH BYTE OF WORD2 
C     E.G.   WORD1=ABCD,    WORD2=1234        
C            KHRFN1(WORD1,3,WORD2,2) GIVES  AB2D        
C        
C     ABSOLUTE VALUES OF I AND J ARE USED        
C        
C     THE CODE BELOW WORKS WITH ALL MACHINES.  HOWEVER, SEE THE        
C     SIMPLIFIED VERSION FURTHER DOWN.        
C        
C     INTEGER      WORD1(1),WORD2(1),TEMP(2)        
C     CHARACTER*8  TEMP8        
C        
C     TEMP(1) = WORD1(1)        
C     TEMP(2) = WORD2(1)        
C     CALL BCDKH2 (TEMP,TEMP8)        
C     II = IABS(I)        
C     JJ = IABS(J) + 4        
C     TEMP8(II:II) = TEMP8(JJ:JJ)        
C     CALL KHRBC2 (TEMP8,TEMP)        
C     KHRFN1 = TEMP(1)        
C        
C     SIMPLIFIED VERSION        
C        
C     FOR MACHINES (CDC, IBM, VAX, AND GRAY) THAT ALLOW EQUIVALENCE     
C     BETWEEN CHARACTERS AND INTEGER VARIABLES, THE FOLLOWING SIMPLIFIED
C     CODE CAN BE USED.        
C        
      INTEGER      WORD1(1),WORD2(1),TEMP1,TEMP2        
      CHARACTER*4  TEMPC1,TEMPC2        
C     CHARACTER*n  TEMPC1,TEMPC2        
C        (WHERE n is 10 for CDC, 8 for 64-BIT UNIX and        
C                     4 for VAX and IBM)        
      EQUIVALENCE (TEMP1,TEMPC1), (TEMP2,TEMPC2)        
C        
      TEMP1 = WORD1(1)        
      TEMP2 = WORD2(1)        
      II = IABS(I)        
      JJ = IABS(J)        
      TEMPC1(II:II) = TEMPC2(JJ:JJ)        
      KHRFN1 = TEMP1        
      RETURN        
C
C     DEC/ULTRIX VERSION
C     ==================
C     THE ABOVE VAX VERSION DOES NOT WORK IN DEC/ULTRIX(RISC)
C
C     INTEGER     TEMP1,TEMP2,WORD1,WORD2
C     CHARACTER*1 TMP1(4),TMP2(4)
C     EQUIVALENCE (TEMP1,TMP1(1)),(TEMP2,TMP2(1))
C
C     TEMP1 = WORD1
C     TEMP2 = WORD2
C     II = IABS(I)
C     JJ = IABS(J)
C     TMP1(II) = TMP2(JJ)
C     KHR = TEMP1
C     RETURN
C        
C     CDC VERSION        
C     ===========        
C     THE CHARACTER OPERATIONS IN CDC MACHINE ARE EXTREMELY SLOW.       
C     THE FOLLOWING CODE, USING SHIFT/AND/OR IS 2 TO 3 TIMES        
C     FASTER        
C        
C     INTEGER       WORD1,WORD2,MASK1(4),MASK2(4),BLANK        
C     DATA  MASK1 / O"77000000000000000000", O"00770000000000000000",   
C    1              O"00007700000000000000", O"00000077000000000000"/   
C     DATA  MASK2 / O"00777777000000000000", O"77007777000000000000",   
C    1              O"77770077000000000000", O"77777700000000000000"/   
C     DATA  BLANK / O"00000000555555555555"/        
C        
C     II = IABS(I)        
C     JJ = IABS(J)        
C     KK = (JJ-II)*6        
C     JJ = SHIFT(WORD2,KK)        
C     KK = AND(JJ,MASK1(II))        
C     JJ = AND(WORD1,MASK2(II))        
C     II = OR(JJ,KK)        
C     KHRFN1 = OR(II,BLANK)        
C     RETURN        
C        
C        
C     UNIVAC VERSION (1988 ORIGINAL)        
C     ==============================        
C        
C     INTEGER     WORD1(1),WORD2(1)        
C     CHARACTER   W1(8)*1 ,W4*4, W8*8        
C     EUIVALENCE  (W1(1),W4,W8)        
C        
C     WRITE  (W8,10) WORD1(1),WORD2(1)        
C  10 FORMAT (2A4)        
C     II = IABS(I)        
C     JJ = IABS(J) + 4        
C     W1(II) = W1(JJ)        
C     READ (W4,20) KHRFN1        
C  20 FORMAT (A4)        
C     RETURN        
C        
      END        
