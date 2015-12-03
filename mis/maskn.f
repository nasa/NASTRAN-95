      FUNCTION MASKN (L2,L1)
C
C     TO BUILD AN INTEGER MASK FOR BIT MANIPULATION
C                                                                   0 OR
C     64  60        48     36   32         <--- BIT COUNT ---          1
C      +---+---------+------+----+-------------------------------------+
C       ...   ....     ....   ...  ....00000011111111111111111111000...
C      +---+---------+------+----+-------------------------------------+
C                                            /                  /
C                                            L2                L1
C
C      BIT COUNTS FROM RIHGT (L1) TO LEFT (L2).  L1=0 IS SAME AS L1=1
C
C      E.G.      L2    L1      MASK PATTERN, WITH LEADING ZERO BITS
C               ----  ----  ------------------------------------------
C                12     0    A 12 BIT MASK, RIGHT ADJUSTED
C                24     8    A 24 BIT MASK, RIGHT ADJUSTED, WITH 8
C                            TRAILING ZERO BITS.
C
C      THIS ROUTINE IS SUITABLE FOR MACHINE WORD OF ANY BIT SIZE
C      BIT PATTERN CAN ALSO INCLUDE SIGN BIT.
C      SYSTEM MASK ROUINTE, IF IT EXISTS, IS NOT USED.
C
C      WRITTEN BY G.CHAN/UNISYS  10/1992
C
       EXTERNAL        RSHIFT,LSHIFT
       INTEGER         RSHIFT
C
       IF (L2 .LT. L1) CALL ERRTRC ('MASKN   ',L2-L1)
       MASKN = LSHIFT(1,L2) - 1
       IF (L1 .GT. 1) MASKN = LSHIFT(RSHIFT(MASKN,L1-1),L1-1)
       RETURN
       END
