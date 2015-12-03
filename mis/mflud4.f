      SUBROUTINE MFLUD4
C*****
C     THIS ROUTINE IS USED FOR THE 4-SIDED FLUID ELEMENT. IT REARRANGES
C      THE DATA AND  CALLS THE MFLUD3 ROUTINE FOR EACH SUBELEMENT.
C****
C     THE ECPT DATA FOR THE ELEMENT AND ITS SUBELEMENTS ARE
C
C        FIELD      SYMBOL(FLUID4)      SYMBOL(FLUID3)
C           1            ID                  ID
C           2            SIL1                SIL1
C           3            SIL2                SIL2
C           4            SIL3                SIL3
C           5            SIL4                RHO
C           6            RHO                 BULK
C           7            BULK                N
C           8            N                   CSF
C           9            CSF                 R1
C          10            R1                  Z1
C          11            Z1                  -
C          12            -                   CSF
C          13            CSF                 R2
C          14            R2                  Z2
C          15            Z2                  -
C          16            -                   CSF
C          17            CSF                 R3
C          18            R3                  Z3
C          19            Z3                  -
C          20            -
C          21            CSF
C          22            R4
C          23            Z4
C          24            -
C          25            -
C****
      INTEGER  NECPT(100)
      COMMON/SMA2IO/ DUM1(10),IFMGG
      COMMON /SMA2CL/    IOPT1,K1GGSW,NPVT
      COMMON /SMA2ET/    ECPT(100)
      EQUIVALENCE   (ECPT(1),NECPT(1))
      IF(ECPT(7) .EQ. 0.0) GO TO 120
      ECPT(7)=ECPT(7)*2.0
      DO 50 I=1,24
   50 ECPT(I+50) =ECPT(I)
      DO 60 I= 5,19
   60 ECPT(I)= ECPT(I+1)
      IRET =1
      GO TO 100
   70 ECPT(4) = ECPT(55)
      ECPT(17)= ECPT(72)
      ECPT(18)= ECPT(73)
      IRET =2
      GO TO 100
   80 ECPT(13)= ECPT(68)
      ECPT(14)= ECPT(69)
      ECPT(3)= ECPT(54)
      IRET=3
      GO TO 100
   90 ECPT(9) = ECPT(64)
      ECPT(10)= ECPT(65)
      ECPT(2)= ECPT(53)
      IRET=4
C*****
C
  100 IF((NECPT(2).NE.NPVT).AND.(NECPT(3).NE.NPVT).AND.
     1   (NECPT(4).NE.NPVT))  GO TO 110
C*****
      CALL MFLUD3
  110 GO TO (70,80,90,120),IRET
  120 RETURN
      END
