      SUBROUTINE DELTKL (DEL,R,Z,KODE)
C
C     EVAULATE -
C        DELT(K,L) = SURFACE-INTEGRAL((R**K)*(Z**L)) DR*DZ
C        WHERE  DR*DZ IS EITHER A TRIANGLE OR A TRAPEZOID.
C
C     USAGE -
C        CALL  DELTKL (DEL,R,Z,KODE)
C        WHERE  DEL   =  DOUBLE PRECISION ARRAY OF 15 LOCATIONS.
C                        CONTAINING THE RESULTS.
C        WHERE  R     =  DOUBLE PRECISION ARRAY OF  4 LOCATIONS.
C                        CONTAINING THE R-COORDINATES OF THE ELEMENT
C        WHERE  Z     =  DOUBLE PRECISION ARRAY OF  4 LOCATIONS.
C                        CONTAINING THE Z-COORDINATES OF THE ELEMENT
C               KODE  =  0  FOR TRIANGULAR  ELEMENT
C               KODE  =  1  FOR TRAPEZOIDAL ELEMENT
C
C     PROCEDURE -
C        INFORMATION IS COMPUTED AND STORED AS FOLLOWS.
C
C           COMPUTED  FOR        ELEMENT         STORED
C        TRIANGLE   TRAPEZOID    DELT(K,L)      DEL(LOC)
C        ================================================
C           X          X              0,0            1
C           X          X              1,0            2
C           X          X              0,1            3
C           X          X             -1,0            4
C           X          X             -1,1            5
C           X          X             -1,2            6
C                      X              1,1            7
C                      X              1,2            8
C                      X              2,1            9
C                      X              2,0           10
C                      X              0,2           11
C                      X              3,0           12
C                      X              3,1           13
C                      X              3,2           14
C                      X              2,2           15
C
      INTEGER          GOBACK,KODE,N,I,M,L
      DOUBLE PRECISION DEL(15),R(4),Z(4),RM,RI,ZM,ZI,LN,A,B,SI,SM,
     1                 R1,R2,R3,R4,R5,AB,A2,B2,A3,B3,AAB,ABB,R6,R7
C
C     ZERO ARRAY (ONLY THAT PORTION USING)
C
      N = 15
      DO 2 L = 1,N
    2 DEL(L) = 0.0D+0
C
C     HERE FOR LINE 1-2
C
      I = 1
      M = 2
      ASSIGN  23 TO GO BACK
      GO TO 50
C
C     HERE FOR LINE 2-3
C
   23 CONTINUE
      I = 2
      M = 3
      ASSIGN 34 TO GO BACK
      GO TO 50
C
C     HERE FOR LINE 31 (TRIANGLE),  LINE 3-4 (TRAP)
C
   34 CONTINUE
      I = 3
      IF (KODE .GT. 0) GO TO 35
      M = 1
      ASSIGN 90 TO GO BACK
      GO TO 50
   35 M = 4
      ASSIGN 41 TO GO BACK
      GO TO 50
   41 I = 4
      M = 1
      ASSIGN 90 TO GO BACK
C
C     BEGIN LOCAL SUBROUTINE (DEL-KL-I,M)
C
   50 RM = R(M)
      RI = R(I)
      R1 = RM - RI
      IF (DABS(R1) .LT. 1.0D-07) GO TO 80
C
C     THIS LINE IS NOT PARALLEL TO Z-AXIS
C
      ZM = Z(M)
      ZI = Z(I)
      IF (ZI.EQ.0.0D+0 .AND. ZM.EQ.0.0D+0) GO TO 80
C
C     SPECIAL CASE, ZM = ZI = 0   THUS ALL  A,B = 0  AND
C     ALL DEL TERMS  = 0 .   THUS SKIP AND SAVE CPU.
C
      A   = (RM*ZI - RI*ZM)/R1
      B   = (ZM - ZI)/R1
      LN  = DLOG(RM/RI)
      SI  = RI * RI
      SM  = RM * RM
      R2  = SM - SI
      SI  = SI * RI
      SM  = SM * RM
      R3  = SM - SI
      SI  = SI * RI
      SM  = SM * RM
      R4  = SM - SI
      SI  = SI * RI
      SM  = SM * RM
      R5  = SM - SI
      A2  = A  * A
      A3  = A  * A2
      B2  = B  * B
      B3  = B  * B2
      AB  = A  * B
      AAB = A  * AB
      ABB = B  * AB
      DEL( 1) = A*R1 + B*R2/2.0D+0 + DEL(1)
      DEL( 2) = A*R2/  2.0D+0 + B*R3 /3.0D+0 + DEL(2)
      DEL( 3) = A2*R1/ 2.0D+0 + AB*R2/2.0D+0 + B2*R3/6.0D+0 + DEL(3)
      DEL( 4) = A*LN + B*R1   + DEL(4)
      DEL( 5) = A2*LN/ 2.0D+0 + AB*R1  + B2*R2 /4.0D+0 + DEL(5)
      DEL( 6) = A3*LN/ 3.0D+0 + AAB*R1 + ABB*R2/2.0D+0 + B3*R3/9.0D+0
     1        + DEL(6)
      DEL( 7) = A2*R2/ 4.0D+0 + AB*R3/3.0D+0  + B2*R4 /8.0D+0 + DEL(7)
      DEL( 8) = A3*R2/ 6.0D+0 + AAB*R3/3.0D+0 + ABB*R4/4.0D+0
     1        + B3*R5/15.0D+0 + DEL(8)
      DEL( 9) = A2*R3/ 6.0D+0 + AB*R4/4.0D+0 + B2*R5/10.0D+0 + DEL(9)
      DEL(10) = A *R3/ 3.0D+0 + B*R4 /4.0D+0 + DEL(10)
      DEL(12) = A *R4/ 4.0D+0 + B*R5 /5.0D+0 + DEL(12)
      IF (KODE .LT. 1) GO TO 80
      SI      = SI*RI
      SM      = SM*RM
      R6      = SM - SI
      R7      = (SM*RM - SI*RI)
      DEL(11) = A3*R1/ 3.0D+0 + AAB*R2/2.0D+0 + ABB*R3/3.0D+0
     1        + B3*R4/12.0D+0 + DEL(11)
      DEL(13) = A2*R4/ 8.0D+0 + AB*R5 /5.0D+0 + B2*R6/12.0D+0 + DEL(13)
      DEL(14) = A3*R4/12.0D+0 + AAB*R5/5.0D+0 + ABB*R6/6.0D+0
     1        + B3*R7/21.0D+0 + DEL(14)
      DEL(15) = A3*R3/ 9.0D+0 + AAB*R4/4.0D+0 + ABB*R5/5.0D+0
     1        + B3*R6/18.0D+0 + DEL(15)
   80 GO TO GO BACK, (23,34,41,90)
C
C     THE ABSOLUTE VALUE IS CHOSEN SO THAT NODES INPUT MAY BE ORDERED
C     CW OR CCW.   RESULTS ARE SAME FOR A GIVEN ELEMENT.
C
   90 DO 95 L = 1,N
   95 DEL(L) = DABS(DEL(L))
      RETURN
      END
