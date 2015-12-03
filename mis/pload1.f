      SUBROUTINE PLOAD1 (OPT,ISLT,V,SA,SB,BA,BB,PA,PB,TA,TB,SLT,EPT)
C
C     PLOAD1 CALCULATES THE END LOADS ON A BAR ELEMENT FOR PLOAD1 LOADS
C     IT IS CALLED ONLY BY PLBAR1
C
C     OPT   = 1, CALLED FROM PLBAR1/EXTERN,  2, CALLED FROM SDRX
C     SLT   = PLOAD1 CARD
C     V     = REFERENCE VECTOR IN BASIC
C     SA    = OFFSET VECTOR IN BASIC POINT A
C     SB    = OFFSET VECTOR IN BASIC POINT B
C     BA    = BASIC  COORD  FOR POINT A
C     BB    = BASIC  COORD  FOR POINT B
C     PA    = LOAD   VECOTR FOR POINT A
C     PB    = LOAD   VECOTR FOR POINT B
C     TA,TB = TRANSFORMATION MATRICES FOR A AND B ONLY USED WITH OPT 1
C     EPT   = POINTER TO EST
C
      INTEGER          OPT,OLDID,TYPE,SCALE,ISLT(7)
      REAL             LEN
      DOUBLE PRECISION AX,AY,AZ,BX,BY,BZ,DX1,DX2,DL,DT,DFX1,DFY1,DFZ1,
     1                 DFX2,DFY2,DFZ2,S1,S2,S3,S4,S5,
     2                 I01,I11,I21,I31,I41,I02,I12,I22,I32,I42
      DIMENSION        V(3),SA(3),SB(3),BA(3),BB(3),PA(6),PB(6),EPT(32),
     1                 A(3),B(3),C(3),D(9),E(9),SLT(7),TA(9),TB(9),TP(3)
      COMMON /MATOUT/  F,G
      EQUIVALENCE      (A(1),E(1)),(B(1),E(7)),(C(1),E(4))
      DATA    OLDID ,  D / 10*0 /
C
      IF (OLDID .EQ. ISLT(1)) GO TO 20
      OLDID = ISLT(1)
C
C     CALCULATE AXIS AND LENGTH, AND THE E MATRIX
C
      A(1) = BB(1)-BA(1) + SB(1)-SA(1)
      A(2) = BB(2)-BA(2) + SB(2)-SA(2)
      A(3) = BB(3)-BA(3) + SB(3)-SA(3)
      LEN  = SQRT(SADOTB(A,A))
      IF (LEN .EQ. 0.0) GO TO 380
      A(1) = A(1)/LEN
      A(2) = A(2)/LEN
      A(3) = A(3)/LEN
      CALL SAXB (A,V,B)
C
      TEMP = SQRT(SADOTB(B,B))
      IF (TEMP .EQ. 0.0) GO TO 380
      B(1) = B(1)/TEMP
      B(2) = B(2)/TEMP
      B(3) = B(3)/TEMP
      CALL SAXB (B,A,C)
C
C     TRANSVERSE SHEAR
C
      TEMP = EPT(31)*EPT(17)*G*LEN**2
      TMP  = 12.0*F*EPT(18)
      ALY  = 0.0
      IF (ABS(TEMP+TMP) .GT. 1.0E-14) ALY = TMP/(TMP+TEMP)
      OMALY = 1.0 - ALY
C
      TEMP = (TEMP/EPT(31))*EPT(32)
      TMP  = 12.0*F*EPT(19)
      ALZ  = 0.0
      IF (ABS(TEMP+TMP) .GT. 1.0E-14) ALZ = TMP/(TMP+TEMP)
      OMALZ = 1.0 - ALZ
C
C     START BUILDING THE FORCES AND MOMENTS
C
   20 TYPE  = ISLT(2)
      SCALE = ISLT(3)
      X1  =  SLT(4)
      F1  =  SLT(5)
      X2  =  SLT(6)
      F2  =  SLT(7)
      I   = (TYPE-1)/6 + 1
      J   = MOD(TYPE,6)
      IF (J .EQ. 0) GO TO 60
      GO TO (30,30,30,40,50,60), J
   30 FX1 = A(J)*F1
      FY1 = C(J)*F1
      FZ1 = B(J)*F1
      FX2 = A(J)*F2
      FY2 = C(J)*F2
      FZ2 = B(J)*F2
      GO TO 100
   40 FX1 = F1
      FY1 = 0.0
      FZ1 = 0.0
      FX2 = F2
      FY2 = 0.0
      FZ2 = 0.0
      GO TO 100
   50 FX1 = 0.0
      FY1 = F1
      FZ1 = 0.0
      FX2 = 0.0
      FY2 = F2
      FZ2 = 0.0
      GO TO 70
   60 FX1 = 0.0
      FY1 = 0.0
      FZ1 = F1
      FX2 = 0.0
      FY2 = 0.0
      FZ2 = F2
   70 J   = 4
C
C     SCALED
C
  100 IF (SCALE.EQ.2 .OR. SCALE.EQ.4) GO TO 110
      X1  = X1/LEN
      X2  = X2/LEN
C
C     DISTRIBUTED SCALED LOADS
C
  110 IF (X1 .EQ. X2) GO TO 220
      IF (SCALE.LE.2 .OR. J.EQ.4) GO TO 120
      FSCALE = SQRT(1.0-A(J)**2)
      FX1  = FSCALE*FX1
      FY1  = FSCALE*FY1
      FZ1  = FSCALE*FZ1
      FX2  = FSCALE*FX2
      FY2  = FSCALE*FY2
      FZ2  = FSCALE*FZ2
C
C     DISTRIBUTED LOADS
C
  120 DX1  = X1
      DX2  = X2
      DL   = LEN
      DFX1 = FX1
      DFY1 = FY1
      DFZ1 = FZ1
      DFX2 = FX2
      DFY2 = FY2
      DFZ2 = FZ2
      S1   = DX2 - DX1
      S2   = .5000000D0*(DX2**2 - DX1**2)
      S3   = .3333333D0*(DX2**3 - DX1**3)
      S4   = .2500000D0*(DX2**4 - DX1**4)
      S5   = .2000000D0*(DX2**5 - DX1**5)
      IF (I .EQ. 2) GO TO 140
C
C     FORCES
C
      I01  = DL*(S1-S2)
      I11  = DL*(S1-3.0D0*S3 + 2.0D0*S4)
      I21  = DL*(   3.0D0*S3 - 2.0D0*S4)
      I31  = DL*(S2-2.0D0*S3 + S4)
      I41  = DL*(S4-S3)
      DT   = DL*DL
      I02  = DT*(S2-S3)
      IF (F1 .EQ. F2) GO TO 130
      I12  = DT*(S2-3.0D0*S4 + 2.0D0*S5)
      I22  = DT*(   3.0D0*S4 - 2.0D0*S5)
      I32  = DT*(S3-2.0D0*S4 + S5)
      I42  = DT*(S5-S4)
      DT   = DL*(DX2-DX1)
      BX   = (DFX2-DFX1)/DT
      BY   = (DFY2-DFY1)/DT
      BZ   = (DFZ2-DFZ1)/DT
      AX   = DFX1 - DX1*BX*DL
      AY   = DFY1 - DX1*BY*DL
      AZ   = DFZ1 - DX1*BZ*DL
      GO TO 170
  130 AX   = DFX1
      AY   = DFY1
      AZ   = DFZ1
      GO TO 160
C
C     MOMENTS
C
  140 I01  = DL*(S1-S2)
      I11  =-6.0D0*(S2-S3)
      I21  =-I11
      I31  = S1 - 4.0D0*S2 + 3.0D0*S3
      I41  =    - 2.0D0*S2 + 3.0D0*S3
      IF (F1 .EQ. F2) GO TO 150
      I02  = (S2-S3)*DL**2
      I12  =-6.0D0*DL*(S3-S4)
      I22  =-I12
      I32  = DL*(S2-4.0D0*S3 + 3.0D0*S4)
      I42  =-DL*(   2.0D0*S3 - 3.0D0*S4)
      DT   = (DX2 -DX1)*DL
      BX   = (DFX2-DFX1)/DT
      BY   = (DFZ2-DFZ1)/DT
      BZ   =-(DFY2-DFY1)/DT
      AX   = DFX1 + DX1*BX*DL
      AY   = DFZ1 + DX1*BY*DL
      AZ   =-DFY1 + DX1*BZ*DL
      GO TO 170
  150 AX   = DFX1
      AY   = DFZ1
      AZ   =-DFY1
  160 BX   = 0.0D0
      BY   = 0.0D0
      BZ   = 0.0D0
      I12  = 0.0D0
      I22  = 0.0D0
      I32  = 0.0D0
      I42  = 0.0D0
C
C     LOADS
C
  170 PA(1) = I01*AX + I02*BX
      PA(2) = I11*AY + I12*BY
      PA(3) = I11*AZ + I22*BZ
      PA(4) = 0.0
      PA(5) =-DL*(I31*AZ + I32*BZ)
      PA(6) = DL*(I31*AY + I32*BY)
      DT    = DL*DL
      PB(1) = DL*S2*AX + DT*S3*BX
      PB(2) = I21  *AY + I22  *BY
      PB(3) = I21  *AZ + I22  *BZ
      PB(4) = 0.0
      PB(5) =-DL*(I41*AZ + I42*BZ)
      PB(6) = DL*(I41*AY + I42*BY)
      IF (I .EQ. 2) GO TO 190
      IF (ALY .EQ. 0.0) GO TO 180
      PA(2) = OMALY*PA(2) + ALY*(  I01*AY + I02*BY   )
      PA(6) = OMALY*PA(6) + ALY*(  I02*AY - I41*BY*DT)*.50
      PB(2) = OMALY*PB(2) + ALY*(DL*S2*AY + S3 *BY*DT)
      PB(6) = OMALY*PB(6) - ALY*(  I02*AY - I41*BY*DT)*.50
  180 IF (ALZ .EQ. 0.0) GO TO 300
      PA(3) = OMALZ*PA(3) + ALZ*(  I01*AZ + I02*BZ   )
      PA(5) = OMALZ*PA(5) - ALZ*(  I02*AZ - I41*BZ*DT)*.50
      PB(3) = OMALZ*PB(3) + ALZ*(DL*S2*AZ + S3 *BZ*DT)
      PB(5) = OMALZ*PB(5) + ALZ*(  I02*AZ - I41*BZ*DT)*.50
      GO TO 300
  190 TEMP  = PA(1)
      PA(1) = PA(4)
      PA(4) = TEMP
      TEMP  = PB(1)
      PB(1) = PB(4)
      PB(4) = TEMP
      IF (ALY .EQ. 0.0) GO TO 200
      PA(2) = OMALY*PA(2)
      PA(6) = OMALY*PA(6) + ALY*(I01*AY + I02*BY)
      PB(2) = OMALY*PB(2)
      PB(6) = OMALY*PB(6) + ALY*(DL*S2*AY + S3*BY*DT)
  200 IF (ALZ .EQ. 0.0) GO TO 300
      PA(3) = OMALZ*PA(3)
      PA(5) = OMALZ*PA(5) + ALZ*(I01*AZ + I02*BZ)
      PB(3) = OMALZ*PB(3)
      PB(5) = OMALZ*PB(5) + ALZ*(DL*S2*AZ + S3*BZ*DT)
      GO TO 300
C
C     CONCENTRATED LOADS
C
  220 TMP = 1.0 - X1
      IF (I .EQ. 2) GO TO 230
C
C     FORCES
C
      TEMP  = 1.0 - 3.0*X1**2 + 2.0*X1**3
      PA(1) = TMP*FX1
      PA(2) = TEMP*FY1*OMALY + FY1*TMP*ALY
      PA(3) = TEMP*FZ1*OMALZ + FZ1*TMP*ALZ
      PA(4) = 0.0
      TEMP  =-LEN*X1*TMP**2
      TMP   = TMP*LEN*X1*.50
      PA(5) = TEMP*FZ1*OMALZ - FZ1*TMP*ALZ
      PA(6) =-TEMP*FY1*OMALY + FY1*TMP*ALY
      TEMP  = 3.0*X1**2 - 2.0*X1**3
      PB(1) = X1*FX1
      PB(2) = TEMP*FY1*OMALY + FY1*X1*ALY
      PB(3) = TEMP*FZ1*OMALZ + FZ1*X1*ALZ
      PB(4) = 0.0
      TEMP  = (1.0-X1)*LEN*X1**2
      PB(5) = TEMP*FZ1*OMALZ + FZ1*TMP*ALZ
      PB(6) =-TEMP*FY1*OMALY - FY1*TMP*ALY
      GO TO 300
C
C     MOMENTS
C
  230 TEMP  =-(6.0/LEN*X1)*TMP
      PA(1) = 0.0
      PA(2) = TEMP*FZ1*OMALY
      PA(3) =-TEMP*FY1*OMALZ
      PA(4) = TMP*FX1
      TEMP  = 1.0 - 4.0*X1 + 3.0*X1**2
      PA(5) = TEMP*FY1*OMALZ + FY1*TMP*ALZ
      PA(6) = TEMP*FZ1*OMALY + FZ1*TMP*ALY
      PB(1) = 0.0
      PB(2) =-PA(2)
      PB(3) =-PA(3)
      PB(4) = X1*FX1
      TEMP  = 3.0*X1**2 - 2.0*X1
      PB(5) = TEMP*FY1*OMALZ + FY1*X1*ALZ
      PB(6) = TEMP*FZ1*OMALY + FZ1*X1*ALY
      GO TO 300
C
C     PIN FLAGS
C
  300 CALL PLOAPF (EPT,EPT,LEN,PA,PB)
C
C     LOAD VECTORS DONE FOR SDRX
C
      IF (OPT .EQ. 2) GO TO 400
C
C     TRANSFORM LOAD VECTOR TO GLOBAL
C
      CALL GMMATS (E ,3,3,1,PA(1),3,1,0,TP   )
      CALL GMMATS (TA,3,3,1,TP   ,3,1,0,PA(1))
      CALL GMMATS (E ,3,3,1,PB(1),3,1,0,TP   )
      CALL GMMATS (TB,3,3,1,TP   ,3,1,0,PB(1))
      CALL GMMATS (E ,3,3,1,PA(4),3,1,0,TP   )
      CALL GMMATS (TA,3,3,1,TP   ,3,1,0,PA(4))
      CALL GMMATS (E ,3,3,1,PB(4),3,1,0,TP   )
      CALL GMMATS (TB,3,3,1,TP   ,3,1,0,PB(4))
C
      DO 310 I = 1,3
      IF (SA(I) .NE. 0.0) GO TO 320
  310 CONTINUE
      GO TO 330
  320 D(2)  =-SA(3)
      D(3)  = SA(2)
      D(4)  = SA(3)
      D(6)  =-SA(1)
      D(7)  =-SA(2)
      D(8)  = SA(1)
      CALL GMMATS (D,3,3,0,PA(1),3,1,0,TP)
      PA(4) = PA(4) + TP(1)
      PA(5) = PA(5) + TP(2)
      PA(6) = PA(6) + TP(3)
C
  330 DO 340 I = 1,3
      IF (SB(I) .NE. 0.0) GO TO 350
  340 CONTINUE
      GO TO 400
  350 D(2)  =-SB(3)
      D(3)  = SB(2)
      D(4)  = SB(3)
      D(6)  =-SB(1)
      D(7)  =-SB(2)
      D(8)  = SB(1)
      CALL GMMATS (D,3,3,0,PB(1),3,1,0,TP)
      PB(4) = PB(4) + TP(1)
      PB(5) = PB(5) + TP(2)
      PB(6) = PB(6) + TP(3)
      GO TO 400
C
C     ERROR
C
  380 CALL MESAGE (-30,31,OLDID)
C
  400 RETURN
      END
