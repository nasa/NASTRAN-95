      SUBROUTINE MBCAP(NPHI,CAPPHI)
C
      REAL  KM , KBAR , MACH , W(10), P(10)
      COMPLEX  CAPPHI(1)
      COMMON /MBOXC/ NJJ ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,
     *  NPTS0,NPTS1,NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,EKM,
     *  BOXL,BOXW,BOXA ,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,KCT,KC1T,KC2T
      EQUIVALENCE  ( KM , EKM ) , ( KBAR , EKBAR )
      DATA  W / 0.0506143,0.1111905,0.1568533,0.1813419,0.1813419,
     *          0.1568533,0.1111905,0.0506143,0.0,0.0/,
     *      P / 0.0198551,0.1016667,0.2372338,0.4082826,0.5917174,
     *          0.7627662,0.8983333,0.9801449,0.0,0.0/
C
      DO   200   I = 1 , NPHI
      CAPPHI(I)  =  ( 0.0 , 0.0 )
 200  CONTINUE
C
C     COMPUTE CAPPHI FOR RECEIVING BOX
C
      IF ( KBAR .LE. 0.0 )   GO TO  400
      DO   300   I = 1 , 8
      J  =  9 - I
      ARG  =  KBAR * P(J) / 2.0
      ARG1  =  W(I) * ZJ ( ARG / MACH ) / 2.0
      CAPPHI(1)  =  CAPPHI(1) + CMPLX ( -COS ( ARG ) * ARG1 ,
     *              SIN ( ARG ) * ARG1 )
 300  CONTINUE
      GO TO 500
C
 400  CAPPHI(1)  =  ( -0.5 , 0.0 )
C
C     COMPUTE REMAINING CAPPHI
C
 500  NPHI  =  1
      XB  =  0.5
      XU  =  XB + 1.0
      DO   900   I = 2 , NCB
      XL  =  -0.5
      XR  =  XL + 1.0
      DO   700   J = 1 , I
      NPHI  =  NPHI + 1
      DO   600   L = 1 , 8
      X  =  XB + P(L)
      ARG  =  KBAR * X
      ARG1  =  W(L) * GO ( X , XR , XL , KM ) / 3.14159265
      CAPPHI(NPHI)  =  CAPPHI(NPHI) - CMPLX ( COS ( ARG ) * ARG1 ,
     *                 -SIN ( ARG ) * ARG1 )
 600  CONTINUE
      XL  =  XR
      XR  =  XR + 1.0
 700  CONTINUE
C
      XB  =  XU
      XU  =  XB + 1.0
 900  CONTINUE
C
      DO   1000   I = 1 , NPHI
      CAPPHI(I)  =  BOXW * CAPPHI(I)
 1000 CONTINUE
      RETURN
      END
