      SUBROUTINE DMATRX (D,V,C,CA, CA2, VA, DM, DB, YI)
C
C
C THIS ROUTINE COMPUTES THE STIFFNESS MATRIX IN FIELD COORDINATES FOR
C THE TOROIDAL RING
C
C
C NOTE THE DOUBLE SUBSCRIPTING USED IN DMATRIX SUBROUTINE IS
C COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
C IS A (11X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
C PROGRAM AND IT IS A (6X11) DOUBLY SUBSCRIPTED ARRAY (STORED
C COLUMNWISE) IN DMATRX ROUTINE.
C
C
      DOUBLE PRECISION   D,   V,   C,   CA,  CA2, VA,  DM,  DB,  YI
      DIMENSION  D (10,10) , YI (6,11)
C
C     ------------------------------------------------------------------
C
      D(1,1)   =  DM * (CA2*YI(1,1) + 2.*VA*YI(2,1) + YI(3,1))
      D(2,1)   =  DM * (CA2*YI(1,2) + 2.*VA*YI(2,2) + YI(3,2))
      D(3,1)   =  DM * (CA2*YI(1,3) + 2.*VA*YI(2,3) + YI(3,3))
      D(4,1)   =  DM * (CA2*YI(1,4) + 2.*VA*YI(2,4) + YI(3,4))
      D(5,1)   =  DM * (CA2*YI(1,5) + 2.*VA*YI(2,5) + YI(3,5))
      D(6,1)   =  DM * (CA2*YI(1,6) + 2.*VA*YI(2,6) + YI(3,6))
      D(7,1)   =  DM * (VA*YI(4,1)  +  YI(5,1))
      D(8,1)   =  DM * (CA*YI(1,1)  +  VA*YI(4,2) + V*YI(2,1) + YI(5,2))
      D(9,1)   =  DM * (2.*CA*YI(1,2) + VA*YI(4,3) + 2.*V*YI(2,2) + YI(5
     1                 ,3))
      D(10,1)  =  DM * (3.*CA*YI(1,3) + VA*YI(4,4) + 3.*V*YI(2,3) + YI(5
     1                 ,4))
      D(2,2)   =  DB * YI(6,1)  +  D(3,1)
      D(3,2)   =  DB * (2.*V*YI(4,1) + 2.*YI(6,2))  +  D(4,1)
      D(4,2)   =  DB * (6.*V*YI(4,2) + 3.*YI(6,3))  +  D(5,1)
      D(5,2)   =  DB * (12.*V*YI(4,3) + 4.*YI(6,4)) +  D(6,1)
      D(6,2)   =  DM * (CA2*YI(1,7) + 2.*VA*YI(2,7) + YI(3,7))  +
     1            DB * (20.*V*YI(4,4) + 5.*YI(6,5))
      D (7,2)  =  DM * (VA*YI(4,2) + YI(5,2))
      D(8,2)   =  DM * (CA*YI(1,2) + VA*YI(4,3) + V*YI(2,2) + YI(5,3))
      D(9,2)   =  DM * (2.*CA*YI(1,3) + VA*YI(4,4) + 2.*V*YI(2,3) + YI(5
     1                 ,4))
      D(10,2)  =  DM * (3.*CA*YI(1,4) + VA*YI(4,5) + 3.*V*YI(2,4) + YI(5
     1                 ,5))
      D(3,3)   =  DB * 4.*(C*YI(1,1) + 2.*V*YI(4,2) + YI(6,3)) + D(5,1)
      D(4,3)   =  DB * 6.*(2.*C*YI(1,2) + 3.*V*YI(4,3) + YI(6,4))+D(6,1)
      D(5,3)   =  DM * (CA2*YI(1,7) + 2.*VA*YI(2,7) + YI(3,7))  +
     1        DB * 2.*(12.*C*YI(1,3)+ 16.*V*YI(4,4)+ 4.*YI(6,5))
      D(6,3)   =  DM * (CA2*YI(1,8) + 2.*VA*YI(2,8) + YI(3,8))  +
     1            DB * 10.*(4.*C*YI(1,4) + 5.*V*YI(4,5) + YI(6,6))
      D (7,3)  =  DM * (VA*YI(4,3) + YI(5,3))
      D(8,3)   =  DM * (CA*YI(1,3) + VA*YI(4,4) + V*YI(2,3) + YI(5,4))
      D(9,3)   =  DM * (2.*CA*YI(1,4) + VA*YI(4,5) + 2.*V*YI(2,4) + YI(5
     1                 ,5))
      D(10,3)  =  DM * (3.*CA*YI(1,5) + VA*YI(4,6) + 3.*V*YI(2,5) + YI(5
     1                 ,6))
      D(4,4)   =  DM * (CA2*YI(1,7) + 2.*VA*YI(2,7) + YI(3,7))  +
     1            DB * 9.*(4.*C*YI(1,3) + 4.*V*YI(4,4) + YI(6,5))
      D(5,4)   =  DM * (CA2*YI(1,8) + 2.*VA*YI(2,8) + YI(3,8))  +
     1            DB * 12.*(6.*C*YI(1,4) + 5.*V*YI(4,5) + YI(6,6))
      D(6,4)   =  DM * (CA2*YI(1,9) + 2.*VA*YI(2,9) + YI(3,9))  +
     1            DB * 15.*(8.*C*YI(1,5) + 6.*V*YI(4,6) + YI(6,7))
      D (7,4)  =  DM * (VA*YI(4,4) + YI(5,4))
      D(8,4)   =  DM * (CA*YI(1,4) + VA*YI(4,5) + V*YI(2,4) + YI(5,5))
      D(9,4)   =  DM * (2.*CA*YI(1,5) + VA*YI(4,6) + 2.*V*YI(2,5) + YI(5
     1                 ,6))
      D(10,4)  =  DM * (3.*CA*YI(1,6) + VA*YI(4,7) + 3.*V*YI(2,6) +
     1                 YI(5,7))
      D(5,5)   =  DM * (CA2*YI(1,9) + 2.*VA*YI(2,9) + YI(3,9))  +
     1            DB * 16.*(9.*C*YI(1,5) + 6.*V*YI(4,6) + YI(6,7))
      D(6,5)   =  DM * (CA2*YI(1,10) + 2.*VA*YI(2,10) + YI(3,10)) +
     1            DB * 20.*(12.*C*YI(1,6) + 7.*V*YI(4,7)  + YI(6,8))
      D (7,5)  =  DM * (VA*YI(4,5) + YI(5,5))
      D(8,5) = DM * (CA*YI(1,5) + VA*YI(4,6) + V*YI(2,5) + YI(5,6))
      D(9,5)   =  DM * (2.*CA*YI(1,6) + VA*YI(4,7) + 2.*V*YI(2,6) + YI(5
     1                 ,7))
      D(10,5)  =  DM * (3.*CA*YI(1,7) + VA*YI(4,8) + 3.*V*YI(2,7) + YI(5
     1                 ,8))
      D(6,6)   =  DM * (CA2*YI(1,11) + 2.*VA*YI(2,11) + YI(3,11))  +
     1            DB * 25.*(16.*C*YI(1,7) + 8.*V*YI(4,8) + YI(6,9))
      D (7,6)  =  DM * (VA*YI(4,6) + YI(5,6))
      D(8,6)   =  DM * (CA*YI(1,6) + VA*YI(4,7) + V*YI(2,6) + YI(5,7))
      D(9,6)   =  DM * (2.*CA*YI(1,7) + VA*YI(4,8) + 2.*V*YI(2,7) + YI(5
     1                 ,8))
      D(10,6)  =  DM * (3.*CA*YI(1,8) + VA*YI(4,9) + 3.*V*YI(2,8) + YI(5
     1                 ,9))
      D (7,7)  =  DM * YI(6,1)
      D (8,7)  =  DM * (V*YI(4,1) + YI(6,2))
      D (9,7)  =  DM * (2.*V*YI(4,2) + YI(6,3))
      D (10,7) =  DM * (3.*V*YI(4,3) + YI(6,4))
      D(8,8)   =  DM * (C*YI(1,1) + 2.*V*YI(4,2) + YI(6,3))
      D(9,8)   =  DM * (2.*C*YI(1,2) + 3.*V*YI(4,3) + YI(6,4))
      D(10,8)  =  DM * (3.*C*YI(1,3) + 4.*V *YI(4,4)+ YI(6,5))
      D(9,9)   =  DM * (4.*C*YI(1,3) + 4.*V*YI(4,4) + YI(6,5))
      D(10,9)  =  DM * (6.*C*YI(1,4) + 5.*V*YI(4,5) + YI(6,6))
      D(10,10) =  DM * (9.*C*YI(1,5) + 6.*V*YI(4,6) + YI(6,7))
      DO 147 I=1,10
      DO 147 J=1,I
      D(J,I)  = D(I,J)
  147 CONTINUE
      RETURN
      END
