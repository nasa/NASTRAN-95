      SUBROUTINE FEERDD
C*******
C
C     SUBROUTINE TO INITIALIZE COMMON /FEERCX/
C
C*******
      INTEGER            JFRCX(28)
      INTEGER            KFRCX( 4)
      INTEGER            LFRCX( 4)
C
      COMMON   /FEERCX/  IFRCX(37)
C
      DATA               JFRCX   /
     1                   101,6*0   ,102,6*0  ,201,6*0  ,202,6*0  /
      DATA               KFRCX   /
     1                   301       ,302      ,303      ,304      /
      DATA               LFRCX   /
     1                   305       ,306      ,307      ,308      /
      DATA               MFRCX   /   204   /
C
      DO 10 I = 1,28
   10 IFRCX(I) = JFRCX(I)
      DO 20 I = 1,4
      IFRCX(I+28) = KFRCX(I)
   20 IFRCX(I+32) = LFRCX(I)
      IFRCX(37) = MFRCX
C
      RETURN
      END
