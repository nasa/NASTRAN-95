      SUBROUTINE UMFZDD
C
C     SUBROUTINE TO INITIALIZE COMMON /UMFZZZ/ USED BY UMFEDT.
C
      INTEGER KT,TID1,TID2,PID1,NO,LL
      LOGICAL AGAIN,T1,T2,END1
      COMMON   /UMFZZZ/ AGAIN,T1,T2,KT,TID1,TID2,PID1,END1,NO,LL
C
      AGAIN = .FALSE.
      T1    = .FALSE.
      T2    = .FALSE.
      KT = 0
      TID1 = -1
      TID2 = -1
      PID1 = -1
      END1  = .FALSE.
      NO = 0
      LL = 0
C
      RETURN
      END
