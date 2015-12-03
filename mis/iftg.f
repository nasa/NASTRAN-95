      SUBROUTINE IFTG(THA,RP,CP)
      CALL IFTE2(THA,R,C)
      CALL IFTE4(THA,R1,C1)
      RP = 2.*R - R1
      CP = 2.*C - C1
      RETURN
      END
