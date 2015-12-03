      SUBROUTINE LOGLOG (A,B,C,D,E,F)
C
C     WRITTEN BY G.CHAN/UNISYS  7/92, THE 1992 SUMMER OLYMPIC WEEK
C
C     LOG-LOG TABLE LOOKUP           10 +------+------+------+--+
C                                     D                 *
C     INPUT : A,B, C,D, AND E         8 +------+-------/-----+--+
C     OUTPUT: F                                       /
C                                                    /
C     ALL A,B,C,D,E,F IN LOG          4 +------+----/-+------+--+
C     SCALE                           F            *
C                                                 /
C     LINEAR EVALUATION ON LOG        2 +------+-/-----+------+--+
C     SCALE (NO POLYNOMIAL            B         *
C     EVALUATION)
C                                     1 +------+------+------+--+
C                                       1      2A  E  4 C    8  10
      AA = ALOG10(A)
      BB = ALOG10(B)
      CC = ALOG10(C) - AA
      DD = ALOG10(D) - BB
      EE = ALOG10(E) - AA
      F  = 10.**(EE*DD/CC + BB)
      RETURN
C
C
      ENTRY SMILOG (A,B,C,D,E,F)
C     ==========================
C
C     SEMI-LOG TABLE LOOKUP       10 +--+--+--+--+--+--+--+--+--+
C                                  D                *
C     INPUT : A,B, C,D, AND E      8 +--+--+--+--+-/+--+--+--+--+
C     OUTPUT: F                                   /
C                                                /
C     A,C,E IN LINEAR SCALE        4 +--+--+--+-/+--+--+--+--+--+
C     B,D,F IN LOG SCALE           F           *
C                                             /
C                                  2 +--+--+-/+--+--+--+--+--+--+
C                                  B        *
C
C                                  1 +--+--+--+--+--+--+--+--+--+
C                                    0  1  2A 3E 4  C  6  7  8  9
      BB = ALOG10(B)
      CC = C - A
      DD = ALOG10(D) - BB
      EE = E - A
      F  = 10.**(EE*DD/CC + BB)
      RETURN
C
C
      ENTRY LOGSMI (A,B,C,D,E,F)
C     ==========================
C
C     LOG-SEMI TABLE LOOKUP          10 +-----+-----+-----+--+
C                                     D                 *
C     INPUT:  A,B, C,D, AND E         8 +-----+-----+--/--+--+
C     OUTPUT: F                                       /
C                                     6 +-----+-----+/----+--+
C     A,C,E IN LOG SCALE                            /
C     B,D,F IN LINEAR SCALE           4 +-----+----/+-----+--+
C                                     F           *
C                                     2 +-----+--/--+-----+--+
C                                     B         *
C                                     0 +-----+-----+-----+--+
C                                       1     2 A E 4   C 8  10
      AA = ALOG10(A)
      CC = ALOG10(C) - AA
      DD = D - B
      EE = ALOG10(E) - AA
      F  = EE*DD/CC + B
      RETURN
      END
