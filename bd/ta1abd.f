      BLOCK DATA TA1ABD
CTA1ABD
C
C     /TA1ACM/ SPECIFIES THE OPEN CORE LABELED COMMONS /ZZEMXX/
C              TO BE USED BY EACH ELEMENT TYPE IN LINK8 OVERLAY
C              TREE.
C              THE LABELED COMMONS /ZZEMXX/ ARE USED ONLY IN CDC
C              AND UNIVAC TO COMPUTE (BY EMGSOC) THE OFFSET OF THE
C              OPEN CORE BETWEEN /ZZEMXX/ AND /ZZEMGX/.
C
C              E.G. /ZZEM24/ IS ASSIGNED TO QUAD4 ELEMENT, TYPE 64
C
C
      COMMON /TA1ACM/ IG(90)
C
      DATA    IG    /
     O         1,  0,  1,  3,  4, 14, 14, 14, 14,  1,
     1         2,  2,  2,  2, 14, 14, 14, 14, 14,  2,
     2         2,  2,  2, 11,  2,  2,  2,  2, 11, 11,
     3         0,  0,  0, 12, 28, 18, 19, 20, 17, 17,
     4        17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
     5        17, 13, 21, 21, 21, 21, 21, 21, 21, 21,
     6        21, 15, 16, 24, 22, 22, 22,  0,  0, 25,
     7        26,  0, 48, 48, 48, 48, 48, 48, 48,  8,
     8        23, 13, 27, 29, 29, 29,  0,  0,  0,  0/
C
      END
