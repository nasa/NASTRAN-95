      FUNCTION IAPD(I,J,NC,NCRD)
      IF(J.NE.1) GO TO 10
      IAPD=NCRD+1
      IF(I.EQ.1) RETURN
      IAPD=IAPD+1
      IF(I.EQ.2) RETURN
      IAPD=3+3*(I-2)+NCRD
      RETURN
   10 IF(J.NE.2) GO TO 20
      IAPD=3+NCRD
      IF(I.EQ.1) RETURN
      IAPD=4+NCRD
      IF(I.EQ.2) RETURN
      IAPD=4+3*(I-2)+NCRD
      RETURN
   20 IAPD=J+NC*(2*J-3)+NCRD
      IF(I.EQ.1) RETURN
      IAPD=IAPD+1
      IF(I.EQ.2) RETURN
      IAPD=IAPD+2*(I-2)
      RETURN
      END
