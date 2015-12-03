      BLOCK DATA OFP5BD
COFP5BD
      INTEGER ESINGL, E1, E21, E41, E61, E81
      COMMON /OFPBD5/ ESINGL(64),E1(100),E21(100),E41(100),E61(100)
     1               ,E81(100)
C*****
C     SPACING ARRAY - ESINGL
C*****
      DATA    ESINGL( 1) / 4H/    /
      DATA    ESINGL( 2) / 4H15X  /
      DATA    ESINGL( 3) / 4H10X  /
      DATA    ESINGL( 4) / 4H5X   /
      DATA    ESINGL( 5) / 4H1X   /
      DATA    ESINGL( 6) / 4H/10X /
      DATA    ESINGL( 7) / 4H16X  /
      DATA    ESINGL( 8) / 4H2H1  /
      DATA    ESINGL( 9) / 4H2H2  /
      DATA    ESINGL(10) / 4H2H3  /
      DATA    ESINGL(11) / 4H2H4  /
      DATA    ESINGL(12) / 4H2H5  /
      DATA    ESINGL(13) / 4H7X   /
      DATA    ESINGL(14) / 4H/16X /
      DATA    ESINGL(15) / 4H/13X /
      DATA    ESINGL(16) / 4H4X   /
      DATA    ESINGL(17) / 4H/14X /
      DATA    ESINGL(18) / 4H11X  /
      DATA    ESINGL(19) / 4H/24X /
      DATA    ESINGL(20) / 4H1H0  /
      DATA    ESINGL(21) / 4H2H / /
      DATA    ESINGL(22) / 4H2HEN /
      DATA    ESINGL(23) / 4H2HDA /
      DATA    ESINGL(24) / 4H2HDB /
      DATA    ESINGL(25) / 4H/1H0 /
      DATA    ESINGL(26) / 4H23X  /
      DATA    ESINGL(27) / 4H/26X /
      DATA    ESINGL(28) / 4H/9X  /
      DATA    ESINGL(29) / 4H/12X /
      DATA    ESINGL(30) / 4H/1H  /
      DATA    ESINGL(31) / 4H/20X /
      DATA    ESINGL(32) / 4H/32X /
      DATA    ESINGL(33) / 4H/28X /
      DATA    ESINGL(34) / 4H/15X /
      DATA    ESINGL(35) / 4H/19X /
      DATA    ESINGL(36) / 4H/21X /
      DATA    ESINGL(37) / 4H/11X /
      DATA    ESINGL(38) / 4H/17X /
      DATA    ESINGL(39) / 4H2X   /
      DATA    ESINGL(40) / 4H1HX  /
      DATA    ESINGL(41) / 4H2HXY /
      DATA    ESINGL(42) / 4H1HA  /
      DATA    ESINGL(43) / 4H2HLX /
      DATA    ESINGL(44) / 4H1HY  /
      DATA    ESINGL(45) / 4H2HYZ /
      DATA    ESINGL(46) / 4H1HB  /
      DATA    ESINGL(47) / 4H2HLY /
      DATA    ESINGL(48) / 4H1HZ  /
      DATA    ESINGL(49) / 4H2HZX /
      DATA    ESINGL(50) / 4H1HC  /
      DATA    ESINGL(51) / 4H2HLZ /
      DATA    ESINGL(52) / 4H2HCP /
      DATA    ESINGL(53) / 4H2HMP /
      DATA    ESINGL(54) / 4H2HC  /
      DATA    ESINGL(55) / 4H3X   /
      DATA    ESINGL(56) / 4H/30X /
      DATA    ESINGL(57) / 4H9X   /
      DATA    ESINGL(58) / 4H/23X /
      DATA    ESINGL(59) / 4H6X   /
      DATA    ESINGL(60) / 4H39X  /
      DATA    ESINGL(61) / 4H24X  /
      DATA    ESINGL(62) / 4H     /
      DATA    ESINGL(63) / 4H     /
      DATA    ESINGL(64) / 4H     /
C
C
C     FORMAT BUILDING BLOCK E-ARRAY
C
C
C                    -STANDARD-                 -ALTERNATES-
C                 ****************        ***********************
      DATA    E1  / 4H1P,E ,4H15.6        , 4H0P,F ,4H6.1  ,4H,9X
     2            , 4H1P,E ,4H16.6        , 4H0P,F ,4H7.1  ,4H,9X
     3            , 4H1P,E ,4H17.6        , 4H0P,F ,4H8.1  ,4H,9X
     4            , 4H1P,E ,4H18.6        , 4H0P,F ,4H9.1  ,4H,9X
     5            , 4H1P,E ,4H19.6        , 4H0P,F ,4H10.1 ,4H,9X
     6            , 4H1P,E ,4H20.6        , 4H0P,F ,4H11.1 ,4H,9X
     7            , 4H1P,E ,4H21.6        , 4H0P,F ,4H12.1 ,4H,9X
     8            , 4H1P,E ,4H30.6        , 4H0P,F ,4H21.1 ,4H,9X
     9            , 4H1P,E ,4H26.6        , 4H0P,F ,4H17.1 ,4H,9X
     O            , 4H1P,E ,4H24.6        , 4H0P,F ,4H15.1 ,4H,9X
     1            , 4H0P,F ,4H11.4        , 4H0P,F ,4H8.1  ,4H,3X
     2            , 4H0P,F ,4H14.4        , 4H0P,F ,4H11.1 ,4H,3X
     3            , 4H1P,E ,4H28.6        , 4H0P,F ,4H19.1 ,4H,9X
     4            , 4H1P,E ,4H37.6        , 4H0P,F ,4H28.1 ,4H,9X
     5            , 4H1P,E ,4H22.6        , 4H0P,F ,4H17.1 ,4H,5X
     6            , 4H1P,E ,4H14.6        , 4H0P,F ,4H5.1  ,4H,9X
     7            , 4H0P,F ,4H15.4        , 4H0P,F ,4H12.1 ,4H,3X
     8            , 4H0P,F ,4H9.4         , 4H0P,F ,4H6.1  ,4H,3X
     9            , 4H0P,F ,4H15.3        , 4H0P,F ,4H12.1 ,4H 3X
     O            , 4H1P,E ,4H23.6        , 4H0P,F ,4H14.1 ,4H,9X    /
      DATA    E21 / 4H1P,E ,4H35.6        , 4H0P,F ,4H26.1 ,4H,9X
     2            , 4H1P,E ,4H25.6        , 4H0P,F ,4H16.1 ,4H,9X
     3            , 4H1P,E ,4H50.6        , 4H0P,F ,4H41.1 ,4H,9X
     4            , 4H0P,F ,4H46.4        , 4H0P,F ,4H43.1 ,4H,3X
     5            , 4H     ,4H            , 4H0P,F ,4H12.1 ,4H,3X
     6            , 4H0P,F ,4H20.4        , 4H0P,F ,4H17.1 ,4H,3X
     7            , 4H0P,F ,4H16.4        , 4H0P,F ,4H13.1 ,4H,3X
     8            , 4H0P,F ,4H22.4        , 4H0P,F ,4H19.1 ,4H,3X
     9            , 4H1P,E ,4H27.6        , 4H0P,F ,4H18.1 ,4H,9X
     O            , 4H0P,F ,4H12.5        , 4H0P,F ,4H11.1 ,4H,3X
     1            , 4H1P,E ,4H13.5        , 4H0P,F ,4H5.1  ,4H,8X
     2            , 4H0P,F ,4H13.3        , 4H0P,F ,4H9.1  ,4H,4X
     3            , 4H0P,F ,4H18.4        , 4H0P,F ,4H15.1 ,4H,3X
     4            , 4H0P,F ,4H26.4        , 4H0P,F ,4H23.1 ,4H,3X
     5            , 4H1P,E ,4H14.5        , 4H0P,F ,4H6.1  ,4H,8X
     6            , 4H0P,F ,4H14.3        , 4H0P,F ,4H10.1 ,4H,4X
     7            , 4H0P,F ,4H5.2         , 4H0P,F ,4H4.1  ,4H,1X
     8            , 4H1P,E ,4H13.6        , 4H0P,F ,4H4.1  ,4H,9X
     9            , 4H     ,4H            , 4H     ,4H     ,4H
     O            , 4H1P,E ,4H9.1         , 4HA1   ,4H,8X  ,4H      /
      DATA    E41 / 4H6X,A ,4H1,3X        , 4HI7   ,4H,3X  ,4H
     2            , 4HI15  ,4H            , 4H     ,4H     ,4H
     3            , 4HI9,1 ,4HX           , 4H     ,4H     ,4H
     4            , 4H1H0, ,4HI8          , 4H     ,4H     ,4H
     5            , 4H1X,I ,4H13          , 4H     ,4H     ,4H
     6            , 4H1X,I ,4H8           , 4H     ,4H     ,4H
     7            , 4H1H0, ,4HI7          , 4H     ,4H     ,4H
     8            , 4H6X,I ,4H8           , 4H     ,4H     ,4H
     9            , 4H1X,I ,4H15          , 4H     ,4H     ,4H
     O            , 4H1X,I ,4H12          , 4H     ,4H     ,4H
     1            , 4HI10  ,4H            , 4H     ,4H     ,4H
     2            , 4HI7,1 ,4HX           , 4H     ,4H     ,4H
     3            , 4H3X,A ,4H4           , 4H     ,4H     ,4H
     4            , 4H1H0, ,4HI13         , 4H     ,4H     ,4H
     5            , 4H1X,I ,4H20          , 4H     ,4H     ,4H
     6            , 4H5X,A ,4H1,3X        , 4HI5   ,4H,4X  ,4H
     7            , 4H1X,I ,4H22          , 4H     ,4H     ,4H
     8            , 4HI12  ,4H            , 4H     ,4H     ,4H
     9            , 4H1X,I ,4H19          , 4H     ,4H     ,4H
     O            , 4HI16  ,4H            , 4H     ,4H     ,4H      /
      DATA    E61 / 4HI8   ,4H            , 4HA4   ,4H,4X  ,4H
     2            , 4HI9   ,4H            , 4HA4   ,4H,5X  ,4H
     3            , 4HI11  ,4H            , 4HA4   ,4H,7X  ,4H
     4            , 4HI20  ,4H            , 4HA4   ,4H,16X ,4H
     5            , 4HI19  ,4H            , 4HA4   ,4H,15X ,4H
     6            , 4H1X,I ,4H23          , 4H     ,4H     ,4H
     7            , 4HI23  ,4H            , 4H     ,4H     ,4H
     8            , 4HI28  ,4H            , 4H     ,4H     ,4H
     9            , 4H/1H  ,4H,I18        , 4H     ,4H     ,4H
     O            , 4H1H0, ,4HI15         , 4H     ,4H     ,4H
     1            , 4H1H0, ,4HI14         , 4H     ,4H     ,4H
     2            , 4H0P,F ,4H22.4        , 4HI9   ,4H,13X ,4H
     3            , 4H0P,F ,4H16.4        , 4HI5   ,4H,11X ,4H
     4            , 4H0P,F ,4H10.4        , 4H     ,4H     ,4H
     5            , 4H1H0, ,4HI19         , 4H     ,4H     ,4H
     6            , 4H1H0, ,4HI20         , 4H     ,4H     ,4H
     7            , 4HI10, ,4H5X          , 4H     ,4H     ,4H
     8            , 4H     ,4H            , 4H     ,4H     ,4H
     9            , 4HI8,  ,4H2X          , 4H3X,3 ,4HHCEN ,4H,A4
     O            , 4HF8.3 ,4H            , 4H     ,4H     ,4H      /
      DATA    E81 / 4H1H0, ,4HI27         , 4H     ,4H     ,4H
     2            , 4H1H0, ,4HI5          , 4H     ,4H     ,4H
     3            , 4H1H0, ,4HI3          , 4H     ,4H     ,4H
     4            , 4HI4   ,4H            , 4H     ,4H     ,4H
     5            , 4H1P,E ,4H11.4        , 4H0P,F ,4H4.1  ,4H,7X
     6            , 4HA4   ,4H            , 4H     ,4H     ,4H
     7            , 4H1P9E ,4H11.3        , 4H0P9( ,4HF9.3 ,4H,2X)
CAIX 7            , 4H  9E ,4H11.3        , 4H  9( ,4HF9.3 ,4H,2X)
     8            , 4H0P,F ,4H22.3        , 4H0P,F ,4H20.1 ,4H,2X
     9            , 4H/1PE ,4H11.3        , 4H/0PF ,4H7.1  ,4H,4X
CAIX 9            , 4H/, E ,4H11.3        , 4H/0P, ,4HF7.1 ,4H,4X
     O            , 4H0P,F ,4H19.4        , 4HI6   ,4H,13X ,4H
     1            , 4HF8.2 ,4H            , 4H     ,4H     ,4H
     2            , 4H1P,E ,4H12.5        , 4H     ,4H     ,4H
     3            , 4H1H0, ,4HI12         , 4H     ,4H     ,4H
     4            , 4H4X,I ,4H8           , 4H4X,A ,4H4,4X ,4H
     4            , 4H     ,4H            , 4H     ,4H     ,4H
     5            , 4H     ,4H            , 4H     ,4H     ,4H
     6            , 4H     ,4H            , 4H     ,4H     ,4H
     7            , 4H     ,4H            , 4H     ,4H     ,4H
     8            , 4H     ,4H            , 4H     ,4H     ,4H
     O            , 4H     ,4H            , 4H     ,4H     ,4H      /
      END
