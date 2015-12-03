      PARAMETER  ( NUMFCB=89, NUMSOF=10 )
      COMMON / GINOX / LGINOX, IDSLIM, MDSFCB( 3,NUMFCB ), 
     &                 LENSOF( NUMSOF )  
      
