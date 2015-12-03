        SUBROUTINE GETURN ( NAMFIL )                                            
      INCLUDE 'DSIOF.COM'                                                       
        COMMON / XFIST  / IFSTMX, IFSTCA, IFIST( 100 )                          
        COMMON / XFIAT  / IFATUF, IFATMX, IFATCA, IFIAT( 640 )                  
        COMMON / XXFIAT / IXFIAT( 19 )                                          
        INTEGER*2         IUNIT                                                 
        COMMON / DSUNIT / IUNIT( 220 )                                          
        DATA  MASK / '00007FFF'X /                                             
        IF ( NAMFIL .EQ. LASNAM .AND. IFILEX .NE. 0 ) GO TO 20                  
        IFILEX = 0                                                              
        LIM = 2 * IFSTCA - 1                                                    
        DO 15 IFST = 1, LIM, 2                                                  
        IF ( NAMFIL .NE. IFIST( IFST ) ) GO TO 15                               
        IF ( NAMFIL .GE. 101 .AND. NAMFIL .LE. 320 ) GO TO 10                   
        IF ( IFIST( IFST + 1 ) .GT. 0 ) GO TO 5                                 
        IFILEX = IXFIAT( IABS( IFIST( IFST+1 ) ) + 1 )                          
        IF (IFILEX .LE. MAXPRI) GO TO 20                                        
   2    IFILEX = 0                                                              
        GO TO 200                                                               
   5    IFILEX = IAND( IFIAT( IFIST( IFST+1 ) - 2 ), MASK )                     
        GO TO 20                                                                
   10   IFILEX = IAND( IFIAT( IFIST( IFST+1 ) - 2 ), MASK )                     
        IF (IFILEX .GT. MAXPRI) GO TO 2                                         
        IUNIT( NAMFIL-100 ) = IFILEX                                            
        GO TO 20                                                                
   15   CONTINUE                                                                
        GO TO 200                                                               
   20   IPRVOP = FCB( 1, IFILEX )                                               
        IF ( IPRVOP .EQ. 2 ) IPRVOP = 0                                         
        NLR    = FCB( 3, IFILEX )                                               
        NBLOCK = FCB( 4, IFILEX )                                               
        LASNAM = NAMFIL                                                         
  200   CONTINUE                                                                
        RETURN                                                                  
        END                                                                     
