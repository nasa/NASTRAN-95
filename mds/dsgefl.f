        SUBROUTINE DSGEFL                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER*2         IUNIT                                                 
        COMMON / DSUNIT / IUNIT(220)                                            
        IF ( NAME .GE. 101 .AND. NAME .LE. 320 ) GO TO 10                       
        CALL GETURN ( NAME )                                                    
        GO TO 20                                                                
   10   IFILEX = IUNIT( NAME-100 )                                              
   20   IF ( IFILEX .NE. 0 ) GO TO 30                                           
        IF ( IRETRN .EQ. 77 ) GO TO 50                                          
        CALL DSMSG ( 107 )                                                      
   30   IOBUF =  FCB( 2, IFILEX )                                               
        IF ( IOBUF .EQ. 0 ) GO TO 40                                            
        IPRVOP = FCB( 1,IFILEX )                                                
        IF ( IPRVOP .EQ. 2 ) IPRVOP = 0                                         
        INDBAS = IOBUF                                                          
        INDCBP = INDBAS + IBASE( INDBAS+1 ) - 1                                 
        INDCLR = INDBAS + IBASE( INDBAS+2 ) - 1                                 
        NBLOCK = FCB( 4, IFILEX )                                               
        LCW    = IBASE( INDBAS+4 )                                              
        LASNAM = NAME                                                           
        GO TO 50                                                                
   40   IFILEX = 0                                                              
   50   CONTINUE                                                                
        RETURN                                                                  
        END                                                                     
