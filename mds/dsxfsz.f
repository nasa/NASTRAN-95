      SUBROUTINE DSXFSZ                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
      COMMON / XFIST / LFIST, NFIST, IFIST( 100 )                               
      COMMON / XFIAT / IFIAT( 643 )                                             
      COMMON / ZZZZZZ/ MEM(2)                                                   
      IDSN   = IFILEX                                                           
      NUN    = 0                                                                
      ITOTAL = 0                                                                
10    LASBLK = FCB( 6,IDSN )                                                    
      IFRBLK = FCB( 5,IDSN )                                                    
      NUMBLK = LASBLK - IFRBLK + 1                                              
      IF ( IDSN .EQ. IFILEX ) GO TO 20                                          
      NUN    = NUN + 1                                                          
      ITOTAL = ITOTAL + NUMBLK                                                  
      GO TO 40                                                                  
20    IPBLKS = NUMBLK                                                           
      IF ( FCB( 10, IFILEX ) .EQ. 0 ) GO TO 40                                  
      INDEX  = FCB( 10, IFILEX )                                                
      LBLOCK = MEM( INDEX+3 )                                                   
      IPBLKS = IPBLKS + LBLOCK                                                  
40    IDSN   = IAND( MDSFCB( 3,IDSN ), MASKH2 )                                 
      IF ( IDSN .NE. 0 ) GO TO 10                                               
      LIM    = 2 * NFIST                                                        
      DO 50 I = 1,LIM,2                                                         
      IF ( NAME .NE. IFIST( I ) ) GO TO 50                                      
      IF ( IFIST( I+1 ) .LE. 0 ) GO TO 70                                       
      INDX   = IFIST( I+1 )                                                     
      IFIAT( INDX+7 ) = IPBLKS * 2**16 + NUN * 2**8                             
      IFIAT( INDX+8 ) = ITOTAL * 2**16                                          
      GO TO 70                                                                  
50    CONTINUE                                                                  
70    CONTINUE                                                                  
      MAXUSM = 0                                                                
      MAXUSD = 0                                                                
C ACCUMULATE TOTAL I/O USAGE STATISTICS                                         
      DO 100 I = 1, 80                                                          
      IF ( I .EQ. 7 ) GO TO 100
      ITOTL1 = 0                                                                
      ITOTL2 = 0                                                                
      IF ( FCB( 4, I ) .EQ. 0 ) GO TO 100                                       
      NEXBLK  = FCB( 10, I )                                                    
      IF ( NEXBLK .NE. 0 ) ITOTL1 = MEM( NEXBLK+3 )                             
      IF ( FCB( 5, I ) .NE. 0 )                                                 
     &   ITOTL2 = FCB( 6, I ) - FCB( 5, I ) + 1                                 
      MAXUSM = MAXUSM + ITOTL1                                                  
      MAXUSD = MAXUSD + ITOTL2                                                  
100   CONTINUE                                                                  
      IF ( MAXBLK .LT. MAXUSM ) MAXBLK = MAXUSM                                 
      IF ( MAXDSK .LT. MAXUSD ) MAXDSK = MAXUSD                                 
      RETURN                                                                    
      END                                                                     
