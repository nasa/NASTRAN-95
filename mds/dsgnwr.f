        SUBROUTINE DSGNWR                                                       
        COMMON /SYSTEM/ ISYBUF, IWR                                             
      INCLUDE 'XNSTRN.COM'                                                      
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'DSIOF.COM'                                                       
        CHARACTER*4     CBUFF(3)                                                
        EQUIVALENCE     (CBUFF,IBASE)                                           
        IDSN   = MDSFCB( 2,IFILEX )                                             
        IDSNR  = IDSN                                                           
   10   ISTRB  = FCB( 5,IDSNR  )                                                
        IF ( NBLOCK .GE. ISTRB ) GO TO 20                                       
        INEXT  = MDSFCB( 3,IDSNR  ) / MULQ2                                     
        GO TO 40                                                                
   20   IALLOC = FCB( 7, IDSNR )                                                
        IF ( NBLOCK .LE. ( IALLOC+ISTRB-1 ) ) GO TO 50                          
        IF ( IDSN .EQ. 8 ) CALL DSMSG( 9 )                                      
        INEXT  = IAND( MDSFCB( 3,IDSNR ), MASKH2 )                              
        IF ( INEXT .NE. 0 ) GO TO 40                                            
        MAXPR1 = MAXPRI + 1                                                     
        DO 30 I = MAXPR1, MAXFCB                                                
        IAVAIL = MDSFCB( 3,I )                                                  
        IF ( IAVAIL .NE. 0 ) GO TO 30                                           
        IFIRST = IALLOC + ISTRB                                                 
        IALLOC = 20000000                                                       
        FCB( 5,I ) = IFIRST                                                     
        FCB( 6,I ) = IFIRST-1                                                   
        MDSFCB( 3,I ) = IDSNR * MULQ2                                           
        INEXT  = I                                                              
        MDSFCB( 3,IDSNR ) = IOR( MDSFCB( 3,IDSNR ), I )                         
        GO TO 40                                                                
   30   CONTINUE                                                                
   40   IDSNR  = INEXT                                                          
        IF ( IDSNR .GE. 1 .AND. IDSNR .LE. MAXDSN ) GO TO 10                    
        CALL DSMSG ( 122 )                                                      
   50   IF ( IDSN .EQ. IDSNR ) GO TO 60                                         
        CALL DSCLOS( IDSN )                                                     
        MDSFCB( 1,IDSN ) = IAND( MDSFCB( 1,IDSN ), MASKH1 )                     
        IDSN   = IDSNR                                                          
        MDSFCB( 1,IDSN )   =  IOR( MDSFCB( 1,IDSN ), MASKH2 )                   
        MDSFCB( 2,IFILEX ) = IDSN                                               
        CALL DSMSG( 8 )                                                         
        IDEVIC = 0                                                              
        DO 55 KK = 1, NUMDEV                                                    
        MDSNAM(IDSN)(1:2) = DEV(KK)                                             
        ISAVE = IOP                                                             
        IOP = 0                                                                 
        CALL DSOPEN( MDSNAM( IDSN ), IDSN, IOP )                                
        IOP = ISAVE                                                             
        CBUFF( INDBAS ) = MDSNAM( IDSN )                                        
        CALL DSWRIT( IDSN, IBASE( INDBAS+3 ), NBUFF, IOBLK, ICCER )             
        IF ( ICCER .EQ. 0 ) GO TO 60                                            
        CALL DSCLOS (IDSN)                                                      
   55   CONTINUE                                                                
   57   WRITE ( IWR, 901 )                                                      
  901   FORMAT(///,' NO MORE DISK SPACE AVAILABLE, JOB ABORTED.')               
        CALL DSMSG( 122 )                                                       
   60   IOBLK  = NBLOCK - ISTRB + 1                                             
        CALL DSWRIT( IDSN, IBASE( INDBAS+3 ), NBUFF, IOBLK, ICCER )             
        IF ( ICCER .NE. 0 ) GO TO 70                                            
        LASBLK = FCB( 6,IDSN )                                                  
        IF ( LASBLK .GE. NBLOCK ) GO TO 7000                                    
        FCB( 6,IDSN ) = FCB( 6,IDSN ) + 1                                       
        GO TO 7000                                                              
   70   IF ( ICCER .NE. 28 ) CALL DSMSG( 101 )                                  
        IF ( IDSN .LE. 21 .AND. IDSN .NE. 8 .AND. IDSN .NE. 9) GO TO 80         
C ALLOW XPDT TO EXTEND (IDSN=9)---NOTE IDSN=8 IS THE NPTP                       
        ITEST = INDEX( MDSNAM(8), 'ZAP' )                                       
        IF ( IDSN .EQ. 8  .AND. ITEST .EQ. 0 ) GO TO 80                         
        FCB( 7,IFILEX ) = FCB( 6,IFILEX )                                       
        IDSNR = IDSN                                                            
        GO TO 10                                                                
   80   WRITE ( IWR, 902 )                                                      
  902   FORMAT(///,' NO MORE DISK SPACE AVAILABLE IN DEFAULT DIRECTORY',        
     &             ' FOR PERMANENT FILES',/,' JOB ABORTED')                     
        CALL DSMSG( 122 )                                                       
 7000   RETURN                                                                  
        END                                                                     
