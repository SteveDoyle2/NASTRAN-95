      SUBROUTINE CONMSG (MESAGE, NWORDS, IDUMMY)
C
      INTEGER FCHAR
      REAL INCTIM, MODTIM 
      CHARACTER   CTIME*8,AHEAD*41,MCHNAM*11,MACHOS*7
C
      character (len=12) real_clock(3), time
      integer values(8)
      character(4) cvalues(8)
      equivalence(values,cvalues)
      DIMENSION MESAGE(1)
      DIMENSION ICRDAT(3)
      DIMENSION IDATE(3), ITIME(3)
C
      COMMON /CHMACH/ MCHNAM, MACHOS
      COMMON /LOGOUT/ LOUT
      COMMON /SYSTEM/ ISYSTM(175)
C
      EQUIVALENCE (ISYSTM( 15), IDATE(1)),
     *            (ISYSTM( 18), CPUSTR),
     *            (ISYSTM( 42), ICRDAT),
     *            (ISYSTM( 75), CPUTIM),
     *            (ISYSTM(151), NLLOG ),
     *            (ISYSTM(152), LOGLIN),
     *            (ISYSTM(159), LOGPAG),
     *            (ISYSTM(160), OLDCPU)
C
      DATA IDSMS,IWRTT,IAUDT,IMPYA /4HDSMS, 4HWRTT, 4HAUDT, 4HMPYA/
      DATA MODTIM /0.0/
      DATA IDASH /4H----/
C
C   ASSEMBLE PAGE HEADING
C
      AHEAD = ' '
      NCMNAM = INDEX(MCHNAM,' ') - 1
      IF (NCMNAM .LE. -1) NCMNAM = 11
      NCMOS  = INDEX(MACHOS,' ') - 1
      IF (NCMOS .LE. -1) NCMOS = 7
      FCHAR = (18 - NCMNAM - NCMOS) + 1
      AHEAD(FCHAR:FCHAR+6)='LOG OF '
      FCHAR = FCHAR + 7
      WRITE (AHEAD(FCHAR:FCHAR+1),15) ICRDAT(3)
   15 FORMAT (A2)
      FCHAR = FCHAR + 3
      AHEAD(FCHAR:41) = MCHNAM(1:NCMNAM) // ' ' // MACHOS(1:NCMOS) //
     1 ' NASTRAN JOB'
C
      IMODTM = 0
      IF (IDUMMY.EQ.111111 .OR. IDUMMY.EQ.222222)
     *    IMODTM = IDUMMY/111111
      IF (LOGLIN.LT.NLLOG.AND.LOGLIN.GT.0) GO TO 300
      IF (LOGLIN.EQ.0) WRITE (LOUT, 2000) IDATE, AHEAD
      IF (LOGLIN.EQ.0) WRITE (LOUT, 2055)
      IF (MESAGE(1).EQ.IDSMS.AND.NWORDS.EQ.1) RETURN
      IF (MESAGE(1).EQ.IWRTT.AND.NWORDS.EQ.1) RETURN
      IF (MESAGE(1).EQ.IAUDT.AND.NWORDS.EQ.1) RETURN
      IF (MESAGE(1).EQ.IMPYA.AND.NWORDS.EQ.1) RETURN
  300 CALL NASTIM (ITIME(1), ITIME(2), ITIME(3), CPUTMM)
      WRITE (CTIME,2056) ITIME
      call date_and_time(real_clock(1), time, Real_clock(3),
     1                  values)
C
 2056 FORMAT (2( I2,':'),I2)
      IF (CTIME(4:4) .EQ. ' ') CTIME(4:4) = '0'
      IF (CTIME(7:7) .EQ. ' ') CTIME(7:7) = '0'
      CPUTMM = CPUTMM + OLDCPU - CPUSTR
      INCTIM = CPUTMM - CPUTIM
      IF (CPUTIM.EQ.0.0) INCTIM = 0.0
      IF (IMODTM.EQ.1) MODTIM = 0.0
      IF (IMODTM.EQ.2) MODTIM = CPUTMM - MODTIM
      MWORDS = MIN0 (NWORDS, 15)
      IF (IMODTM.NE.2) WRITE (LOUT,2100) time(1:2),time(3:4),time(5:10),
     *                        CPUTMM, INCTIM,(MESAGE(I), I = 1, MWORDS)
      IF (IMODTM.EQ.2) WRITE (LOUT,2110) time(1:2),time(3:4),time(5:10),
     *               CPUTMM, INCTIM, MODTIM, (MESAGE(I), I = 1, MWORDS)
      call flush(4)
C
      LOGLIN = LOGLIN + 1
      CPUTIM = CPUTMM
      IF (IMODTM.EQ.1) MODTIM = CPUTMM
      RETURN
C
 2000 FORMAT (1H1,  77(1H*)/
     *        1X , 1H*,  75X, 1H*/
     *        1X , 1H*, 7X, 'DATE ', 2(I2, '/'), I2,
     *                   7X, A41, 7X, 1H*/
     *        1X , 1H*,  75X, 1H*/
     *        1X ,  77(1H*)/)
 2055 FORMAT (1X, 2X, 'WALL', 15X,
     *                'TOTAL', 7X,
     *                'INCREMENTAL', 6X,
     *                'MODULE', 14X,
     *                'MODULE/'/
     *        1X, 2X, 'CLOCK', 15X,
     *                'CPU', 12X,
     *                'CPU', 12X,
     *                'CPU', 13X,
     *                'SUBROUTINE'/
     *        1X, 2X, 'TIME', 14X,
     *                'SECONDS', 8X,
     *                'SECONDS', 8X,
     *                'SECONDS', 13X,
     *                'STATUS'//
     *        1X,  78(1H-)/)
 2100 FORMAT (1X, a2,':',a2,':',a6,
     *                 4X, F10.3,  5X, F10.3, 15X,
     *                             5X, A4, 2X, 2A4, 2X, 12A4)
 2110 FORMAT (1X, a2,':',a2,':',a6,
     *                 4X, F10.3,  5X, F10.3,  5X, F10.3,
     *                             5X, A4, 2X, 2A4, 2X, 12A4)
      END
