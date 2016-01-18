program nastran

  CHARACTER*80    VALUE
  CHARACTER*5     TMP
  INTEGER         SPERLK
  REAL            SYSTM(94)
  COMMON / LSTADD / LASTAD
  COMMON / SYSTEM / ISYSTM(94),SPERLK
  COMMON / SOFDSN / SDSN(10)
  COMMON / LOGOUT / LOUT
  COMMON / RESDIC / IRDICT, IROPEN
  COMMON / ZZZZZZ / IZ(14000000)
  COMMON / DBM    / &
       IDBBAS, IDBFRE, IDBDIR, INDBAS, INDCLR, INDCBP, &
       NBLOCK, LENALC, IOCODE, IFILEX, NAME,   MAXALC, &
       MAXBLK, MAXDSK, IDBLEN, IDBADR, IBASBF, INDDIR, &
       NUMOPN, NUMCLS, NUMWRI, NUMREA, LENOPC

  ! INCLUDE 'NASNAMES.COM'
  character(len=80) :: dirtry, rfdir
  character(len=80) :: input, output, log, punch
  character(len=80) :: plot, nptp, dic, optp, rdic, in12, out11
  character(len=80) :: inp1, inp2
  common /dosnam/ dirtry, rfdir, input, output, log, punch, plot, &
       nptp, dic, optp, rdic, in12, out11, inp1, inp2

  character(len=80), dimension(89) :: dsnames
  common /dsname/ dsnames

  character(len=1) :: pthsep
  common /pthblk/ pthsep
  ! End of NASNAMES.COM

  CHARACTER*80    SDSN
  EQUIVALENCE    ( ISYSTM, SYSTM )

  ! Memory namelist
  integer :: dbmem = 12000000
  integer :: ocmem =  2000000
  namelist /memory/ dbmem, ocmem

  ! Directories namelist
  character(len=72) :: rfdircty, dircty
  namelist /directories/ rfdircty, dircty

  ! I/O files namelist
  character(len=80) :: inpnm, outnm, lognm, optpnm, nptpnm, pltnm, dictnm, punchnm
  namelist /iofiles/ inpnm, outnm, lognm, optpnm, nptpnm, pltnm, dictnm, punchnm

  ! Fortran units namelist
  character(len=80) :: ftn11, ftn12, ftn13, ftn14, ftn15
  character(len=80) :: ftn16, ftn17, ftn18, ftn19, ftn20, ftn21
  namelist /funits/ ftn11, ftn12, ftn13, ftn14, ftn15, &
       ftn16, ftn17, ftn18, ftn19, ftn20, ftn21

  ! SDSN files namelist
  character(len=80) :: sof1, sof2, sof3, sof4, sof5
  character(len=80) :: sof6, sof7, sof8, sof9, sof10
  namelist /sdsnfiles/ sof1, sof2, sof3, sof4, sof5, sof6, sof7, sof8, sof9, sof10

  ! Parameters
  integer, parameter :: nmlunit = 999

  ! Local variables
  integer :: ios

  LENOPC = 14000000

  ! SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/

  ISYSTM(18) = 0
  CALL SECOND (SYSTM(18))
  CALL WALTIM (ISYSTM(32))

  ! EXECUTE NASTRAN SUPER LINK

  LEN = 80
  VALUE = ' '
  CALL BTSTRP

  ! Open the namelist file
  open (unit=nmlunit, file="nastran.nml", iostat=ios)
  select case (ios)
  case (0)
     continue
  case default
     print *, "Error opening nastran.nml"
     stop
  end select

  ! Read the memory namelist
  read (unit=nmlunit, nml=memory, iostat=ios)
  select case (ios)
  case (0)
     idblen = dbmem
     iocmem = ocmem
     if (lenopc < ocmem) then
        print *, " Largest value for open core allowed is:", lenopc
        call mesage ( -61, 0, 0 )
     end if
     if (idblen /= 0) idblen = lenopc - iocmem
     lastad = locfx(iz(iocmem))
     if (idblen /= 0) idbadr = locfx(iz(iocmem+1))
     lenopc = iocmem
  case default
     print *, "Error reading the memory namelist."
     stop
  end select

  CALL DBMINT
  LOUT   = 3
  IRDICT = 4
  SPERLK = 1
  ISYSTM(11) = 1

  ! Read the directories namelist
  read (unit=nmlunit, nml=directories, iostat=ios)
  select case (ios)
  case (0)
     rfdir = trim(rfdircty)
     dirtry = trim(dircty)
     do i = 1, 9
        write (tmp,901) i
        dsnames(i) = trim(dirtry) // pthsep // tmp
     end do
     do i = 10,89
        write (tmp,902) i
        dsnames(i) = trim(dirtry) // pthsep // tmp
     end do
  case default
     print *, "Error reading the directories namelist."
     stop
  end select
901 format('scr',I1)
902 format('scr',I2)

  ! Read the I/O files namelist
  read (unit=nmlunit, nml=iofiles, iostat=ios)
  select case (ios)
  case (0)
     log = trim(lognm)
     optp = trim(optpnm)
     nptp = trim(nptpnm)
     plot = trim(pltnm)
     dic = trim(dictnm)
     punch = trim(punchnm)
     !
     dsnames(1) = trim(punchnm)
     dsnames(3) = trim(lognm)
     dsnames(4) = trim(dictnm)
     dsnames(5) = trim(inpnm)
     dsnames(6) = trim(outnm)
     dsnames(7) = trim(optpnm)
     dsnames(8) = trim(nptpnm)
     dsnames(10) = trim(pltnm)
  case default
     print *, "Error reading the iofiles namelist"
     stop
  end select

  ! Read the Fortran units namelist
  read (unit=nmlunit, nml=funits, iostat=ios)
  select case (ios)
  case (0)
     out11 = trim(ftn11)
     in12 = trim(ftn12)
     !
     dsnames(11) = trim(ftn11)
     dsnames(12) = trim(ftn12)
     dsnames(13) = trim(ftn13)
     dsnames(14) = trim(ftn14)
     dsnames(15) = trim(ftn15)
     dsnames(16) = trim(ftn16)
     dsnames(17) = trim(ftn17)
     dsnames(18) = trim(ftn18)
     dsnames(19) = trim(ftn19)
     dsnames(20) = trim(ftn20)
     dsnames(21) = trim(ftn21)
  case default
     print *, "Error reading the funits namelist."
     stop
  end select

  ! Read the SDSN files namelist
  read (unit=nmlunit, nml=sdsnfiles, iostat=ios)
  select case (ios)
  case (0)
     sdsn(1) = trim(sof1)
     sdsn(2) = trim(sof2)
     sdsn(3) = trim(sof3)
     sdsn(4) = trim(sof4)
     sdsn(5) = trim(sof5)
     sdsn(6) = trim(sof6)
     sdsn(7) = trim(sof7)
     sdsn(8) = trim(sof8)
     sdsn(9) = trim(sof9)
     sdsn(10) = trim(sof10)
  case default
     print *, "Error reading the sdsnfiles namelist."
     stop
  end select

  ! Open basic I/O files
  open (unit=3, file=trim(lognm), status='UNKNOWN')
  open (unit=5, file=trim(inpnm), status="OLD")
  open (unit=6, file=trim(outnm), status="NEW")

  if (punchnm(1:4) /= 'NONE') open (unit=1, file=trim(punchnm), status='UNKNOWN')
  if (dictnm(1:4) /= 'NONE') open (unit=4, file=trim(dictnm), status='UNKNOWN')
  if (pltnm(1:4) /= 'NONE') open (unit=10, file=trim(pltnm), status='UNKNOWN')
  if (ftn11(1:4) /= 'NONE') open (unit=11, file=trim(ftn11), status='UNKNOWN')
  if (ftn12(1:4) /= 'NONE') open (unit=12, file=trim(ftn12), status='UNKNOWN')

  call xsem00

  stop
end program nastran
