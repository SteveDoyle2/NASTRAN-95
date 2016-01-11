C
C     Simple text parsing in Fortran 77
C     https://github.com/OdonataResearchLLC/Text77
C

C Position the file at the end for appending.
      integer function eof77 (iunit)
      implicit none
      integer iunit, ios
      common /txterr/ ios
      ios = 0
      do while (ios.EQ.0)
         read (iunit,'(A1)',iostat=ios)
      end do
      if (ios.LT.0) backspace (iunit,iostat=ios)
      eof77 = ios
      return
      end

C  Return the beginning position of the string
      integer function begpos (string)
      implicit none
      integer endpos
      character*(*) string
      begpos = 1
      endpos = len(string)
      do while
     & ((ichar(string(begpos:begpos)).EQ.32).AND.(begpos.LT.endpos))
         begpos = begpos + 1
      end do
      if (begpos.EQ.endpos) begpos = 0
      return
      end

C  Return the ending position of the string
      integer function endpos (string)
      implicit none
      character*(*) string
      endpos = len(string)
      do while
     & ((ichar(string(endpos:endpos)).EQ.32).AND.(endpos.GT.0))
         endpos = endpos - 1
      end do
      return
      end

C  Trim spaces from the beginning of the string.
      subroutine trim77 (string)
      implicit none
      character*(*) string
      integer begpos
      string = string(begpos(string):)
      return
      end

C  Split the string at the first delimiter.
C  Input
C     string : delimited string
C     delmtr : characters delimiting a field
C     greedy : logical indicating greedy delimiter
C  Output
C      field : first delimited field in string
C     string : string with first field removed
      subroutine split (field, string, delmtr, greedy)
      implicit none
C  Argument variables
      character*(*) field, string, delmtr
      logical greedy
C  Local variables
      logical skip
      integer endstr, enddel, delpos
C  External functions
      integer endpos
C  Initialization
      skip = greedy
      endstr = endpos(string)
      enddel = max(1,endpos(delmtr))
      delpos = index(string,delmtr(1:enddel))
C  SPACE delimiter at the end of the field.
      if (delpos.GT.endstr) then
         field = string(1:endstr)
         string = char(32)
C  Standard field
      else if (delpos.GT.1) then
         field = string(1:delpos-1)
         string = string(delpos+enddel:max(delpos+enddel,endstr))
C     Consume repeated delimiters
         do while (skip)
            endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
            else
               skip = .FALSE.
            end if
         end do
C  Empty field
      else if (delpos.EQ.1) then
         field = char(32)
         string = string(enddel+1:)
         do while (skip)
            endstr = endpos(string)
            delpos = index(string,delmtr(1:enddel))
            if ((delpos.EQ.1).AND.(delpos.LT.endstr)) then
               string = string(delpos+enddel:max(delpos+enddel,endstr))
            else
               skip = .FALSE.
            end if
         end do
C  No delimiter
      else if (delpos.EQ.0) then
         field = string
         string = char(32)
      end if
      return
      end

C  Return true if the field contains an integer.
      logical function is_int (field)
      implicit none
      character*(*) field
      integer ios, scratch
      character*11 buffer
      common /txterr/ ios
      if (len(field).LT.11) then
         buffer = field
         read (buffer,'(BN,I11)',iostat=ios) scratch
      else
         read (field,'(BN,I11)',iostat=ios) scratch
      end if
      is_int = ios.EQ.0
      return
      end

C  Return an integer from the field
      integer function atoi (field)
      implicit none
      character*(*) field
      integer ios
      character*11 buffer
      common /txterr/ ios
      if (len(field).LT.11) then
         buffer = field
         read (buffer,'(BN,I11)',iostat=ios) atoi
      else
         read (field,'(BN,I11)',iostat=ios) atoi
      end if
      return
      end

C  Return true if the field contains a real.
      logical function isreal (field)
      implicit none
      character*(*) field
      integer ios
      real scratch
      character*16 buffer
      common /txterr/ ios
      if (len(field).LT.16) then
         buffer = field
         read (buffer,'(BN,F16.0)',iostat=ios) scratch
      else
         read (field,'(BN,F16.0)',iostat=ios) scratch
      end if
      isreal = ios.EQ.0
      return
      end

C  Return a real from the field
      real function ator (field)
      implicit none
      character*(*) field
      integer ios
      character*16 buffer
      common /txterr/ ios
      if (len(field).LT.16) then
         buffer = field
         read (buffer,'(BN,F16.0)',iostat=ios) ator
      else
         read (field,'(BN,F16.0)',iostat=ios) ator
      end if
      return
      end

C  Return true if the field contains a double precision.
      logical function is_dbl (field)
      implicit none
      character*(*) field
      integer ios
      double precision scratch
      character*32 buffer
      common /txterr/ ios
      if (len(field).LT.32) then
         buffer = field
         read (buffer,'(BN,F32.0)',iostat=ios) scratch
      else
         read (field,'(BN,F32.0)',iostat=ios) scratch
      end if
      is_dbl = ios.EQ.0
      return
      end

C  Return a double precision from the field
      double precision function atod (field)
      implicit none
      character*(*) field
      integer ios
      character*32 buffer
      common /txterr/ ios
      if (len(field).LT.32) then
         buffer = field
         read (buffer,'(BN,F32.0)',iostat=ios) atod
      else
         read (field,'(BN,F32.0)',iostat=ios) atod
      end if
      return
      end

C  Return the status of the last text function
      integer function txtios ()
      implicit none
      integer ios
      common /txterr/ ios
      txtios = ios
      return
      end

C  Print the ASCII collating sequence table to standard output
      subroutine ascii ()
      implicit none
      integer i, j, code
      code(i,j) = 32 + 8*i + j
      print *
      do i = 0, 11
         print 10, (code(i,j), char(code(i,j)), j = 0, 7)
      end do
      print *
   10 format (8(1x,I3,':',A1))
      end
