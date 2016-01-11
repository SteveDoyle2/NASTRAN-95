

      character*80 dirtry, rfdir
      character*80 input, output, log, punch
      character*80 plot, nptp, dic, optp, rdic, in12, out11
      character*80 inp1, inp2
      common /dosnam/ dirtry, rfdir, input, output, log, punch, plot,
     &     nptp, dic, optp, rdic, in12, out11, inp1, inp2
C
      character*80 dsnames(89)
      common /dsname/ dsnames
