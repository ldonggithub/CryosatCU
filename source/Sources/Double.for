      program Double
C +
C +------------------------------------------------------------------------+
C |   program Double   activates Double Precision: real --> real*8         |
C |                                                                        |
C |                                                                        |
C |                                                                        |
C |                                                                        |
C +------------------------------------------------------------------------+
C +
      IMPLICIT      NONE
C +
      integer       ic    ,il
      character* 15 filnam,filinp,filout
      character* 80 line
C +
      open( unit=1,status='old',file='Double.ctr')
      rewind     1
        read(1,1) filnam
 1      format(a15)
C +
        ic =      0
 10     CONTINUE
        ic = ic + 1
        IF      (filnam(ic:ic).ne.'.')                          GO TO 10
                 filinp            =  '               '
                 filout            =  '               '
                 filinp(   1:ic  ) =   filnam(1:ic)
                 filout(   1:ic  ) =   filnam(1:ic)
        IF      (filnam(ic+1:ic+3).eq.'for')                        THEN
                 filinp(ic+1:ic+3) =  'for'
                 filout(ic+1:ic+1) =  'f'
        ELSE IF (filnam(ic+1:ic+3).eq.'INC')                        THEN
                 filinp(ic+1:ic+3) =  'INC'
                 filout(ic+1:ic+3) =  'inc'
        ELSE
            stop 'ERROR: File Name Second Label not consistant'
        END IF
C +
      close(unit=1)
C +
      open( unit=1,status='old',file=filinp)
      rewind     1
      open( unit=2,status='new',file=filout)
      rewind     2
C +
      il = 0
 100  CONTINUE
      il = il + 1
        read( 1,102,END=101) line
 102    format(a80)
C +
C +     **********
        call spdou(line,il)
C +     **********
C +
        write(2,102        ) line
      GO TO 100
 101  CONTINUE
C +
      close(unit=1)
      close(unit=2)
C +
      stop
      end
      subroutine spdou(line,il)
C +
C +------------------------------------------------------------------------+
C |   subroutine spdou modifies statements  real into real*8               |
C |                                         Real into Real*8               |
C |                                                                        |
C +------------------------------------------------------------------------+
C +
      implicit     none
      character*80 line
      integer      il,ic
C +
      IF     (line(1:6).eq.'      ')                              THEN ! CTR
        ic = 6
 1000   CONTINUE
        ic = ic+1
        IF   (line(ic:ic  ).eq.' '        )                 GO TO 1000
        IF   (line(ic:ic+3).ne.'real'.AND.
     .        line(ic:ic+3).ne.'Real')                      GO TO 1020
c #WR     write(6,6000) line
 6000     format(a80)
          IF (line(ic+4:ic+6).ne.'   ')                           THEN
            write(6,1060) il,line
 1060       format(' line',i6,' is not correctly programmed',
     .                        ' for preprocessing double precision',
     .           /,  12x, a80)
          ELSE
              line(ic+4:ic+5) = '*8'
          END IF
c #WR     write(6,6000) line
 1020   CONTINUE
      END IF                                                           ! CTR
C +
      return
      end
