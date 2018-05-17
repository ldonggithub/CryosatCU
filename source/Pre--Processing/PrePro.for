      program PrePro
C +
C +------------------------------------------------------------------------+
C |   program PrePro   activates option lab (i.e., "#XY")                  |
C |                                                                        |
C +------------------------------------------------------------------------+
C +
      integer       nkx
      parameter    (nkx=60)
C +
      integer       nk
C +
      character*  3 lab(nkx)
      character* 15 filnam,filinp,filout
      character* 80 line
C +
      open( unit=1,status='old',file='PrePro.ctr')
      rewind     1
        read(1,1) filnam
 1      format(a15)
C +
        ic =      0
 10     CONTINUE
        ic = ic + 1
        IF (filnam(ic:ic).ne.'.') GO TO 10
            filinp            ='               '
            filout            ='               '
            filinp(   1:ic  ) = filnam(1:ic)
            filout(   1:ic  ) = filnam(1:ic)
            filinp(ic+1:ic+3) ='for'
            filout(ic+1:ic+1) ='f'
C +
        nk = 0
 20     CONTINUE
        nk = nk + 1
        IF  (nk.gt.nkx)  stop '#~@à# increase nkx'
        read(1,21,END=22) lab(nk)
 21     format(a3)
        GO TO 20
 22     CONTINUE
        nk = nk - 1
        IF  (nk.le.  1)  stop '#~@à# NO Label to preprocess'
C +
      close(unit=1)
C +
      open( unit=1,status='old',file=filinp)
      rewind     1
      open( unit=2,status='new',file=filout)
      rewind     2
C +
 100  CONTINUE
        read( 1,102,END=101) line
 102    format(a80)
        DO k=1,nk
          IF (line(1:2).eq.'c ' .AND. line(3:5).eq.lab(k))          THEN
              line(1:5) =  '     '
          END IF
        END DO
        write(2,102        ) line
      GO TO 100
 101  CONTINUE
C +
      close(unit=1)
      close(unit=2)
C +
      stop
      end
