
C +   ===================
C +   MARdim for ETH-camp
C +   ===================

      integer   mx    ,my  ,ip11  ,jp11
      parameter(mx= 1,my= 1,ip11=1,jp11=1)
      integer   mz   ,mzir1     ,mzir
      parameter(mz=35,mzir1=mz+1,mzir=mz+2)
c ... mzir1 may be chosen much larger than mz, 
c     if the model vertical domain covers a small part of the air column

      integer   mx1     ,mx2
      parameter(mx1=1,mx2=1)
      integer   my1     ,my2     ,myd2
      parameter(my1=1,my2=1,myd2=1+my/2)
      integer   mz1     ,mzz
      parameter(mz1=mz-1,mzz=mz+1)
      integer   i_2
      parameter(i_2=mx-mx1+1) 
      integer   j_2
      parameter(j_2=my-my1+1) 
      integer   mzabso      ,mzhyd
      parameter(mzabso =   1,mzhyd=mzabso+1)

      integer   klon,        klev
      parameter(klon=1,klev=mz)
C +...if #NV removed (NO vectorization)
C +   then      klon=      1

      integer   kdlon,       kflev
      parameter(kdlon=klon  ,kflev=klev)


C +   MAR_SL.inc
C +   ==========

      integer   mw                                                              
      parameter(mw=1)  

C +   MAR_SN.inc
C +   ==========

c     integer   mg                                                              
c     parameter(mg=35)  


C +   MAR_LB.inc
C +   ==========

      integer   n6  ,n7
      parameter(n6=6,n7=7)

