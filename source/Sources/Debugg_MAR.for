      subroutine Debugg_MAR(debugm)

C +------------------------------------------------------------------------+
C |                                                                        |
C | MAR          Debugg_MAR.f-Profil                   Thu  2-07-2009  MAR |
C |              Verification of MAR Variables                             |
C |                                                                        |
C +------------------------------------------------------------------------+


      IMPLICIT NONE


C +--General Variables
C +  =================

      include 'MARCTR.inc'
      include 'MARphy.inc'

      include 'MARdim.inc'
      include 'MARgrd.inc'
      include 'MAR_GE.inc'

      include 'MAR_RA.inc'

      include 'MAR_LB.inc'
      include 'MAR_DY.inc'
      include 'MAR_HY.inc'
      include 'MAR_CA.inc'
      include 'MAR_TE.inc'
      include 'MAR_TU.inc'
      include 'MAR_SV.inc'
      include 'MAR_SL.inc'
      include 'MAR_TV.inc'
      include 'MARsSN.inc'


C +--Local   Variables
C +  =================

      logical          DUMPOK
      real             dump_v(mx,my,mz)

      character*10     debugm

      real             qsat0D

      logical          Debugg,DebugT
      common/DebuggLOG/Debugg,DebugT

      logical          WriSum
      logical          WrThet
      logical          WrTsrf
      logical          WrFul1
      logical          WrFul2
      logical          WrFul3

      real             WVa
      real             Taq(mz),Ta2(mz),Ta1(mz),TCM(mz),CMx(mz)
      common/Debugg_CM/Taq    ,Ta2    ,Ta1    ,TCM    ,CMx
      real             Qsa(mz)

      real             Taq_ij(mx,my),Ta2_ij(mx,my),Ta1_ij(mx,my)
      real             TCM_ij(mx,my),CMx_ij(mx,my)
      common/DebugT_CM/Taq_ij       ,Ta2_ij       ,Ta1_ij    
     .                ,TCM_ij       ,CMx_ij

      integer          i_wri  ,j_wri  ,d_wri  ,d_wrs  ,l      ,n
      integer          ii     ,ii1    ,ii2                    ,iw

      real             VV_ijk ,VV_MAX ,TH_MIN ,TH_MAX ,TT_MIN ,TT_MAX
      real             QQ_MIN ,QQ_MAX ,TK_MIN ,TK_MAX ,Te_MIN ,Te_MAX
      real             pp_MIN ,pp_MAX ,zi_MIN ,zi_MAX ,HL_MIN ,HL_MAX
      real             ss_ijk ,ss_MIN ,ss_MAX         ,HS_MIN ,HS_MAX
      real             RA_MIN ,RA_MAX                 ,CM_MIN ,CM_MAX
      real             TS_MIN(mw)     ,TS_MAX(mw)
      integer          ITsMIN(mw)     ,ITsMAX(mw)
      integer          JTsMIN(mw)     ,JTsMAX(mw)
      integer          KTsMIN(mw)     ,KTsMAX(mw)
      real             TI_MIN(mw)     ,TI_MAX(mw)
      integer          ITiMIN(mw)     ,ITiMAX(mw)
      integer          JTiMIN(mw)     ,JTiMAX(mw)
      integer          KTiMIN(mw)     ,KTiMAX(mw)
      integer          ip_MAX ,jp_MAX ,ip_MIN ,jp_MIN ,iz_MAX ,jz_MAX 
      integer          iz_MIN ,jz_MIN ,iL_MAX ,jL_MAX ,iL_MIN ,jL_MIN
      integer                          iShMAX ,jShMAX ,iShMIN ,jShMIN
      integer                          iC_MAX ,jC_MAX ,iC_MIN ,jC_MIN
      integer          iV_MAX ,jV_MAX ,kV_MAX 
      integer          is_MIN ,js_MIN ,ks_MIN ,is_MAX ,js_MAX ,ks_MAX 
      integer          iH_MAX ,jH_MAX ,kH_MAX ,iH_MIN ,jH_MIN ,kH_MIN 
      integer          iT_MAX ,jT_MAX ,kT_MAX ,iT_MIN ,jT_MIN ,kT_MIN 
      integer          iQ_MAX ,jQ_MAX ,kQ_MAX ,iQ_MIN ,jQ_MIN ,kQ_MIN 
      integer          iK_MAX ,jK_MAX ,kK_MAX ,iK_MIN ,jK_MIN ,kK_MIN 
      integer          ie_MAX ,je_MAX ,ke_MAX ,ie_MIN ,je_MIN ,ke_MIN 
      integer          ir_MAX ,jr_MAX ,kr_MAX ,ir_MIN ,jr_MIN ,kr_MIN 

      data     WriSum/.false./ !  Domain               : Extrema
      data     WrThet/.false./ !  Domain               : Thetamz OUTPUT Switch
      data     WrTsrf/.false./ !  Domain               : Tsurfac OUTPUT Switch
      data     WrFul1/.true. / !  Grid Pt (i_wri,j_wri): General OUTPUT Switch
      data     WrFul2/.true. / !  Grid Pt (i_wri,j_wri): Surface OUTPUT Switch
      data     WrFul3/.true. / !  Grid Pt (i_wri,j_wri): Snow    OUTPUT Switch
      data     DUMPOK/.false./ !  DUMP                                  Switch

      data     Debugg/.false./ !  Auxiliary Variable ** PLEASE DO'NT MODIFY ** 
      data     DebugT/.false./ !  Auxiliary Variable ** PLEASE DO'NT MODIFY ** 

      data     i_wri/  1/
      data     j_wri/  1/
      data     d_wri/  0/
      data     d_wrs/  0/


!  VERIFICATION
!  ============

c #DB  if (iterun .lt. 72) GO TO 5
c #DB  if (iterun .GE.106) STOP '** STOP in Debugg_MAR iterun = 106 **'

       IF (iterun .GE.  3 .AND. WrThet)                             THEN
         DO   ii = 0       ,mx/20-1
              ii1= ii   *20+1
              ii2=(ii+1)*20
              write(36,660)   (       i                   ,i=ii1,ii2)
 660          format(/,6x,20i6)
           DO j=my,1,-1
              write(36,661) j,(pktaDY(i,j,mz)*pkDY(i,j,mz),i=ii1,ii2)
 661          format(i6,20f6.1)
           ENDDO
         ENDDO
       END IF
c #    IF (iterun .EQ. 14)                                          STOP


!  OUTPUT Statistics
!  -----------------

      IF (WriSum)                                                   THEN
       VV_MAX = 0.d0
       ss_MIN = 5.d0
       ss_MAX =-5.d0
       TH_MIN = 5.d3
       TH_MAX = 0.d0
       TT_MIN = 5.d3
       TT_MAX = 0.d0
       QQ_MIN = 5.d3
       QQ_MAX = 0.d0
       TK_MIN = 5.d3
       TK_MAX =-1.d0
       Te_MIN = 5.d3
       Te_MAX =-1.d0
       pp_MIN = 5.d3
       pp_MAX = 0.d0
       zi_MIN = 5.d3
       zi_MAX =-1.d0
       HS_MIN = 5.d6
       HS_MAX =-5.d6
       HL_MIN = 5.d6
       HL_MAX =-5.d6
       CM_MIN = 5.d6
       CM_MAX =-5.d6
       RA_MIN = 5.d6
       RA_MAX =-5.d6
       DO n=1,mw
       TS_MIN(n)=5.d6
       TS_MAX(n)=0.0
       TI_MIN(n)=5.d6
       TI_MAX(n)=0.0
       ENDDO

       DO j=jp1(1),jm1(my)
       DO i=ip1(1),im1(mx)

            Ta2_ij(i,j)=          Ta1_ij(i,j)
            Ta1_ij(i,j)=          Taq_ij(i,j)
            Taq_ij(i,j)=          pktaDY(i,j,mz)*pkDY(i,j,mz)
         IF (.NOT.DebugT)                                           THEN
                  DebugT=.true.
            Ta1_ij(i,j)=          Taq_ij(i,j)
            Ta2_ij(i,j)=          Taq_ij(i,j)
         END IF
            TCM_ij(i,j)= abs(0.5*(Ta2_ij(i,j)+Taq_ij(i,j))-Ta1_ij(i,j))
            CMx_ij(i,j)= max(     TCM_ij(i,j),CMx_ij(i,j))

        IF (pstDYn(i,j)  .gt.pp_MAX)                                THEN
            pp_MAX =         pstDYn(i,j)
            ip_MAX = i
            jp_MAX = j
        END IF

        IF ( pstDY(i,j)  .lt.pp_MIN)                                THEN
            pp_MIN =         pstDYn(i,j)
            ip_MIN = i
            jp_MIN = j
        END IF

        IF (CMx_ij(i,j)  .gt.CM_MAX)                                THEN
            CM_MAX =         CMx_ij(i,j)
            iC_MAX = i
            jC_MAX = j
        END IF

        IF (CMx_ij(i,j)  .lt.CM_MIN)                                THEN
            CM_MIN =         CMx_ij(i,j)
            iC_MIN = i
            jC_MIN = j
        END IF

        IF (zi__TE(i,j)  .gt.zi_MAX)                                THEN
            zi_MAX =         zi__TE(i,j)
            iz_MAX = i
            jz_MAX = j
        END IF

        IF (zi__TE(i,j)  .lt.zi_MIN)                                THEN
            zi_MIN =         zi__TE(i,j)
            iz_MIN = i
            jz_MIN = j
        END IF

        IF (HsenSL(i,j)  .gt.HS_MAX)                                THEN
            HS_MAX =         HsenSL(i,j)
            iShMAX = i
            jShMAX = j
        END IF

        IF (HsenSL(i,j)  .lt.HS_MIN)                                THEN
            HS_MIN =         HsenSL(i,j)
            iShMIN = i
            jShMIN = j
        END IF

        IF (HlatSL(i,j)  .gt.HL_MAX)                                THEN
            HL_MAX =         HlatSL(i,j)
            iL_MAX = i
            jL_MAX = j
        END IF

        IF (HlatSL(i,j)  .lt.HL_MIN)                                THEN
            HL_MIN =         HlatSL(i,j)
            iL_MIN = i
            jL_MIN = j
        END IF

       END DO
       END DO

       DO n=1,mw
       DO j=jp1(1),jm1(my)
       DO i=ip1(1),im1(mx)

        IF (TsrfSL(i,j,n).lt.TS_MIN(    n))                         THEN
            TS_MIN(n) =      TsrfSL(i,j,n)
            iTsMIN(n) =             i
            jTsMIN(n) =               j
            kTsMIN(n) =                 n
        END IF

        IF (TsrfSL(i,j,n).gt.TS_MAX(    n))                         THEN
            TS_MAX(n) =      TsrfSL(i,j,n)
            iTsMAX(n) =             i
            jTsMAX(n) =               j
            kTsMAX(n) =                 n
        END IF

       DO k=1,nssSNo(i,j,n)

        IF (tisSNo(i,j,n,k).lt.TI_MIN(    n  ))                     THEN
            TI_MIN(n) =        tisSNo(i,j,n,k)
            iTiMIN(n) =             i
            jTiMIN(n) =               j
            kTiMIN(n) =                     k
        END IF

        IF (tisSNo(i,j,n,k).gt.TI_MAX(    n  ))                     THEN
            TI_MAX(n) =        tisSNo(i,j,n,k)
            iTiMAX(n) =             i
            jTiMAX(n) =               j
            kTiMAX(n) =                     k
        END IF

       END DO

       END DO
       END DO
       END DO

       DO k=1,mz
       DO j=jp1(1),jm1(my)
       DO i=ip1(1),im1(mx)
            VV_ijk  =   sqrt(uairDY(i,j,k)*uairDY(i,j,k)
     .                      +vairDY(i,j,k)*vairDY(i,j,k))
        IF (VV_ijk       .gt.VV_MAX)                                THEN
            VV_MAX =         VV_ijk
            iV_MAX = i
            jV_MAX = j
            kV_MAX = k
        END IF

            ss_ijk  =       (psigDY(i,j,k)/pstDYn(i,j))
     .                      *qsigm1(    k)*dt/((ntFast+1)*nt_Mix)
        IF (ss_ijk       .gt.ss_MAX)                                THEN
            ss_MAX =         ss_ijk
            is_MAX = i
            js_MAX = j
            ks_MAX = k
        END IF
        IF (ss_ijk       .lt.ss_MIN)                                THEN
            ss_MIN =         ss_ijk
            is_MIN = i
            js_MIN = j
            ks_MIN = k
        END IF

        IF (pktaDY(i,j,k).gt.TH_MAX)                                THEN
            TH_MAX =         pktaDY(i,j,k)
            iH_MAX = i
            jH_MAX = j
            kH_MAX = k
        END IF

        IF (pktaDY(i,j,k).lt.TH_MIN)                                THEN
            TH_MIN =         pktaDY(i,j,k)
            iH_MIN = i
            jH_MIN = j
            kH_MIN = k
        END IF

        IF (TairDY(i,j,k).gt.TT_MAX)                                THEN
            TT_MAX =         TairDY(i,j,k)
            iT_MAX = i
            jT_MAX = j
            kT_MAX = k
        END IF

        IF (TairDY(i,j,k).lt.TT_MIN)                                THEN
            TT_MIN =         TairDY(i,j,k)
            iT_MIN = i
            jT_MIN = j
            kT_MIN = k
        END IF

        IF (  qvDY(i,j,k).gt.QQ_MAX)                                THEN
            QQ_MAX =           qvDY(i,j,k)
            iQ_MAX = i
            jQ_MAX = j
            kQ_MAX = k
        END IF

        IF (  qvDY(i,j,k).lt.QQ_MIN)                                THEN
            QQ_MIN =           qvDY(i,j,k)
            iQ_MIN = i
            jQ_MIN = j
            kQ_MIN = k
        END IF

        IF (ect_TE(i,j,k).gt.TK_MAX)                                THEN
            TK_MAX =         ect_TE(i,j,k)
            iK_MAX = i
            jK_MAX = j
            kK_MAX = k
        END IF

        IF (ect_TE(i,j,k).lt.TK_MIN)                                THEN
            TK_MIN =         ect_TE(i,j,k)
            iK_MIN = i
            jK_MIN = j
            kK_MIN = k
        END IF

        IF (eps_TE(i,j,k).gt.Te_MAX)                                THEN
            Te_MAX =         eps_TE(i,j,k)
            ie_MAX = i
            je_MAX = j
            ke_MAX = k
        END IF

        IF (eps_TE(i,j,k).lt.Te_MIN)                                THEN
            Te_MIN =         eps_TE(i,j,k)
            ie_MIN = i
            je_MIN = j
            ke_MIN = k
        END IF

        IF (pktRAd(i,j,k).gt.RA_MAX)                                THEN
            RA_MAX =         pktRAd(i,j,k)
            ir_MAX = i
            jr_MAX = j
            kr_MAX = k
        END IF

        IF (pktRAd(i,j,k).lt.RA_MIN)                                THEN
            RA_MIN =         pktRAd(i,j,k)
            ir_MIN = i
            jr_MIN = j
            kr_MIN = k
        END IF

       END DO
       END DO
       END DO

       write(36,6000)iterun,debugm,jdarGE,jhurGE,minuGE,jsecGE
     .              ,iV_MAX,jV_MAX,kV_MAX,VV_MAX
     .              ,ip_MAX,jp_MAX,       pp_MAX
     .              ,ip_MIN,jp_MIN,       pp_MIN
     .              ,is_MAX,js_MAX,ks_MAX,ss_MAX
     .              ,is_MIN,js_MIN,ks_MIN,ss_MIN
     .              ,iH_MAX,jH_MAX,kH_MAX,TH_MAX*3.73
     .              ,iH_MIN,jH_MIN,kH_MIN,TH_MIN*3.73
     .              ,iT_MAX,jT_MAX,kT_MAX,TT_MAX
     .              ,iT_MIN,jT_MIN,kT_MIN,TT_MIN
     .              ,iC_MAX,jC_MAX,       CM_MAX
     .              ,iC_MIN,jC_MIN,       CM_MIN
     .              ,iShMAX,jShMAX,       HS_MAX
     .              ,iShMIN,jShMIN,       HS_MIN
     .              ,iQ_MAX,jQ_MAX,kQ_MAX,QQ_MAX*1.d3
     .              ,iQ_MIN,jQ_MIN,kQ_MIN,QQ_MIN*1.d3
     .              ,iL_MAX,jL_MAX,       HL_MAX
     .              ,iL_MIN,jL_MIN,       HL_MIN
     .              ,iK_MAX,jK_MAX,kK_MAX,TK_MAX
     .              ,iK_MIN,jK_MIN,kK_MIN,TK_MIN
     .              ,ie_MAX,je_MAX,ke_MAX,Te_MAX
     .              ,ie_MIN,je_MIN,ke_MIN,Te_MIN
     .              ,iz_MAX,jz_MAX,       zi_MAX
     .              ,iz_MIN,jz_MIN,       zi_MIN
     .              ,ir_MAX,jr_MAX,kr_MAX,RA_MAX*3.73
     .              ,ir_MIN,jr_MIN,kr_MIN,RA_MIN*3.73
 6000  format(/,i6,3x,a10,3x,4(i2,'.'),         '    Vmax',3i4,f15.2
     .       ,/,9x,10('=')
     .       ,/,34x,      '  p__max',2i4,f19.2,8x,'p__min',2i4,f19.2
     .       ,/,34x,      '  WW_max',3i4,f15.2,8x,'WW_min',3i4,f15.2
     .       ,/,34x,      '  TH_max',3i4,f15.2,8x,'TH_min',3i4,f15.2
     .       ,/,34x,      '  Ta_max',3i4,f15.2,8x,'Ta_min',3i4,f15.2
     .       ,/,34x,      '  dTamax',2i4,f19.3,8x,'dTamin',2i4,f19.3
     .       ,/,34x,      '  HS_max',2i4,f19.3,8x,'HS_min',2i4,f19.3
     .       ,/,34x,      '  Qv_max',3i4,f15.3,8x,'Qv_min',3i4,f15.3
     .       ,/,34x,      '  HL_max',2i4,f19.3,8x,'HL_min',2i4,f19.3
     .       ,/,34x,      '  TKEmax',3i4,f15.3,8x,'TKEmin',3i4,f15.3
     .       ,/,34x,      '  e__max',3i4,f15.6,8x,'e__min',3i4,f15.6
     .       ,/,34x,      '  ZI_max',2i4,f19.2,8x,'ZI_min',2i4,f19.2
     .       ,/,34x,      '  RADmax',3i4,f15.2,8x,'RADmin',3i4,f15.2
     .       )
       DO n=1,mw
       write(36,6011)n,iTsMAX(n),jTsMAX(n),kTsMAX(n),TS_MAX(n)
     .                ,iTsMIN(n),jTsMIN(n),kTsMIN(n),TS_MIN(n)
     .                ,iTiMAX(n),jTiMAX(n),kTiMAX(n),TI_MAX(n)
     .                ,iTiMIN(n),jTiMIN(n),kTiMIN(n),TI_MIN(n)
 6011  format(  30x,i4,   '  TS_max',3i4,f15.2,8x,'TS_min',3i4,f15.2
     .       ,/,34x,      '  T*_max',3i4,f15.2,8x,'T*_min',3i4,f15.2)
       ENDDO
      END IF


!  Ts / sol (whole Domain)
!  -----------------------

      IF (WrTsrf)                                                   THEN
         write(36,6600) iterun,debugm,jdarGE,mmarGE,jhurGE,minuGE,jsecGE
 6600    format(/,i6,3x,a10,6x,'(Time = ',i2,'-',i3,i3,'h',i2,':',i2,')'
     .         ,/,9x,'=',8('-'),'=')
           write(36,6020)     (       i     ,i=i_wri-d_wrs,i_wri+d_wrs)
 6020      format(/,'Ts   j  n',(12(i9,2x)))
         DO j=j_wri+d_wrs,j_wri-d_wrs,-1
         DO n=1,mw
           write(36,6021) j,n,(TsrfSL(i,j,n)
     .                       ,isolSL(i,j)  ,i=i_wri-d_wrs,i_wri+d_wrs)
 6021      format(i6,i3,(12(f9.3,i2)))
         ENDDO
         ENDDO
         DO n=1,mw
           write(36,6022) n
 6022      format(/,'Ts/273.15 for n =',i3)
           write(36,6023)   (       i            ,i= 1,40)
 6023      format( /,4x,41i3)
         DO j=1,my
           write(36,6024) j,(TsrfSL(i,j,n)/273.15,i= 1,40)
 6024      format(i3,1x,41f3.1)
         ENDDO
           write(36,6023)   (       i            ,i=41,81)
         DO j=1,my
           write(36,6024) j,(TsrfSL(i,j,n)/273.15,i=41,81)
         ENDDO
         ENDDO
      END IF


! Atmospheric Profiles
! --------------------
       
      IF(WrFul1)                                                    THEN
         DO i = i_wri-d_wri,i_wri+d_wri
         DO j = j_wri-d_wri,j_wri+d_wri

            WVa   =   0.
         DO n=1,nLimit
            WVa   = WVa+WV__SL(i,j,n)
         ENDDO
            WVa   = WVa/nLimit

         DO k=1,mz
            Ta2(k)=        Ta1    (k)
            Ta1(k)=        Taq    (k)
            Taq(k)=     pktaDY(i,j,k)*          pkDY(i,j,k)
            Qsa(k)=     qsat0D(Taq(k),sigma(k),pstDY(i,j),ptopDY,0)
         ENDDO
         IF (.NOT.Debugg)                                           THEN
                  Debugg=.true.
           DO k=1,mz
            Ta1(k)=        Taq    (k)
            Ta2(k)=        Taq    (k)
           ENDDO
         END IF
         DO k=1,mz
            TCM(k)= abs(0.5* ( Ta2(k)+Taq  (k) )-Ta1    (k))
            CMx(k)= max(       TCM(k),CMx  (k) )
         ENDDO

         write(36,6600) iterun,debugm,jdarGE,mmarGE,jhurGE,minuGE,jsecGE
         write(36,6001) i,j
 6001    format(/,' Atmospher.Profiles, Grid Point',2i4)
         write(36,6601)
 6601    format(  8x,' Ta',4x,' Qa',4x,'Qsa',
     .            4x,' Qi',4x,' Qw',4x,' Qs',4x,' Qr',
     .            5x,' Ua',5x,' Va',5x,' Wa',
     .            5x,'TKE',5x,'eps',5x,' Kz',5x,'  z',
     .           '  Convection',' T Comput.Mode')
         write(36,6002)(k,Taq(    k)     ,  qvDY(i,j,k)*1.e3,Qsa(k)*1.e3
     .                ,  qiHY(i,j,k)*1.e3,  qwHY(i,j,k)*1.e3
     .                ,  qsHY(i,j,k)*1.e3,  qrHY(i,j,k)*1.e3
     .                ,uairDY(i,j,k)     ,vairDY(i,j,k)
     .                ,wairDY(i,j,k)
     .                ,ect_TE(i,j,k)     ,eps_TE(i,j,k),   TUkvm(i,j,k)
     .                ,gplvDY(i,j,k)*grvinv            *1.e-3
     .                ,dpktCA(i,j,k)*3.73,dqv_CA(i,j,k)*1.e3
     .                ,   TCM(    k)     ,   CMx(    k)
     .                ,           k =    1 ,mz)
 6002    format(i3,f8.3,6f7.3,7f8.3,f6.1,f6.3,2f7.3)
         write(36,6601)
         write(36,6003) TairSL(i,j)  ,qvapSL(i,j)*1.e3
     .           ,1.e3*snowHY(i,j)  
     .           ,1.e3*rainHY(i,j)
     .           ,      SLuus(i,j)  *  SLuus(i,j)
 6003    format(' S.',f8.3,f7.3,20x,f8.3,f7.3,16x,f16.3)
         write(36,6603)
 6603    format(  8x,' TS',4x,'QvS',4x,'   ',
     .            4x,'   ',4x,'   ',4x,'US*',4x,' RR',
     .            5x,'   ',5x,'   ',5x,'   ',
     .            5x,'u*2',5x,'   ',5x,'   ',5x,'   ',
     .           '            ','              ')
         ENDDO
         ENDDO
      END IF


!  Surface Characteristics
!  -----------------------
         
      IF(WrFul2)                                                    THEN
         DO i = i_wri-d_wri,i_wri+d_wri
         DO j = j_wri-d_wri,j_wri+d_wri
         write(36,6600)iterun,debugm,jdarGE,mmarGE,jhurGE,minuGE,jsecGE
         write(36,6100)i,j,isolSL(i,j),(SLsrfl(i,j,n  ),n=1,mw)
         write(36,6101)                (tsrfSL(i,j,n  ),n=1,mw)
         write(36,6102)                 sst_LB(i,j),
     .                                 sst1LB(i,j),
     .                                 sst2LB(i,j)
         write(36,6103)                (nssSNo(i,j,n  ),n=1,mw)
         write(36,6128)                (issSNo(i,j,n  ),n=1,mw)
         write(36,6129)                (nisSNo(i,j,n  ),n=1,mw)
         write(36,6104)                 tairDY(i,j,mz)
         write(36,6105)                  ssvSL(i,j,mz)
         write(36,6106)                   qvDY(i,j,mz)
         write(36,6107)                 RAdsol(i,j)
         write(36,6108)                 RAD_ir(i,j)
         write(36,6109)                (IRsoil(i,j,n  ),n=1,mw)
         write(36,6110)                (SLlmol(i,j,n  ),n=1,mw)
         write(36,6111)                (SLuusl(i,j,n  ),n=1,mw)
         write(36,6211)                (SaltSN(i,j,n  ),n=1,mw)
         write(36,6311)                (SLussl(i,j,n  ),n=1,mw)
         write(36,6112)                (SL_z0( i,j,n  ),n=1,mw)
         write(36,6113)                (SLutsl(i,j,n  ),n=1,mw)
         write(36,6114)                (SL_r0( i,j,n  ),n=1,mw)
         write(36,6115)                (SLuqsl(i,j,n  ),n=1,mw),WVa
         write(36,6116)                 maskSL(i,j)
         write(36,6117)                 isolTV(i,j)
         write(36,6118)                 AlbSTV(i,j)
         write(36,6119)                (ivegTV(i,j,n  ),n=1,mw)
         write(36,6120)                (alaiTV(i,j,n  ),n=1,mw)
         write(36,6121)                (glf_TV(i,j,n  ),n=1,mw)
         write(36,6122)                (TvegTV(i,j,n  ),n=1,mw)
         write(36,6123)                (CaSnTV(i,j,n  ),n=1,mw)
         write(36,6124)                (CaWaTV(i,j,n  ),n=1,mw)
         write(36,6125)                (psivTV(i,j,n  ),n=1,mw)
         write(36,6126)               ((TsolTV(i,j,n,l),l=1,llx),n=1,mw)
         write(36,6127)               ((eta_TV(i,j,n,l),l=1,llx),n=1,mw)
 6100    format(/,' SL characteristics, Grid Point',2i4
     .         ,'   Type =', i2   
     .         ,/,' SrfSL  = ',9e15.6)
 6101    format(  ' Ts     = ',9e15.6)
 6102    format(  ' SST    = ', e15.6,'   (between',e15.6,
     .                                      '  and',e15.6,')')
 6103    format(  ' Details:',
     .          /,' Nb *   = ',9(i4,11x))
 6128    format(  ' Nb ice = ',9(i4,11x))
 6129    format(  ' Nb ICE = ',9(i4,11x))
 6104    format(  ' tairDY = ', e15.6)
 6105    format(  '  ssvSL = ', e15.6)
 6106    format(  '   qvDY = ', e15.6)
 6107    format(  ' RAdsol = ', e15.6)
 6108    format(  ' RAD_ir = ', e15.6)
 6109    format(  ' IRsoil = ',9e15.6)
 6110    format(  ' SLlmol = ',9e15.6)
 6111    format(  ' SLuusl = ',9e15.6)
 6211    format(  ' SaltSN = ',9e15.6)
 6311    format(  ' SLussl = ',9e15.6)
 6112    format(  ' SL_z0  = ',9e15.6)
 6113    format(  ' SLutsl = ',9e15.6)
 6114    format(  ' SL_r0  = ',9e15.6)
 6115    format(  ' SLuqsl = ',2e15.6,'   WVa = ',e15.6)
 6116    format(  ' maskSL = ', i15)
 6117    format(  ' isolTV = ', i15)
 6118    format(  ' AlbSTV = ', e15.6)
 6119    format(  ' ivegTV = ',2i15)
 6120    format(  ' alaiTV = ',9e15.6)
 6121    format(  ' glf_TV = ',9e15.6)
 6122    format(  ' TvegTV = ',9e15.6)
 6123    format(  ' CaSnTV = ',9e15.6)
 6124    format(  ' CaWaTV = ',9e15.6)
 6125    format(  ' psivTV = ',9e15.6)
 6126    format(  ' TsolTV = ',7e15.6,6(/,'          ',7e15.6))
 6127    format(  ' eta_TV = ',7e15.6,6(/,'          ',7e15.6))

         ENDDO
         ENDDO
      END IF


!  Snow Pack Characteristics
!  -------------------------

                                       iw=min(mw,2)
      IF(WrFul3.AND.nssSNo(i_wri,j_wri, 1).GT.0
     .          .OR.nssSNo(i_wri,j_wri,iw).GT.0)                    THEN
       DO i = i_wri-d_wri,i_wri+d_wri
       DO j = j_wri-d_wri,j_wri+d_wri
         write(36,6600) iterun,debugm,jdarGE,mmarGE,jhurGE,minuGE,jsecGE
         write(36,6130) albeSL(i_wri,j_wri)  *100.
 6130    format(27x,'Grid Mesh Albedo:',f5.1,' %',f61.1,' %')
        IF (mw.GE.2)                                                THEN
         write(36,6230)(albxSL(i_wri,j_wri,n)*100.,n=1,iw)
 6230    format(27x,'Mosaic    Albedo:',f5.1,' %',f61.1,' %')
         write(36,6131) i,j
 6131    format(/,' Snow      Profiles, Grid Point',2i4,
     .          /,8x,'Ti1',5x,'dZ1',5x,'ro1',5x,'wa1',
     .            5x,'G11',5x,'G12',5x,'Ag1',2x,'Hi1',
     .            5x,'Ti2',5x,'dZ2',5x,'ro2',5x,'wa2',
     .            5x,'G21',5x,'G22',5x,'Ag2',2x,'Hi2')
         write(36,6132)(k,tisSNo(i,j,1 ,k),dzsSNo(i,j,1 ,k) 
     .                   ,rosSNo(i,j,1 ,k),wasSNo(i,j,1 ,k)
     .                   ,g1sSNo(i,j,1 ,k),g2sSNo(i,j,1 ,k)
     .                   ,agsSNo(i,j,1 ,k),nhsSNo(i,j,1 ,k)
     .                   ,tisSNo(i,j,iw,k),dzsSNo(i,j,iw,k)
     .                   ,rosSNo(i,j,iw,k),wasSNo(i,j,iw,k)
     .                   ,g1sSNo(i,j,iw,k),g2sSNo(i,j,iw,k)
     .                   ,agsSNo(i,j,iw,k),nhsSNo(i,j,iw,k)
     .                 ,k = max(nssSNo(i,j,1),nssSNo(i,j,iw)),1,-1)
 6132    format((i3,2(7f8.3,i3,4x)))
         write(36,6133)        (nssSNo(i,j,n),SWaSNo(i,j,n),n=1,iw)
 6133    format(3x,'nb *** = ',i3,f20.3,39x,9x,i3,f20.3)
         write(36,6134)        (issSNo(i,j,n  ),n=1,iw)
 6134    format(3x,'nb ice = ',i3,4x,55x,9x,i3)
         write(36,6135)        (nisSNo(i,j,n  ),n=1,iw)
 6135    format(3x,'nb ICE = ',i3,4x,55x,9x,i3)
        ELSE
         write(36,6230) albxSL(i_wri,j_wri,1)*100.
         write(36,6231) i,j
 6231    format(/,' Snow      Profiles, Grid Point',2i4,
     .          /,8x,'Ti1',5x,'dZ1',5x,'ro1',5x,'wa1',
     .            5x,'G11',5x,'G12',5x,'Ag1',2x,'Hi1')
         write(36,6232)(k,tisSNo(i,j,1 ,k),dzsSNo(i,j,1 ,k) 
     .                   ,rosSNo(i,j,1 ,k),wasSNo(i,j,1 ,k)
     .                   ,g1sSNo(i,j,1 ,k),g2sSNo(i,j,1 ,k)
     .                   ,agsSNo(i,j,1 ,k),nhsSNo(i,j,1 ,k)
     .                   ,k = nssSNo(i,j,1),1,-1)
 6232    format((i3,1(7f8.3,i3,4x)))
         write(36,6233)      (nssSNo(i,j,n),SWaSNo(i,j,n),n=1,iw)
 6233    format(3x,'nb *** = ',i3,f20.3,39x,9x,3x)
         write(36,6234)      (issSNo(i,j,n),n=1,iw)
 6234    format(3x,'nb ice = ',i3,4x,55x,9x,3x)
         write(36,6235)      (nisSNo(i,j,n),n=1,iw)
 6235    format(3x,'nb ICE = ',i3,4x,55x,9x,3x)
        END IF
       ENDDO
       ENDDO
      END IF


!  DUMP
!  ====

      IF (DUMPOK)                                                   THEN

          DO k=1,mz
          DO j=1,my
          DO i=1,mx
           dump_v(i,j,k) = pktRAd(i,j,k)
          END DO
          END DO
          END DO

C +            **********
          call dump3D_MAR(dump_v,debugm,'pktRAd    ')
C +            **********

      END IF

 5    CONTINUE

      return
      end 
      subroutine dump3D_MAR(dump3D,debugm,dump_m)

C +------------------------------------------------------------------------+
C |                                                                        |
C | MAR          dump3D_MAR                             Mc 07-04-2004  MAR |
C |              DUMP for Verification of MAR Variables                    |
C |                                                                        |
C +------------------------------------------------------------------------+


      IMPLICIT NONE


C +--General Variables
C +  =================

      include 'MARCTR.inc'

      include 'MARdim.inc'
      include 'MARgrd.inc'

      real              dump3D(mx,my,mz)
      character*10      debugm,dump_m

      logical           dumpIN
      common/dump3D_log/dumpIN

      IF (.NOT.dumpIN)                                              THEN
        open(unit=80,status='unknown',file='dump3D_MAR.OUT')
        rewind    80
        dumpIN=.true.
      END IF

      DO k=1,mz
      DO j=1,my
      DO i=1,mx
        write(80,800) itexpe,debugm,dump_m,i,j,k,dump3D(i,j,k)
 800    format(i6,3x,a10,3x,a10,3i6,f15.6)
      ENDDO
      ENDDO
      ENDDO

      return
      end
