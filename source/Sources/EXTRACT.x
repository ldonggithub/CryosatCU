# gallee [forMAR]> grep block  MAR___.FOR
#       block data PHYdat
# C |   INPUT    (via common block)                                          |
# C |   INPUT    (via common block)                                          |
#       block data SISVAT_CdP
#       block data SISVAT_dat
# C +    (see 'block data SISVAT_dat', variable rbtdSV)
# gallee [forMAR]> grep function MAR___.FOR
#       function qsat0D(ttq,ss,pstar,pt,lsf)
# gallee [forMAR]> grep subrou MAR___.FOR
#       subroutine PHYmar
#       subroutine PHYrad_top(Dis_ST)
#       subroutine PHY_SISVAT(ihamr_SIS,nhamr_SIS)
#       subroutine SISVAT_ini
#       subroutine SISVAT
#       subroutine SISVAT_BSn
#       subroutine SISVAT_BDu
#       subroutine SISVAT_SIc
#       subroutine SISVAT_zSn
#       subroutine SISVAT_zCr
#       subroutine SISVAT_zAg
#       subroutine SnOptP
#       subroutine VgOptP
#       subroutine ColPrt_SBL
#       subroutine SISVATeSBL
#       subroutine SISVAT_SBL
#       subroutine SISVAT_TVg
#       subroutine SISVAT_TSo
#       subroutine SISVAT_qVg
#       subroutine SISVAT_qSn
#       subroutine SISVAT_GSn
#       subroutine SISVAT_qSo
#       subroutine SISVAT_wEq( labWEq ,istart)
#       subroutine INIglf(ihamr_glf,nhamr_glf,newglfINI)
##############################################################################
cp -p $MARONH/forMAR/MAR0SV.inc .
cp -p $MARONH/forMAR/MAR_BS.inc .
cp -p $MARONH/forMAR/MAR_CA.inc .
cp -p $MARONH/forMAR/MARCTR.inc .
cp -p $MARONH/forMAR/MARdSV.inc .
cp -p $MARONH/forMAR/MAR_DY.inc .
cp -p $MARONH/forMAR/MAR_GE.inc .
cp -p $MARONH/forMAR/MARgrd.inc .
cp -p $MARONH/forMAR/MAR_HY.inc .
cp -p $MARONH/forMAR/MAR_IB.inc .
cp -p $MARONH/forMAR/MAR_IO.inc .
cp -p $MARONH/forMAR/MAR_LB.inc .
cp -p $MARONH/forMAR/MARlSV.inc .
cp -p $MARONH/forMAR/MARphy.inc .
cp -p $MARONH/forMAR/MAR_RA.inc .
cp -p $MARONH/forMAR/MARsIB.inc .
cp -p $MARONH/forMAR/MAR_SL.inc .
cp -p $MARONH/forMAR/MARSND.inc .
cp -p $MARONH/forMAR/MAR_SN.inc .
cp -p $MARONH/forMAR/MARsSN.inc .
cp -p $MARONH/forMAR/MAR_TE.inc .
cp -p $MARONH/forMAR/MAR_TU.inc .
cp -p $MARONH/forMAR/MAR_TV.inc .
cp -p $MARONH/forMAR/MAR_VB.inc .
cp -p $MARONH/forMAR/MAR_WK.inc .
cp -p $MARONH/forMAR/MARxSV.inc .
cp -p $MARONH/forMAR/MARySV.inc .
##############################################################################

