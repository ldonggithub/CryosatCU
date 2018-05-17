      program    ECsvat

C +------------------------------------------------------------------------+
C |                                                                        |
C |   SISVAT     StandALONE .main. program                 16-02-2006  MAR |
C |                                                                        |
C |   Modification % MAR code: INIglf is NOT used                          |
C |   (see #sa)                sol_SL is NOT a forcing                     |
C |                            IRsoil is     included in .main.            |
C |                            glf_sv is NOT used to calibrate Z0mdSV      |
C |                            LAI_sv is     used to calibrate Z0mdSV      |
C |                                                                        |
C |   Preprocessing  Option: SISVAT PHYSICS                                |
C |   ^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^                                |
C |                          #HY: Cloud Microphysics                       |
C |                          #SN: Snow Model                               |
C |                          #CZ: Snow Albedo, Zenith Angle Correction     |
C |                          #BS: Blowing Snow Parameterization            |
C |                          #SI: Sea-Ice      Parameterization            |
C |                                                                        |
C |                          #DS: diffuse radiation differing from direct  |
C |                              (variable RADsod must still be included)  |
C |                                                                        |
C |   Preprocessing  Option: SISVAT PHYSICS: Col de Porte                  |
C |   ^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^                  |
C |                          #CP: SBL,                       Col de Porte  |
C |                          #cp  Solar Radiation,           Col de Porte  |
C |                          #AG: Snow Ageing,               Col de Porte  |
C |                                                                        |
C |   Preprocessing  Option: SISVAT IO                                     |
C |   ^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^                                     |
C |   FILE                 |      CONTENT                                  |
C |   ~~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |
C |   ANI.yyyymmdd.LAB.nc  | #NC: OUTPUT on NetCDF File (Stand Alone EXP.) |
C |   SISVAT_iii_jjj_n     | #WV: OUTPUT on ASCII  File (SISVAT Variables) |
C |                        |                                               |
C |                        | #ES: OUTPUT/Verification: Energy Conservation |
C |                        | #E2: OUTPUT/Verification: Energy Consrv.2e pt.|
C |                        |                           (no premature stop) |
C |                        | #MW: OUTPUT/Verification: H2O    Conservation |
C |                        | #MS: OUTPUT/Verification: * Mass Conservation |
C |                        | #MI: OUTPUT/Verification: SeaIce Conservation |
C |                        |                                               |
C |   SISVAT__zSn.OUT      | #as: OUTPUT/Verification: Snow Layers Agrega. |
C |   SISVAT__SnO.OUT      | #aw: OUTPUT/Verification: Albedo Parameteriz. |
C |   SISVATe_qSn.OUT      | #em: OUTPUT/Verification: Energy/Water Budget |
C |   SISVATu_qSn.OUT      | #su: OUTPUT/Verification: Slush  Parameteriz. |
C |   SISVATw_qSo.OUT      | #mw: OUTPUT/Verif+Detail: H2O    Conservation |
C |                        |                                               |
C |   SISVAT__GSn.OUT      | #VP: OUTPUT/Verification: Snow   Properties   |
C |   SISVAT__wEq.OUT      | #EQ: OUTPUT/Verification: Snow/Ice Water Eqv. |
C |                                                                        |
C |   Preprocessing  Option: Set-Up                                        |
C |   ^^^^^^^^^^^^^^^^^^^^^  ^^^^^^                                        |
C |                                                                        |
C |   Preprocessing  Option: Set-Up: ETH-Camp                              |
C |   ^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^                              |
C |                                                                        |
C |                                                                        |
C |                                                                        |
C +------------------------------------------------------------------------+


      IMPLICIT NONE


C +--General Variables
C +  =================

      include  "MARCTR.inc"
      include  "MARdim.inc"
      include  "MAR_SV.inc"      
      include  "MARphy.inc"
      include  "MARgrd.inc"
      include  "MAR_GE.inc"

      include  "MAR_DY.inc"
      include  "MAR_LB.inc"
      include  "MAR_HY.inc"

      include  "MAR_SL.inc"
      include  "MARsSN.inc"
      include  "MAR_SN.inc"
      include  "MAR_TV.inc"
      include  "MARdSV.inc"

      include  "MAR_IO.inc"

      integer   njmo  ,l     ,n     ,npt
      integer   ihamr ,nhamr
      integer   ntexpe,n_iter
      integer   ianETH
      integer   yr1,mo1,dd1,hh1 !Start date (yyyy,mo,dd,hh)
      integer   yr2,mo2,dd2,hh2 !End date (yyyy,mo,dd,hh)
      logical   found
      character(len=100) ind

      real      Dis_ST
      real      curtim
      real      TimeNC

      double precision  totTIM ! total simulation time, minutes.

      real         aIcdSV
      common /ETHa/aIcdSV


C +--DATA
C +  ====

      data      explIO/'ETH'/

C      data      TimeNC/1800./
C +...          TimeNC:Time Interval between IO on NetCDF File

      So1dSV = 0.58 
      So2dSV = 0.32
      So3dSV = 0.10
      aI1dSV = 0.55
c     aI2dSV = 0.55
c     aI3dSV = 0.55

c      data  date1
c     .      /1990, 6, 12, 0/
c      data  date2
c     .      /1990, 8, 30, 0/

C +--Useful  Constants
C +  -----------------

      pi     = acos(-1.)
      degrad = pi /180.
      hourad = pi / 12.

C +              ******
        call     PHYmar
C +              ******

      grvinv    =      1. / gravit            !
      VegMod    = .true.                      ! NOT necessary
      SnoMod    = .true.                      !
      reaLBC    = .false.                     !


C +--Initialization
C +  ==============

C +--Year
C +  ----

      open (unit=30,status='old',file='sisvat.ctr')

C     READ IN START DATE
      call ctrPrep(30,'START DATE (YYYY,MM,DD,HH)') 
      read (30,*) yr1,mo1,dd1,hh1
      write(*,*) "START DATE: ",yr1,mo1,dd1,hh1

C     READ IN END DATE
      call ctrPrep(30,'END DATE (YYYY,MM,DD,HH)')
      read (30,*) yr2,mo2,dd2,hh2
      write(*,*) "END DATE:   ",yr2,mo2,dd2,hh2
     
C     DETERMINE SIMULATION LENGTH 
      call simlength(yr1,mo1,dd1,hh1,yr2,mo2,dd2,hh2,totTIM) 
      write(*,*) "Simulation length (minutes): ",totTIM  

C     OBTAIN PHYSICS TIMESTEP     
      call ctrPrep(30,'dt physics (s)')
      read (30,*) dtPhys
      write(*,*) "Timestep (s):   ",dtPhys

C     OBTAIN NETCDF OUTPUT INTERVAL      
      call ctrPrep(30,'Output Interval (min)')
      read (30,*) TimeNC
      write(*,*) "Output Interval (min): ",TimeNC
      TimeNC = TimeNC*60.

C     Number of timesteps:
      ntexpe = totTIM*60./dtPhys

C      write(*,*) "ntexpe: ",ntexpe     
C      open (unit=30,status='old',file='sISvat.ctr')
C      rewind     30
C            read(30,300) ianETH
C 300        format(i4)
C      close(unit=30)
C
C      IF      (ianETH.eq.1990)                                    THEN
C        ntexpe =   3792 * 5
C      ELSE IF (ianETH.eq.1991)                                    THEN
C        ntexpe =   3696 * 5
C      ELSE
C        write(6,3000)
C 3000   format(' Data do not exist for ',i4)
C        STOP   ' @^`?#?#     EMERGENCY EXIT'
C      END IF

        nterun = ntexpe                      !
c        dtPhys =    360.                     !
        dt     = dtPhys                      !


C +--Surface
C +  -------

        im1(1)      =    1
        ip1(1)      =    1
        jm1(1)      =    1
        jp1(1)      =    1
        sigma(mz)   =    1.0

C       LAT/LON
        call ctrPrep(30,'LATITUDE (dd)')
        read (30,*) GElat0
        write(*,*) "LATITUDE (dd): ",GElat0

        call ctrPrep(30,'LONGITUDE (dd)')
        read (30,*) GElon0
        write(*,*) "LONGITUDE (dd): ",GElon0

C        GElat0      =   69.6123
C        GElon0      =  -49.1479

C       LON/LAT continued.
        GElatr(1,1) =   GElat0 * degrad
        GElonh(1,1) =   GElon0 * degrad / hourad

c        write(*,*) "GElonh",GElonh

        clatGE(1,1) =   cos(GElatr(1,1))
        slatGE(1,1) =   sin(GElatr(1,1))

c        itizGE(1,1) =   -3   !"Time Zone"
        itizGE(1,1) = GElonh(1,1)
        if (itizGE(1,1).gt. 12) itizGE(1,1) = itizGE(1,1)-24
        if (itizGE(1,1).lt.-12) itizGE(1,1) = itizGE(1,1)+24
c        write(*,*) 'itizGE',itizGE
        SLsrfl(1,1,1)= 1

C       Load Surface height.
        call ctrPrep(30,'SURFACE HEIGHT (m)')
        read (30,*) sh(1,1)
        write(*,*) "SURFACE HEIGHT (m): ",sh(1,1)
c        sh    (1,1) = 1155.

        isolSL(1,1) =    3
        maskSL(1,1) =    0
        Tfr_LB      =   tfrwat

C +          ******
        call ETHdat
C +          ******
          
c#PA        write(*,*) "ros ", rosSNo
          iyrrGE =  iyr0GE
          mmarGE =     mo1
          jdarGE =  jda0GE
          jhurGE = -itizGE(1,1)
      IF (jhurGE.lt.0)                                            THEN
          jhurGE =  jhurGE + 24
          jdarGE =  jdarGE -  1
      END IF
      
      write (*,*) "Start, UTC:"
      write (*,*) iyrrGE, mmarGE,jdarGE,jhurGE,minuGE,jsecGE
      write (*,*) itizGE

C +--Atmosph?re
C +  ----------

        i               =     1
        j               =     1
        n               =     1
        ptopDY          =     0.
        gplvDY(i,j,mz)  =    10.*gravit
c #     TairDY(i,j,mz)  =   280.
        pstDYn(i,j)     =    84.
          pkDY(i,j,mz) = exp(cap *log(pstDYn(i,j)*sigma(mz)+ptopDY))
        TairSL(i,j)     = TairDY(i,j,mz)
        snowHY(i,j)     =     0.
        rainHY(i,j)     =     0.


C +--V?g?tation
C +  ----------

        ifraTV(i,j,1)   = 100              !
        mskSNo(1,1)   = ifraTV(1,1,1)
C +                                        !
        LAIdSV          =   4.             !
C +                                        !
        ivegTV(i,j,1)   =   0              ! NO   VEGETATION
        alaiTV(i,j,1)   =   0.             !
        glf_TV(i,j,1)   =   0.             !


C +--Continental Ice
C +  ---------------

        isolTV(i,j)     =  12              ! CONTINENTAL ICE
        iWaFTV(i,j)     =   1              !
        albSTV(i,j)     =   aIcdSV         !
        eps0SL(i,j)     =   1.             !

        TgrdTV(i,j,1)   =  tisSNo(       i,j,1,1)
      DO n=1,nvx
      DO l=1,llx
        TsolTV(i,j,n,l) =  tisSNo(       i,j,1,1)
        eta_TV(i,j,n,l) =  etadSV(isolTV(i,j))
      END DO
      END DO


C +--IO
C +  --

      DO npt=1,iptx
        IOi_TV(npt) = 1
        IOj_TV(npt) = 1
      END DO


C +--OUTPUT (NetCDF File)
C +  ====================

      itexpe = 0
      itexpe = 0

      n_iter = TimeNC    / dtPhys
C +...n_iter : Number of Time steps between IO

      ipr_nc =                      1
      npr_nc = 365*86400 / TimeNC + 1
C +...npr_nc : Upper Bound to the Number of IO

C +   ***********
c      call Out_nc(ipr_nc,npr_nc,n_iter)
C +   ***********


C +--Initialisation of SISVAT
C +  ========================

c#PA      write(*,*) "ros2 ", rosSNo
C +   ***************
      call PHY_SISVAT(ihamr,nhamr)
C +   ***************

c#PA      write(*,*) "ros3 ", rosSNo
C +   ***************
c      call Debugg_MAR('SISVAT ETH')
C +   ***************


C +--TIME STEPPING
C +  =============


      DO itexpe = 1,ntexpe
         iterun =   itexpe


C +--Current Time (dd-MM-yyyy:hh:mm:ss)
C +  ----------------------------------

          jsecGE = jsecGE + int(dtPhys)

 240  CONTINUE
      if (jsecGE .lt.  60)                                    GO TO 241
          jsecGE = jsecGE - 60
          minuGE = minuGE + 1
      go to 240
 241  CONTINUE
      if (minuGE .lt.  60)                                    GO TO 242
          minuGE = minuGE - 60
          jhurGE = jhurGE + 1
      go to 241
 242  CONTINUE
      if (jhurGE .lt.  24)                                    GO TO 243
          jhurGE = jhurGE - 24
          jdarGE = jdarGE + 1
      go to 242
 243  CONTINUE
          njmo   =          njmoGE(mmarGE)
     .                     +njmbGE(mmarGE)*max(0,1-mod(iyrrGE,4))
      if (jdarGE .le.       njmo)                             GO TO 244
          jdarGE = jdarGE - njmo
          mmarGE = mmarGE + 1
      if (mmarGE .le.  12)                                    GO TO 243
          mmarGE = mmarGE - 12
          iyrrGE = iyrrGE + 1
      go to 243
 244  CONTINUE


C +--Current Time (hh since 15-JAN-1901)
C +  -----------------------------------

          curtim = (351+(iyrrGE  -1902) *365       ! Nb Days before iyrrGE
     .                 +(iyrrGE  -1901) /  4       ! Nb Leap Years
     .                 + njyrGE(mmarGE)            ! Nb Days before mmarGE
     .                 + njybGE(mmarGE)            ! (including Leap Day)
     .             *max(0,1-mod(iyrrGE,4))         !
     .                 + jdarGE     -1      )*  24 !
     .             +jhurGE                         !
     .           + (minuGE *60 +jsecGE      )/3600.!

c       write(*,*) "Date: ", iyrrGE, mmarGE, jdarGE,jhurGE,minuGE,jsecGE
C +--Update the snowpack for Greenland ice sheet.
C +  --------------------------------------------

      IF ( iterun > 1 .and. mod(iterun,12*3600/int(dtPhys))==0) THEN
c       write (*,*) iterun
c       write (*,*) 12*3600/int(dtPhys)
       call UPDsnow
      END IF

C +--FORCING
C +  -------

C +          ******
        call ETHdat
C +          ******


C +--SISVAT Simulation
C +  -----------------

C +   ***************
      call PHYrad_top(Dis_ST)
C +   ***************

C +--Ideal Forcing in case of Blowing Snow
C +  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c #bs    czenGE(1,1)  = 0.5

C +   ***************
      call PHY_SISVAT(ihamr,nhamr)
C +   ***************

C +   ***************
c      call Debugg_MAR('SISVAT ETH')
C +   ***************


C +--OUTPUT (NetCDF File)
C +  --------------------

      IF       (mod(itexpe,n_iter).eq.0)                      THEN ! CTR
                    ipr_nc=ipr_nc  +  1

C +     ***********
c        call Out_nc(ipr_nc,npr_nc,n_iter)
C +     ***********

      END IF                                                       ! CTR

      i = 1
      j = 1
      n = 1

      END DO

C +--Update the snowpack one last time.
C +  ----------------------------------
      IF ( iterun > 1 .and. mod(iterun,12*3600/int(dtPhys))==0)
     . call UPDsnow
C +  **********************************

      end

      subroutine ctrPrep(fileNum,field)
C +------------------------------------------------------------------------+
C | MAR   (Author: Patrick Alexander)                      08-11-2016 MAR  |
C |   SubRoutine ctrPrep looks through the file specified by fileNum,      |
C |       and stops at the specified field, so that it can then be read    |
C |       into a variable.                                                 |
C +------------------------------------------------------------------------+
C |                                                                        |
C |   INPUT: fileNum: file identifier                                      |
C |          field: string containing the field to be stopped at in file.  |
C |                                                                        |
C +------------------------------------------------------------------------+

      Implicit None

C +--General Variables
C +  =================

C +--Local Variables
C +  ===============
      integer fileNum
      character(len=100) ind
      character(len=*) field
      logical found

C +--Main
C +  ====

      rewind fileNum
      found = .FALSE.
      do while (.NOT. found)
         read (30,111) ind
111      FORMAT(a100)
         if (ind .EQ. field) found = .TRUE.
      end do

c      write(*,*) "In File: ", ind
c      write(*,*) "Searching for: ", field      

      return
      end
C     end subroutine ctrPrep

      subroutine simlength(yr1,mo1,dd1,hh1,yr2,mo2,dd2,hh2,totTIM)
C +------------------------------------------------------------------------+
C | MAR  (Author: Patrick Alexander)                       04-11-2016  MAR |
C |   SubRoutine simlength calculates the length of a simulation (in       |
C |              minutes) given the start and end dates of the simulation. |
C +------------------------------------------------------------------------+
C |                                                                        |
C |   INPUT:  date1:  start date array with (yr,mo,dd,hh)                  |
C |           date2:  end   date array with (yr,mo,dd,hh)                  |
C |   OUTPUT: totTIM:  total simulation time, in minutes.                  |
C |                                                                        |
C +------------------------------------------------------------------------+

      Implicit None

C +--General Variables
C +  =================
      include 'MARdim.inc'
      include 'MAR_GE.inc'

C +--Local Variables
C +  ===============
c      integer date1(1:4)
c      integer date2(1:4)
      integer njyrGE1(0:12)
      integer njyrGE2(0:12), njmoGE2(0:12)
      integer yr1,mo1,dd1,hh1
      integer yr2,mo2,dd2,hh2      
      integer n,ind
      double precision totTIM
      logical leap

C +--Main
C +  ====
c      write(*,*) 'Hello World!'     
C     1. Get yr,mo,dd,hh
c      yr1 = date1(1)
c      yr2 = date2(1)
c      mo1 = date1(2)
c      mo2 = date2(2)
c      dd1 = date1(3)
c      dd2 = date2(3)
c      hh1 = date1(4)
c      hh2 = date2(4)
c      write(*,*) yr1,mo1,dd1,hh1
c      write(*,*) yr2,mo2,dd2,hh2

C     2. Calculate total minutes for all years.
      totTIM = (yr2-yr1+1)*365*24*60 !Total minutes.
      
c      write(*,*) totTIM
C     3. Correct total minutes for leap years.
      leap = .FALSE.
      do n = yr1,yr2
C        Check for leap year.
         if (mod(n,4).NE.0) then
            leap = .FALSE.
         else if (mod(n,100).NE.0) then
            leap = .TRUE.
         else if (mod(n,400).NE.0) then
            leap = .FALSE.
         else
            leap = .TRUE.
         end if
C        Add minutes for leap years.
         if (leap) totTIM = totTIM + 24*60
c         write(*,*) totTIM
C        Determine start and end year date timings.
C        njyrGE:  number of days since beginning of year.
C        njmoGE:  number of days in each month
C        njmbGE:  leap year correction to jnmoGE
C        njybGE:  leap year correction to njyrGE
         if (n.EQ.yr1 .AND. leap) then
            njyrGE1 = njyrGE + njybGE
         else
            njyrGE1 = njyrGE
         endif

         if (n.EQ.yr2 .AND. leap) then
            njyrGE2 = njyrGE + njybGE
            njmoGE2 = njmoGE + njmbGE
         else
            njyrGE2 = njyrGE
            njmoGE2 = njmoGE
         endif
         
      end do
      
c      write(*,*) totTIM
C     4. Correct total minutes for partial years.

C     correct for partial year before start date.
      totTIM = totTIM - njyrGE1(mo1)*24*60 !preceding mo's
      totTIM = totTIM - (dd1-1)*24*60    !preceding dy's
      totTIM = totTIM - (hh1)*60       !preceding hr's
      
c      write(*,*) njyrGE1
c      write(*,*) totTIM
C     correct for partial year after start date.
      totTIM = totTIM - (njyrGE2(12)+31-njyrGE2(mo2)-dd2)*24*60
c      write(*,*) totTIM
c      totTIM = totTIM - (njmoGE2(mo2)-dd2)*24*60
c      write(*,*) totTIM
      totTIM = totTIM - (24-hh2)*60

c      write(*,*) 'Total Time: ',totTIM
c      totTIM = DBLE(totTIM)
      
      return
      end   
c     end subroutine simlength


      subroutine ETHdat

C +------------------------------------------------------------------------+
C | MAR INPUT      SURFACE BOUNDARY LAYER                  16-02-2006  MAR |
C |   SubRoutine ETHdat (Special ETH-CAMP 1990-1991)                       |
C |                                                                        |
C |     1) initialises the SNOW MODEL                                      |
C |     2) assimilates New Surface Boundary Layer Conditions               |
C |       (Interpolated ETH-camp DATA)                                     |
C |                                                                        |
C |        Adaptation from MARdir/ETH-Cp/SBCnew.1G0                        |
C |                                                                        |
C | # OPTIONS: #di: double initialisation                                  |
C |   ^^^^^^^                                                              |
C +------------------------------------------------------------------------+


      Implicit None


C +--General Variables
C +  =================

      include 'MARCTR.inc'
      include 'MARphy.inc' 
      include 'MARdim.inc'
      include 'MAR_SV.inc'
      include 'MARgrd.inc'
      include 'MAR_GE.inc'
      include 'MAR_DY.inc'
      include 'MAR_HY.inc'
      include 'MAR_RA.inc'

      include 'MAR_SL.inc'
      include 'MAR_TV.inc'
      include 'MARdSV.inc'
      include 'MARsSN.inc'
      include 'MAR_SN.inc'
      include 'MAR_BS.inc'
      include 'MAR_IB.inc'

      real     avtemIB(mx,my)
      real     avwinIB(mx,my)

      include 'MAR_IO.inc'

      logical      ETH_1990, SBCnew_in_INIphy,ETH_camp
      common/ETH_L/ETH_1990, SBCnew_in_INIphy,ETH_camp

      character*6  index

      integer      Nb_Lay  !Number of layers in ObsETH.dat
      integer      Nb_SnoLay

      real         ETH_1(9)
      real         ETH_2(9)
      real         Temper_ETH(9)
      real         Precip_ETH   
      real         totime
      real, Allocatable :: dz_ice__ini(:)  !Model Layers 
      real, Allocatable :: dzIICE(:), ttIICE(:),  roIICE(:) !Obs
      real, Allocatable :: dz_snow_ini(:)  !Model Layers
      real, Allocatable :: dz_SNOW(:),tt_SNOW(:), ro_SNOW(:) !Obs
      real, Allocatable :: wa_SNOW(:),gs_SNOW(:) !Obs
c      real         dz_SNOW(12),tt_SNOW(12),ro_SNOW(12)
c      real         wa_SNOW(12)      ,gs_SNOW(12) 
      real         snwae (mg)        ,smb000
      real         Height_snow_90(80),Height_snow_91(78)   
      real         albedo______90(79),albedo______91(77)   
      real         Bilan_masse_90(80),Bilan_masse_91(78)
      real         temp_z_mes(15)
      real         temp_mod(15)
      real         Height, Difference, qsa, qsat0D
      real         uairDY_plus, tsrf, height_snow
      real         lower, upper,middlelayer,lowerborder,dtemp
      real         drho,dwat,dgss,tairav,dsqr 
      real         zSNOW(0:mg)
      real         zicetotal
      real         zice_step
      real         zsnototal   , Snow___Depth, total
      real         Mean_Density, MeanLiqWater, dopsno
      real         alpha1, alpha2, alpha3, SRFdia_sno
      real         upper_ice,ab_upper,ab_middle
      real         SaltMx,SaltMo,SaltSU,Salt_U,Por_BS

      integer      nb_iice, limite , nb_snow, saison_ETH, n
      integer      ni, nj, nm, nk, pourcent,teller1,teller2
      integer      Type_of_Precip_ETH, dt_ETH_1, dt_ETH_2,Temper_ETH2
      integer      yr1,mo1,dd1,hh1,yr2,mo2,dd2,hh2

      common /ETHi/dt_ETH_1, dt_ETH_2,saison_ETH,
     .             Type_of_Precip_ETH,Temper_ETH2
      common /ETHr/Precip_ETH,ETH_1,ETH_2,Temper_ETH,smb000,totime

      real         aIcdSV
      common /ETHa/aIcdSV

      real         TimeNC
      double precision  totTIM ! total simulation time, minutes.
      real         forcInt
      integer      force_dt

      real         agsSNo1

      logical      exactProf

C +   DATA
C +   ====

      data    ETH_camp /.true./

      data    tairav/  -8.65d00/    
C +...        tairav: Ice Temperature at 10 m for the 3.06.1991 at ETH-camp  

      data    SaltMx  /-5.83e-2/
C +...        SaltMx: Maximum Snow Mobility affecting Erodibility   [kg/m3]

C +   Hauteur de la couche de neige observ?es en 1990 & 1991

      data Height_snow_90/113.8,112.9,111.4,110.9,109.0,107.4,106.3,
     .                    101.6, 97.5, 93.3, 91.3, 87.9, 85.3, 81.4,
     .                     79.0, 76.7, 74.0, 72.3, 69.1, 67.5, 63.8,
     .                     61.0, 58.6, 56.4, 55.5, 54.9, 51.5, 48.5,
     .                     48.1, 47.3, 44.7, 41.7, 37.0, 32.7, 29.1,
     .                     24.5, 22.5, 21.3, 20.5, 20.5, 19.7, 18.4,
     .                     15.4, 10.0,  6.8,  1.2,  3.6,  0.0, -2.6,
     .                     -4.8, -8.6, -9.2, -9.6,-10.2,-12.6,-13.6,
     .                    -18.0,-18.2,-18.0,-17.6,-19.6,-18.4,-18.2,
     .                    -18.2,-18.4,-18.2,-19.0,-19.0,-19.4,-18.2,
     .                    -18.6,-19.0,-19.2,-19.2,-19.8,-20.2,-20.2,
     .                    -20.8,-15.2,-13.2/

      data Height_snow_91/163.5,160.4,158.0,154.8,152.6,152.1,150.8,
     .                    150.1,149.1,147.3,147.1,146.8,145.6,146.1,
     .                    143.4,138.1,133.8,129.9,127.1,124.9,123.0,
     .                    120.4,117.9,126.4,118.5,116.3,116.6,114.4,
     .                    112.8,113.3,109.8,108.6,113.6,108.3,106.5,
     .                    103.1, 99.6, 96.3, 94.1, 90.4, 86.5, 83.6,
     .                     81.9, 79.8, 75.9, 74.0, 70.5, 66.9, 64.1,
     .                     62.6, 61.5, 59.9, 57.6, 55.6, 54.4, 57.4,
     .                     60.4, 59.8, 57.9, 55.3, 50.3, 47.3, 48,
     .                     48.8, 46.9, 46.3, 45.6, 43.9, 43.4, 43.3,
     .                     42.5, 48.3, 47.1, 48.4, 49.0, 49.5, 51.5,
     .                     58.5/

      data Albedo______90/
     .               75,72,72,75.5,79,74,74,74,72,72,72,72,72,72,71,
     .               71,72,71,71  ,71,70,71,75,73,72,70,72,71,70,73,
     .               71,70,70,71  ,70,74,70,62,69,68,66,69,63,60,48,
     .               50,52,54,55  ,56,63,68,67,72,65,58,64,76,84,69,
     .               80,81,76,77  ,76,76,77,75,81,78,76,75,74,73,71,
     .               70,69,89,87/

C      data Albedo______90/
C     .               0.,0.,0.,0.  ,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
C     .               0.,0.,0.,0.  ,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
C     .               0.,0.,0.,0.  ,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
C     .               0.,0.,0.,0.  ,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
C     .               0.,0.,0.,0.  ,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
C     .               0.,0.,0.,0./

      data Albedo______91/
     .               77.3,76.3,79.2,76  ,75.1,75.3,77.6,74.8,77.4,73.7,
     .               74.9,72.9,73.2,73.4,76.9,75  ,74.9,72.8,79.8,81.7,
     .               79  ,78.2,86  ,76.1,77  ,77  ,85  ,78.1,83.1,74.8,
     .               80.6,91.2,77.9,72.4,69.9,71.2,71.4,71.1,71.8,71.4,
     .               71.4,71  ,73.8,71  ,71.7,71.3,71.9,71.8,71  ,70.7,
     .               70.4,70.7,69.7,70.5,90  ,83.3,81.1,77.9,78.3,72.9,
     .               79  ,67.3,86.2,84.1,82  ,77.3,74.1,76.1,79.8,74.4,
     .               88.9,86  ,87.2,87.8,88.8,92  ,92/

      data Bilan_masse_90/45.12,45.02,44.97,44.89,44.77,44.73,44.66,
     .                    44.55,44.44,44.36,44.28,44.21,44.08,43.83,
     .                    43.48,43.08,42.68,42.24,41.80,41.36,40.76,
     .                    40.08,39.35,39.03,38.27,37.67,37.17,36.61,
     .                    35.95,35.43,34.95,34.27,33.42,32.42,31.38,
     .                    30.12,28.91,27.66,26.41,25.26,24.20,23.17,
     .                    22.27,18.62,14.06,10.16,9.79,4.81,0.50,-3.83,
     .                    -7.76,-8.00,-11.26,-10.97,-12.92,-13.80,
     .                    -16.33,-17.12,-16.86,-14.92,-16.85,-14.84,
     .                    -14.92,-15.03,-15.05,-15.05,-15.06,-15.09,
     .                    -15.15,-15.18,-15.20,-15.21,-15.22,-15.24,
     .                    -15.24,-15.26,-15.26,-15.27,-14.86,-14.66/

      data Bilan_masse_91/67.20,67.10,67.80,68.20,67.90,68.40,68.00,
     .                    67.30,66.70,66.50,66.50,66.30,66.00,66.70,
     .                    65.90,64.90,65.50,66.50,67.70,69.00,70.40,
     .                    71.20,71.90,78.80,76.70,76.40,76.90,76.60,
     .                    75.20,76.50,74.40,73.50,75.30,71.90,69.20,
     .                    67.10,65.90,64.20,63.30,62.10,60.70,58.50, 
     .                    58.70,58.10,56.40,56.00,54.60,52.80,51.60,
     .                    51.40,51.20,50.80,50.00,49.30,48.90,50.60,
     .                    52.30,52.20,51.60,50.80,48.60,48.60,48.70,
     .                    48.80,48.10,47.70,47.10,46.50,44.90,44.40,
     .                    44.30,45.30,44.90,45.30,45.50,45.60,46.20,
     .                    48.30/

      data    temp_z_mes/10.0,8.0,6.0,5.0, 4.0,3.0,
     .                    2.5,2.0,1.5,1.25,1.0,0.75,
     .                    0.5,0.25,0.0/

C +           temp_z_mes: pronfondeur en m des sondages


C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ++ A. INITIALISATION  ++++++++++++++++++++++++++++++++++++++++++++
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      ni=1 ! mx
      nj=1 ! my
      nm=1 ! nsx
C +
C +
      IF (itexpe.eq.0.and.dt_ETH_2.ne.-1) THEN    

        write (*,*)
        write (*,*) 'Initialization ...'
        write (*,*) '======================='
        write (*,*) 

C +      	
C +      	
C +---1) Time Step (Specification)
C +   ============================
C +
        Open (unit=30, status='old',file='sisvat.ctr')
        call ctrPrep(30,'dt physics (s)')
        read (30,*) dtPhys
c        dtPhys = 360.
        dt     = dtPhys
C +
C +      	
C +---2)Blowing Snow Model Initialisation (Constants)
C +   ===============================================
C +
        VVs_BS      = 10.0
        RRs_BS      =  1.0
        FacRBS      =            2.868d00
        FacSBS      = 1.000d00 / FacRBS
        FacTBS      =            0.085d00
        FacUBS      = 1.000d00 / FacTBS
C +
        g1__BS      =  99.00d00
        g2__BS      =  DScdSV
C +
        SaltMo      = -0.833d-2 * g1__BS - 0.583d-2 * g2__BS + 0.833d00
        SaltSU      = (1.000d00 + SaltMo)           * FacSBS
        Salt_U      =        -log(SaltSU)           * FacUBS
C +...  Salt_U      :  Guyomarc'h & Merindol (1997), Ann. Glac.
C +
        Por_BS      =  1.000 - blsno/ro_Ice
        SheaBS      =                Por_BS/(unun-Por_BS)
C +...  SheaBS ==> Arg(sqrt(shear)) with shear the max shear stress in snow:
C +     shear       =  3.420 * exp(-(Por_BS      +Por_BS)
C +  .                             /(unun        -Por_BS))
C +     SheaBS      :  see de Montmollin         (1978),
C +                    These Univ. Sci. Medic. Grenoble, Fig. 1 p. 124
C +
C +---3) Initialisation des variables
C +   ===============================
C +
        aIcdSV      = aI1dSV*So1dSV+aI2dSV*So2dSV+aI3dSV*So3dSV
c #AI   aIcdSV      = 0.58    ! Bare  Ice Albedo

        call  ctrPrep(30,'Initial Ice Layers')       
        read (30,*) nb_iice   ! Number of initial ice layers.
        call  ctrPrep(30,'Total Layers')
        read (30,*) nb_snow   ! Number of initial snow and ice layers.
        call  ctrPrep(30,'Initial Ice Thickness (m)')
        read (30,*) zicetotal ! Initial ice thickness [m].       
        call  ctrPrep(30,'Minimum Layer Thickness (m)') 
        read (30,*) zice_step ! Minimum layer thickness [m].
 
c        nb_iice     = 15      ! Nbr de couches de glace initiales
c        nb_snow     = 35      ! Nbr de couches de glace + neige initiales
c        zicetotal   = 20.0    ! Epaisseur de la couche de glace [m]
c        zice_step   = 0.03    ! Epaisseur minimum [m]

c        Open (unit=30, status='unknown',file ='sISvat.ctr')
c        read      (30,*)                       saison_ETH
c 99     format(i4)      
c        close(unit=30)
c        saison_ETH = 1990 ! TEMPORARY...
c        ETH_1990 = .true.

        call ctrPrep(30,'Initial Snow Thickness (m)')
        read (30,*) zsnototal ! Initial snow thickness [m].

        write (*,*) "Total Layers: ",nb_snow
        write (*,*) "Initial Ice Layers: ",nb_iice
        write (*,*) "Initial Ice Thickness: ",zicetotal
        write (*,*) "Initial Snow Thickness: ",zsnototal
        write (*,*) "Minimum Layer Thickness: ",zice_step        

        call ctrPrep(30,'Number of Input Layers')
        read (30,*) Nb_Lay    ! Number of layers in ObsETH.dat
        write (*,*) "Number of input layers: ",Nb_Lay   
  
        call ctrPrep(30,'Input File Snow Layers')
        read (30,*) Nb_SnoLay ! Number of snow layers in ObsETH.dat
        write (*,*) "Number of input snow layers: ",Nb_SnoLay

        call ctrPrep(30,'Exact Profile')
        read (30,*) exactProf
      
        if (exactProf) then
           if ((nb_snow.ne.Nb_Lay).or.
     .          (nb_iice.ne.(Nb_Lay-Nb_SnoLay))) then
             write (*,*) "ERROR! Snow model + input layers don't match!"
             STOP "@#$!  EMERGENCY EXIT"
           end if
           write (*,*) "Exact input profile will be used."
        end if        

C       Date/Time Info.
        call ctrPrep(30,'START DATE (YYYY,MM,DD,HH)')
        read (30,*) yr1,mo1,dd1,hh1

        call ctrPrep(30,'END DATE (YYYY,MM,DD,HH)')
        read (30,*) yr2,mo2,dd2,hh2
      
        call ctrPrep(30,'Output Interval (min)')
        read (30,*) TimeNC
        TimeNC = TimeNC*60.

        call simlength(yr1,mo1,dd1,hh1,yr2,mo2,dd2,hh2,totTIM)
        nprint = totTIM*60./TimeNC
        jda0GE = dd1
        iyr0GE = yr1
        nterun = totTIM*60./dtPhys
C +        
c        IF (saison_ETH.eq.1990)                                     THEN
c          ETH_1990  = .true.
c          zsnototal =    1.138 ! Epaisseur initilale de la couche de neige
c          Nb_SnoLay =    7     ! Nbr de couches de neige dans 'ObsETH.dat'
c          nprint    = 3792     ! Fichier MARctr.dat                             
c          jda0GE    =   12     ! Fichier MARdom.dat 
c          iyr0GE    = 1990     !
c        ELSE
c          ETH_1990  = .false.
c          zsnototal =    1.635
c          Nb_SnoLay =   12
c          nprint    = 3696
c          jda0GE    =    3          
c          iyr0GE    = 1991          
c          cld_SL(1,1) =  3./8.
c        END IF  
c          nterun    = nprint*5   

        write(*,*) 'Years simulated:', iyr0GE,'to',yr2

C +---4) Epaisseurs initiales
C +   =======================

        write(*,*) 'Thickness initialization'

      ! Ice
      ! ---  
        if (nb_iice .gt. 0)  ALLOCATE( dz_ice__ini(nb_iice) )

        IF (exactProf .neqv. .True.) THEN
          if (nb_iice .gt. 0) then
 1019       CONTINUE   
            dz_ice__ini(nb_iice) = zice_step
            Snow___Depth    = zice_step
            limite          =        nb_iice 
            DO n= nb_iice-1,1,-1
              dz_ice__ini(n) =                dz_ice__ini(n+1) *2.0
              Snow___Depth   = Snow___Depth + dz_ice__ini(n)
              if (Snow___Depth .ge. zicetotal)                     then 
                limite = n
                total  = Snow___Depth
                goto 1020 
              end if 
            END DO
 1020       CONTINUE
  
            if (Snow___Depth .ge. zicetotal)                       then
              if (limite.eq. 1)                                    then
                    dz_ice__ini(1)      = zicetotal 
     .                                  -(total - dz_ice__ini(1))
              else 
                    dz_ice__ini(limite) = zicetotal 
     .                                  -(total - dz_ice__ini(limite))
                  do n = 1,limite-1
                    dz_ice__ini(n)      = 0.0
                  end do    
                if (dz_ice__ini(limite) .lt.    dz_ice__ini(limite+1)) 
     .          then 
                    dz_ice__ini(limite)   =
     .             (dz_ice__ini(limite) + dz_ice__ini(limite+1)) * 0.5
                    dz_ice__ini(limite+1) = dz_ice__ini(limite)
                end if
                  do n = 0,limite-2
                    dz_ice__ini(2*n+1)  = dz_ice__ini(limite+n)  * 0.5
                    dz_ice__ini(2*n+2)  = dz_ice__ini(limite+n)  * 0.5
                  end do
              end if
            else
                    zice_step = zice_step + 0.01
              goto 1019
            end if 
          end if    !Zero ice layers query.

      ! Snow
      ! ----
          if (nb_iice .lt. nb_snow) then !Num. ice layers < total layers
           ALLOCATE( dz_snow_ini(nb_snow-nb_iice) )
           DO n= (nb_snow-nb_iice) ,1 ,-1
                  dz_snow_ini(n) = zsnototal / (nb_snow - nb_iice)
           END DO
          end if    !Zero snow layers query.
        END IF

C +---5) Initialisations de la couche de gla?e
C +   ========================================

        IF (nb_iice .gt. 0) THEN
          write(*,*) 'Ice layer initialization'     

          ! Lecture des profils de T?, densit?, ... dans ObsETH.dat
          ! ------------------------------------------------------- 

C +       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C +       Pour se placer au bon endroit dans le fichier ObsETH.dat
          OPEN(unit=40,status='old',file='sisvat.dat')
 33       CONTINUE
          READ (40,100) index
 100      FORMAT(a6)
c          IF (ETH_1990)                                               THEN
c            IF (index .eq. 'ICE_90')                              GO TO 34
c          ELSE
c            IF (index .eq. 'ICE_91')                              GO TO 34
c          END IF
          IF (index .eq. 'ICE_ST')                             GO TO 34
          GO TO 33
 34       CONTINUE
          DO i = 1,6
            READ (40,*)   
          END DO
C +     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ALLOCATE (dzIICE(Nb_Lay-Nb_SnoLay))
          ALLOCATE (ttIICE(Nb_Lay-Nb_SnoLay))
          ALLOCATE (roIICE(Nb_Lay-Nb_SnoLay))

          READ (40,*) (dzIICE(n), ttIICE(n), roIICE(n),
     .     n=1,(Nb_Lay-Nb_SnoLay))
          CLOSE(40)  
       
c        write (*,*) "Nb_Lay-Nb_SnoLay: ", (Nb_Lay-Nb_SnoLay)
c        write (*,*) "roIICE: "
c        write (*,*) roIICE

        ! Interpolation des donn?es aux couches MAR
        ! ----------------------------------------- 
          if (exactProf) then 
             do nk=nb_iice, 1, -1 
               dz_ice__ini(nk) = dzIICE(nb_iice-nk+1)
               dzsSNo(ni,nj,nm,nk) = dz_ice__ini(nk)
               tisSNo(ni,nj,nm,nk) = ttIICE(nb_iice-nk+1) + tfsnow
               rosSNo(ni,nj,nm,nk) = ro_Ice  !Density
               g1sSNo(ni,nj,nm,nk) = 99.0    !Sphericity
               g2sSNo(ni,nj,nm,nk) = 99.0    !Grain size
               nhSSNo(ni,nj,nm,nk) = 0       !Historicity
             end do
c            write (*,*) "rosSNo"
c            write (*,*) rosSNo
          else
           middlelayer      =  0.0
           lowerborder      =  0.0
           do nk=nb_iice,1,-1
            dzsSNo(ni,nj,nm,nk) =  dz_ice__ini(nk)

            middlelayer    =  lowerborder + dzsSNo(ni,nj,nm,nk)*0.5
            lowerborder    =  lowerborder + dzsSNo(ni,nj,nm,nk)

            do n=1,(Nb_Lay-1)
             upper= dzIICE(n)
             lower= dzIICE(n+1)
             if (middlelayer.lt.lower .and. middlelayer.ge.upper) then
              dtemp               = ttIICE(n)   - ttIICE(n+1)
              tisSNo(ni,nj,nm,nk) = ttIICE(n+1) + tfsnow
     .                            + dtemp * (lower - middlelayer)
     .                                    / (lower - upper      )
             end if 
             if (middlelayer.gt.dzIICE(Nb_Lay))                   then
              tisSNo(ni,nj,nm,nk) = tfsnow      + ttIICE(15)
             end if 
            end do

              rosSNo(ni,nj,nm,nk) =  ro_Ice ! Densit?
              g1sSNo(ni,nj,nm,nk) =  99.0   ! Sph?ricit?
              g2sSNo(ni,nj,nm,nk) =  99.0   ! Taille des grains
              nhsSNo(ni,nj,nm,nk) =  0      ! Historicit?
            end do
          end if
        END IF  !Check for presence of ice layers.
C + 	
C +      	
C +---6) Initialisations de la couche de neige
C +   ========================================
C +
        IF (nb_iice .lt. nb_snow) THEN  ! Num. ice layers < tot. layers
          write(*,*) 'Snow layer initialization'    
C +
        ! Lecture des profiles de T?, densit?, ... dans ObsETH.dat
        ! -------------------------------------------------------- 
C +      
C +       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
C +       Pour se placer au bon endroit dans le fichier ObsETH.dat
          OPEN(unit=40,status='old',file='sisvat.dat')
 35       CONTINUE
          READ (40,100) index
c         IF (ETH_1990) THEN
c           IF (index .eq. 'SNO_90') GO TO 36
c         ELSE
c           IF (index .eq. 'SNO_91') GO TO 36
c         END IF
          IF (index .eq. 'SNO_ST') GO TO 36
          GO TO 35
 36       CONTINUE
          DO i = 1,6
            READ (40,*)   
          END DO
C +       -----------------------------------------------------------
C +
          ALLOCATE (dz_SNOW(Nb_SnoLay))
          ALLOCATE (tt_SNOW(Nb_SnoLay))
          ALLOCATE (ro_SNOW(Nb_SnoLay))
          ALLOCATE (wa_SNOW(Nb_SnoLay))
          ALLOCATE (gs_SNOW(Nb_SnoLay))

          READ (40,*) (dz_SNOW(n),tt_SNOW(n),ro_SNOW(n),
     .                 wa_SNOW(n),gs_SNOW(n),n=1,Nb_SnoLay)
          CLOSE(40)
c         write (*,*) "Nb_SnoLay: ", Nb_SnoLay
c         write (*,*) "ro_SNOW:"
c         write (*,*) ro_SNOW
c         write (*,*) "nb_snow: ",nb_snow
c         write (*,*) "nb_iice: ",nb_iice
C +
        ! Interpolation des donn?es aux couches MAR
        ! -----------------------------------------
C +     
          if (exactProf) then
             do nk=nb_snow,nb_iice+1,-1
                tisSNo(ni,nj,nm,nk) = tt_SNOW(nb_snow-nk+1) + tfsnow
c                write (*,*) "snow index: ", (nb_snow-nk+1)
c                write (*,*) "density: ", ro_SNOW(nb_snow-nk+1)
                dzsSNo(ni,nj,nm,nk) = dz_SNOW(nb_snow-nk+1)
                rosSNo(ni,nj,nm,nk) = ro_SNOW(nb_snow-nk+1)
                wasSNo(ni,nj,nm,nk) = wa_SNOW(nb_snow-nk+1)
                g2sSNo(ni,nj,nm,nk) = gs_SNOW(nb_snow-nk+1)
                g2sSNo(ni,nj,nm,nk) = g2sSNo(ni,nj,nm,nk)* 10.0
                g1sSNo(ni,nj,nm,nk) =                      99.0
                nhsSNo(ni,nj,nm,nk) =                         0
              
                if (rosSNo(ni,nj,nm,nk) > epsi) then
                   wasSNo(ni,nj,nm,nk) = wasSNo(ni,nj,nm,nk)*1000.0 !Water
     .                                 / rosSNo(ni,nj,nm,nk)        !Content
c                write (*,*) "epsilon: ", epsi
                else
                   wasSNo(ni,nj,nm,nk) = 0.0
                end if 
                if (wasSNo(ni,nj,nm,nk).gt.0.0)
     .                                nhsSNo(ni,nj,nm,nk)=istdSV(2)
             end do
c             write (*,*) "rosSNo:"
c             write (*,*) rosSNo
          else 
            middlelayer      =  0.0
            lowerborder      =  0.0
            do nk=nb_snow,nb_iice+1,-1
             dzsSNo(ni,nj,nm,nk) = dz_snow_ini(10)
             middlelayer      = lowerborder +dzsSNo(ni,nj,nm,nk)*0.5
             lowerborder      = lowerborder +dzsSNo(ni,nj,nm,nk)
             do n=1,(Nb_SnoLay-1)
              upper= dz_SNOW(n)
              lower= dz_SNOW(n+1)
              if (middlelayer.lt.lower .and. middlelayer.ge.upper) then
                  dtemp= tt_SNOW(n)   - tt_SNOW(n+1)
                  tisSNo(ni,nj,nm,nk) = tt_SNOW(n+1) + tfsnow
     .                                + dtemp *(lower - middlelayer)
     .                                        /(lower - upper      )
                  drho = ro_SNOW(n)   - ro_SNOW(n+1)
                  rosSNo(ni,nj,nm,nk) = ro_SNOW(n+1)
     .                                + drho  *(lower - middlelayer)
     .                                        /(lower - upper      )
                  dwat = wa_SNOW(n)   - wa_SNOW(n+1)
                  wasSNo(ni,nj,nm,nk) = wa_SNOW(n+1)
     .                                + dwat  *(lower - middlelayer)
     .                                        /(lower - upper      )
                  dgss = gs_SNOW(n)   - gs_SNOW(n+1)
                  g2sSNo(ni,nj,nm,nk) = gs_SNOW(n+1)
     .                                + dgss *(lower  - middlelayer)
     .                                       /(lower  - upper      )
              end if
             end do
             g1sSNo(ni,nj,nm,nk) =                       99.0
             g2sSNo(ni,nj,nm,nk) = g2sSNo(ni,nj,nm,nk)*  10.0
             nhsSNo(ni,nj,nm,nk) =                          0
             wasSNo(ni,nj,nm,nk) = wasSNo(ni,nj,nm,nk)*1000.0 !Water
     .                             / rosSNo(ni,nj,nm,nk)      !Content
             if (wasSNo(ni,nj,nm,nk).gt.0.0) 
     .                                   nhsSNo(ni,nj,nm,nk)=istdSV(2)
            end do
          end if
C +     
        END IF !Check for snow layers...

        tsrfSL(ni,nj,1)  =   tisSNo(ni,nj,nm,nb_snow)
        tairSL(ni,nj)    =   tsrfSL(ni,nj,1)
        pktaDY(ni,nj,mz) =   tairSL(ni,nj)/(88.0d0**cap)
        zSNOW(mg) = dzsSNo(ni,nj,nm,mg) !CHECK MG!!!!!!!!!!!!
        snwae(mg) = rosSNo(ni,nj,nm,mg)*dzsSNo(ni,nj,nm,mg)
     .              * 1.d3               /ro_Wat
        do nk=mg-1,1,-1
          zSNOW(nk) = dzsSNo(ni,nj,nm,nk)+ zSNOW(nk+1)
          snwae(nk) = rosSNo(ni,nj,nm,nk)*dzsSNo(ni,nj,nm,nk)
     .                * 1.d3               /ro_Wat
     .                + snwae(nk+1)
        end do
C +
C +
C +---7) Correction de l'initialisation (Couche de neige)
C +   ===================================================
C +
            d1_SL(ni,nj)        = 0.6257d+6       ! rhos*cs*(Depth diurnal Wave)
                                                  ! [J/m2/K]
            t2_SL(ni,nj)        = tfsnow + tairav ! Soil Deep Layers Temperature
C +
            nssSNo(ni,nj,nm)    = nb_snow
            nisSNo(ni,nj,nm)    = nb_iice
C +
        if (nhsSNo(ni,nj,nm,mg).le.istdSV(1))                     then
             ro_SL(ni,nj)       =  0.d0           ! Densit? de surface
        else
             ro_SL(ni,nj)       =  rosSNo(ni,nj,nm,mg)
        end if
C +
C +
C +---8) Blowing Snow Model Parameters
C +   ================================
C +
        IF     (nhsSNo(ni,nj,nm,mg-1).le.istdSV(1))               THEN ! CTR
                 ro_SL(ni,nj) =  0.d0
C +
          IF   (g1sSNo(ni,nj,nm,mg-1).lt.0.d0)                    THEN ! CTR
                SaltMo        = -0.750e-2*g1sSNo(ni,nj,nm,mg-1)
     .                          -0.500e-2*g2sSNo(ni,nj,nm,mg-1)+0.500
C +...          SaltMo        :  Guyomarc'h & Merindol, 1998, Ann. Glac.
C +                    CAUTION:  Guyomarc'h & Merindol Dendricity Sign is +
C +                    ^^^^^^^^                    MAR Dendricity Sign is -
          ELSE                                                         ! CTR
                SaltMo        = -0.833e-2*g1sSNo(ni,nj,nm,mg-1)
     .                          -0.583e-2*g2sSNo(ni,nj,nm,mg-1)+0.833
          END IF                                                       ! CTR
C +
          IF   (nhsSNo(ni,nj,nm,mg-1) .ge.istdSV(2))
     .          SaltMo        =       min(SaltMo,SaltMx)
C +
                SaltSU        =  (1.000 + SaltMo)         * FacSBS
                Salt_U        =      -log(SaltSU)         * FacUBS
C +...          Salt_U        :  Guyomarc'h & Merindol, 1997, Ann. Glac.
C +
                SaltSL(ni,nj) =           Salt_U / 26.5
C +...          Us(U10)       :  Budd et al.            1966, Ant.Res.Ser.9
C +                         (see Pomeroy & Gray 1995 NHRI Sci.Rep.7(30)p.62)
C +
        ELSE                                                           ! CTR
                 ro_SL(ni,nj) =  rosSNo(ni,nj,nm,mg-1)
                SaltSL(ni,nj) =  1.e2
        END IF                                                         ! CTR
C +     	
C +     	
C +---9) Surficial Water and Fall Line Slope
C +   ======================================
C +
        IF (dzsSNo(ni,nj,nm,1).gt.0.)                             THEN ! CTR
            SWaSNo(ni,nj,nm) =  zero 
            call ctrPrep(30,'SURFACE SLOPE')
            read (30,*) slopTV(ni,nj)
            write (*,*) "Surface Slope: ", slopTV(ni,nj)
c            slopTV(ni,nj)    =  0.02
        END IF
       
C +	
C +	
C +---10) Age de la neige
C +   ====================
C +	
        write(*,*) 'Snow age and albedo initialization'
C +
        call ctrPrep(30,'Initial Snow Age')
        read (30,*) agsSNo1
        write (*,*) "Init. Snow Age: ",agsSNo1

        DO nk=nb_snow,nb_iice+1,-1
            agsSNo(ni,nj,nm,nk)      = agsSNo1
        END DO

        call ctrPrep(30,'Top Initial Snow Age')
        read (30,*) agsSNo(ni,nj,nm,nb_snow)
        write (*,*) "Top Layer Init. Age: ",agsSNo(ni,nj,nm,nb_snow)    
  
c        IF (ETH_1990) THEN
c            agsSNo(ni,nj,nm,nb_snow) =  2.0
c        ELSE
c            agsSNo(ni,nj,nm,nb_snow) =  5.0
c        END IF
        
C +
C +
C +---11) Taille des grains pour le calcul de l'albedo
C +   ================================================
C +
c #xf   IF (dzsSNo(ni,nj,nm,1).gt.0.d0)                           THEN ! CTR
C +       	
C +                   **********
c #xf       dopsno =  SRFdia_sno(g1sSNo(ni,nj,nm,nb_snow),
c #xf.                           g2sSNo(ni,nj,nm,nb_snow))
C +                   **********
C +
c #xf       dsqr   = sqrt(dopsno)
C +
C +         ***************
c #xf       call SRFalb_sno(alb0SL(ni,nj),alpha1,alpha2,alpha3,
c #xf.                      dopsno,dsqr,
c #xf.                      rosSNo(ni,nj,nm,nssSNo(ni,nj,nm)),
c #xf.                      wasSNo(ni,nj,nm,nssSNo(ni,nj,nm)),
c #xf.                      agsSNo(ni,nj,nm,nssSNo(ni,nj,nm)),
c #xf.                      zero, zero, zero)
C +         ***************
C +	  
c #xf       alb0SL(ni,nj)   = alb0SL(ni,nj)
c #xf.               + 0.05 *(cld_SL(ni,nj)-0.50)
c #xf       alb0SL_1(ni,nj) = alb0SL(ni,nj)
c #xf       alb0SL_2(ni,nj) = alb0SL(ni,nj)
c #xf       alb0SL_3(ni,nj) = alb0SL(ni,nj)
C +
c #xf   END IF
C +
        write(*,*) 'Output of initialization'  
C +
C +
C +---12) Output de l'initialisation
C +   ==============================
C +   
          Mean_Density  = 0.0
          MeanLiqWater  = 0.0
          Snow___Depth  = 0.0
        DO nk=nb_snow,nb_iice+1,-1
          Mean_Density  = Mean_Density
     .                  + dzsSNo(ni,nj,nm,nk)*rosSNo(ni,nj,nm,nk)
          MeanLiqWater  = MeanLiqWater
     .                  + dzsSNo(ni,nj,nm,nk)*rosSNo(ni,nj,nm,nk)
     .                                       *wasSNo(ni,nj,nm,nk) 
          Snow___Depth  = Snow___Depth      + dzsSNo(ni,nj,nm,nk)
        END DO
          Mean_Density  = Mean_Density/(Snow___Depth)
          MeanLiqWater  = MeanLiqWater/(Snow___Depth)/1000.0
C +
        open (unit=50, status ='replace', file='Data____ETH.ini')
C +	
        write (50,*)
     .    "Snow/Ice Profile: Initial Conditions"
c        IF (ETH_1990) THEN
c          write(30,*) 
c     .   "Conditions Initiales du ETH_Camp: le 12/06/1990 ? 00:00"
c        ELSE
c          write(30,*) 
c     .   "Conditions Initiales du ETH_Camp: le 03/06/1991 ? 00:00"
c        END IF

          write(50,*) 
     .   "======================================================="
          write(50,401)
 401      format(/,' Internal Characteristics',
     .           /,' ========================',
     .       /,'  n |  z    |  dz   |   T    | rho   |  W    |',
     .      ' z(WE) | Age   | Extin |  UW   | Dendr.| Spher.| Hist. |',
     .       /,'    | [m]   | [mm]  |  [K]   | kg/m3 | kg/kg |',
     .      '  [mm] | [d]   |       | mim/s | /Sphe.| /Size |       |',
     .       /,'----+-------+-------+--------+-------+-------+',
     .      '-------+-------+-------+-------+-------+-------+-------+')
          write(50,402)
     .     (nk,zSNOW(nk),1.e3*dzsSNo(ni,nj,nm,nk),tisSNo(ni,nj,nm,nk),
     .                        rosSNo(ni,nj,nm,nk),wasSNo(ni,nj,nm,nk),
     .         snwae(nk),     agsSNo(ni,nj,nm,nk),zero  ,zero  ,
     .                        g1sSNo(ni,nj,nm,nk),g2sSNo(ni,nj,nm,nk),
     .                        nhsSNo(ni,nj,nm,nk),
     .      nk=mg,1,-1)
 402      format((i3,' |',f6.2,' |',  f6.1,' |', f7.2,' |',  f6.1,' |',
     .          f6.3,' |',f7.0, '|',  f6.1,' |', f6.3,' |',  f6.2,' |',
     .        2(f6.1,' |'),i4,'   |'))
          write(50,403) 
 403      format('----+-------+-------+--------+-------+-------+',
     .        '-------+-------+-------+',
     .        '-------+-------+-------+-------+')
          write (50,*)
          write (50,*) 'Densit? moyenne             :', Mean_Density
          write (50,*) 'Contenu en eau liquide moyen:', MeanLiqWater
        close (50)
C +
C +  
        SBCnew_in_INIphy = .false.
c #di   SBCnew_in_INIphy = .true.      
C +
        IF (SBCnew_in_INIphy) THEN    
          write(*,*) 'Call of SBCnew in INIphy' 
          dt_ETH_2 = -1       	 
          write (*,*)
          go to 404                                  ! Fin de SBCnew
c #di   ELSE
c #di     write(*,*) 'WARNING: no-Call of SBCnew in INIphy' 
c #di     write (*,*)
c #di     pause
        END IF
C +
    
      END IF
C +
C +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ++ B. INTERPOLATION  +++++++++++++++++++++++++++++++++++++++++++++
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +
C +   1) Lecture des donn?es de for?age dans le fichier ObsETH.dat  
C +   ============================================================
C +
      dt_ETH_1 = dt_ETH_1 + 1
C +
      IF (itexpe.eq.0)                                              THEN
        OPEN(unit=40,status='old',file='sisvat.dat')
        REWIND 40
 37     CONTINUE

        READ (40,100) index
        IF (index .eq. 'OBS_ME')                                GO TO 38
c        IF (ETH_1990)                                               THEN
c          IF   (index .eq. 'FOR_90')                            GO TO 38
c        ELSE                              !Pour se placer au bon endroit
c          IF   (index .eq. 'FOR_91')                            GO TO 38
c        END IF
                                                                GO TO 37
 38     CONTINUE
        DO i = 1,5
          READ (40,*) 
        END DO
C +
            read (40,*) (ETH_1(i),i=1,6),
     .                   Precip_ETH,Type_of_Precip_ETH, 
     .                  (ETH_1(i),i=7,9)
            read (40,*) (ETH_2(i),i=1,6),
     .                   Precip_ETH,Type_of_Precip_ETH, 
     .                  (ETH_2(i),i=7,9)
            ETH_1(8) =   ETH_1(8) + 273.15d0
            ETH_2(8) =   ETH_2(8) + 273.15d0 
            dt_ETH_1 = 0
c            dt_ETH_2 = 0 
      ELSE
        call ctrPrep(30,'Forcing Interval (min)')
        read (30,*) forcInt
        forcInt = forcInt * 60.

        force_dt = forcInt/dt !Num. timesteps per forcing interval.
c        write (*,*) "force_dt ", force_dt
c        write (*,*) "dt_ETH_1 ", dt_ETH_1
c        write (*,*) "itexpe ", itexpe
c        write (*,*) "nterun ", nterun
        IF (dt_ETH_1.eq.force_dt)                                   THEN
          DO i = 1, 9    
            ETH_1(i) = ETH_2(i)
          END DO
          IF (itexpe.lt.nterun-4)                                   THEN
            read (40,*) (ETH_2(i),i=1,6),
     .                   Precip_ETH,Type_of_Precip_ETH, 
     .                  (ETH_2(i),i=7,9)
            ETH_2(8) =   ETH_2(8) + 273.15d0 
c            write (*,*) "windspeed: ", ETH_2(3) 
            dt_ETH_1 = 0 
          END IF
        END IF
      END IF
c      write (*,*) ETH_2
C +
C +
C +   2) Interpolation
C +   ================
C +
      ! Radiation
      ! ---------           
C +   
      RAdsol(1,1)   = ETH_1(1) + (real(dt_ETH_1))
     .              *(ETH_2(1) -  ETH_1(1)) / (forcInt/dt)

      sol_SL(1,1)   = ETH_1(7) + (real(dt_ETH_1))
     .              *(ETH_2(7) -  ETH_1(7)) / (forcInt/dt)

      RAd_ir(1,1)   = ETH_1(2) + (real(dt_ETH_1))
     .              *(ETH_2(2) -  ETH_1(2)) / (forcInt/dt)
c #VR write(6,6600) jhurGE,minuGE,jsecGE,RAdsol,czenGE
 6600 format(3i3,f12.1,f12.6)
C +
      ! Vent  
      ! ----  
C +
      uairDY(1,1,mz)= ETH_1(3) + (real(dt_ETH_1))
     .              *(ETH_2(3) -  ETH_1(3)) / (forcInt/dt)
      uairDY(1,1,mz)= max (0.1,uairDY(1,1,mz))
      vairDY(1,1,mz)=      0.0
C +
      IF (itexpe.eq.0)                                            THEN ! CTR
         call ctrPrep(30,'Initial Monin-Obukhov Length')
         read (30,*) SLlmo(1,1)
         write (*,*) "Initial Monin-Obukhov Len.: ", SLlmo(1,1)
         
         call ctrPrep(30,'Initial Friction Velocity')
         read (30,*) sluus(1,1)
         write (*,*) "Initial Friction Velocity: ", sluus(1,1)
c        IF (ETH_1990)                                             THEN ! CTR
c            SLlmo(1,1)= -50.0
c            sluus(1,1)=   0.25  
c        ELSE
c            SLlmo(1,1)= -48.0
c            sluus(1,1)=   0.5
c        END IF
      END IF

C +	       
c      IF (jhurGE               .eq. 0 .and. minuGE .eq. 0)        THEN ! CTR
c          dt_ETH_2 = dt_ETH_2 + 1
c      END IF
C +
c      IF (ETH_1990) THEN
c          Difference = 1.24  - height_snow_90(dt_ETH_2) / 100.0
c      ELSE 
c          Difference = 1.859 - height_snow_91(dt_ETH_2) / 100.0
c      END IF
C +
          height     = 10.0d0
c          height     = 10.0d0 + Difference
C +
c          uairDY_plus not needed for non-ETH simulation.
c          uairDY_plus   = uairDY(1,1,mz) +(SLuus(1,1)/0.4)
c     .            *(log((2.0+Difference) / height)
c  #HG.             +6.0*(2.0-10.0)       / SLlmo(1,1)    
c     .                                                   )

C +
          uairDY(1,1,mz)= uairDY(1,1,mz) +(SLuus(1,1)/0.4)
     .            *(log( 2.0             / height)
c #HG.             +6.0*(2.0-height    ) / SLlmo(1,1)    
     .                                                   )
C +
c          uairDY_plus   = max(0.1,uairDY_plus   )
          uairDY(1,1,mz)= max(0.1,uairDY(1,1,mz))
C +       uairDY_plus   = min(15.,uairDY_plus   )
C +       uairDY(1,1,mz)= min(15.,uairDY(1,1,mz))
C +
           ssvSL(1,1,mz)= uairDY(1,1,mz)
      IF  (ssvSL(1,1,mz).le.0.0) ssvSL(1,1,mz)=0.1
C +
      !**************************************************************! 
      ! Le vent a ?t? mesur? ? 10 m => r?duction ? 2 m via           ! 
      ! la methode de Bintanja. Comme les tenseurs ?taient fixes     !
      ! au cours de la campagne d'observation, il a fallu introduire !
      ! une correction due ? la fonte de la couche de neige.         !  
      ! Idem en 1990 pour les senseurs de la temp et de l'humidit?   !
      !                                                              !
      ! HG,  5 Juillet 2009: suppression influence stabilit\E9 vertica.!
      !**************************************************************!      
C +     
      ! Temp?rature
      ! ----------- 
C +
        tsrf           = ETH_1(8)  +  (real(dt_ETH_1))! 
     .                 *(ETH_2(8)-ETH_1(8))/(forcInt/dt)!
c        tsrf           = tsrf/sqrt(sqrt(0.97))         ! (Emissivity = 0.97)
        tsrf           = min(tsrf, 273.16)
C +
        tairDY(1,1,mz) = ETH_1(4)  +  (real(dt_ETH_1))
     .                 *(ETH_2(4)-ETH_1(4))/(forcInt/dt)
C +
c      IF (ETH_1990)                                               THEN
c        tairDY(1,1,mz) = tsrf-uairDY(1,1,mz)/uairDY_plus
c     .                 *(tsrf-tairDY(1,1,mz))
c      END IF                                          ! Senseur fixe au cours
C +                                                   ! de la campagne d'obs.
      ! Pression
      ! --------
C + 
        pstDY(1,1) = ETH_1(6)  +  (real(dt_ETH_1))
     .             *(ETH_2(6)-ETH_1(6))/(forcInt/dt)
        pstDY(1,1) = pstDY(1,1) * 0.1                 ! [hPa] --> [kPa]
         pkDY(1,1,mz) = exp(cap *log(pstDYn(1,1)*sigma(mz)+ptopDY))
C +
      ! Humidit? sp?cifique
      ! -------------------
C +
        qvDY(1,1,mz) = ETH_1(5)  +  (real(dt_ETH_1))
     .               *(ETH_2(5)-ETH_1(5))/(forcInt/dt)
        qvDY(1,1,mz) = qvDY(1,1,mz) * 0.001           ! [g/kg] --> [kg/kg]
C +
        qsa   = qsat0D(tairDY(1,1,mz),sigma(mz),pstDY(1,1),ptopDY,1)
c      IF (ETH_1990)                              THEN ! Senseur fixe au cours
c        qvDY(1,1,mz) = qsa-uairDY(1,1,mz)/uairDY_plus ! de la campagne d'obs.
c     .               *(qsa-  qvDY(1,1,mz)) 
c        qvDY(1,1,mz) =       qvDY(1,1,mz) * 1.1       ! Correction humidit?
c      ELSE 
        qvDY(1,1,mz) =       qvDY(1,1,mz) * 1.0       ! Correction humidit?
c      END IF
        qvDY(1,1,mz) = min  (qvDY(1,1,mz), qsa)
C +
      ! Temperature potentielle
      ! -----------------------
C +
        pktaDY(1,1,mz) =   tairDY(1,1,mz)
     .               /    ((pstDY(1,1)*sigma(mz)+ptopDY)**cap)
C +	
      ! Precipitation
      ! -------------
C +
        precSL(1,1) = (0.001*Precip_ETH*.9) /(forcInt/dt) !.9 factor is temporary!!
C +
      IF (Type_of_Precip_ETH.eq.7) THEN
        snowHY(1,1) = snowHY(1,1) + precSL(1,1)
      ELSE
        rainHY(1,1) = rainHY(1,1) + precSL(1,1)
      END IF  
C +
      ! N?bulosit?
      ! ----------
C +
          cld_SL(1,1) = ETH_1(9)  +  (real(dt_ETH_1))
     .                *(ETH_2(9)-ETH_1(9))/(forcInt/dt)
      IF (cld_SL(1,1) .gt. 8.0 .or. cld_SL(1,1) .lt. 0.0)         THEN
          cld_SL(1,1) =              4.0
      END IF
          cld_SL(1,1) = cld_SL (1,1)/8.0              ! octet => %
C +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ++ C. OUTPUT  ++++++++++++++++++++++++++++++++++++++++++++++++++++
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +
      ! Open files
      ! ----------
C +
      IF (itexpe.eq.0)                                            THEN
        open (unit=31, status='replace', file='Output__ETH.out')
        rewind     31
        open (unit=32, status='replace', file='T_surf__ETH.out')
        rewind     32 
        open (unit=33, status='replace', file='sondage_ETH.out')
        rewind     33 
        open (unit=34, status='replace', file='moyenne_ETH.out')
        rewind     34 
        open (unit=35, status='replace', file='Output__ETH.ferret')
        rewind     35                                ! For FERRET v5.22
c        open (unit=36, status='replace', file='Debugg__ETH.out')
c        rewind     36 
        open (unit=71, status='replace', file='sondage_DZsn.out')
        rewind     71
        open (unit=72, status='replace', file='sondage_TIsn.out')
        rewind     72
        open (unit=73, status='replace', file='sondage_ROsn.out')
        rewind     73
        open (unit=74, status='replace', file='sondage_WAsn.out')
        rewind     74
      END IF
 
C + 
      ! Diagnostics
      ! -----------
            snwae(mg)  =  rosSNo(1,1,1,mg) *dzsSNo(1,1,1,mg)
        DO nk = nsno-1,1,-1
            snwae(nk)  =  rosSNo(1,1,1,nk) *dzsSNo(1,1,1,nk)
     .                 +   snwae(nk+1)
        END DO
c      write (*,*) "mg: ", mg
c      write (*,*) "nsno: ", nsno
c      write (*,*) "nisSNo: ", nisSNo
c      write (*,*) "snwae: "
c      write (*,*) snwae
c      write (*,*) "rosSNo: "
c      write (*,*) rosSNo
C +
      IF  (                   itexpe.eq.0)                        THEN
              swdIB(1,1)  =  0.0
              al1IB(1,1,1)  =  0.0
            smb000        =   snwae(1)  - snwae(nisSNo(1,1,1)+1)
c            write (*,*) "snwae(1): ", snwae(1)
c            write (*,*) "swnae(nisSNo(1,1,1)+1): ",
c     .                    snwae(nisSNo(1,1,1)+1)
c            write (*,*) "smb000: ", smb000
      END IF
C +
            totime        =  totime     + dt
              swdIB(1,1)  =   swdIB(1,1)+ RAdsol(1,1)
              al1IB(1,1,1)  =   al1IB(1,1,1)+ RAdsol(1,1)*albeSL(1,1)
C +
      IF  ((jhurGE.eq.0 .and. minuGE.eq.0 .and. jsecGE.eq.0)
     .                  .or.  itexpe.ge.nterun-1)                 THEN
C +     
              al1IB(1,1,1)  =   al1IB(1,1,1)/ max(epsi,swdIB(1,1))
             smbsSN(1,1,1)=   snwae(1)  - smb000
c             write (*,*) "snwae(1): ", snwae(1)
c             write (*,*) "smb000: ", smb000
c             write (*,*) "smbsSN: ", smbsSN(1,1,1)
C +
c        IF (itexpe.ge.nterun-1)          dt_ETH_2 = dt_ETH_2 + 1    
C +
      ! Variables principales
      ! ---------------------
C +
            height_snow = 0.0D0     
          DO nk = 1, mg
            height_snow = height_snow + dzsSNo(ni,nj,nm,nk)
          END DO
            height_snow = height_snow * 100 - 2000
        IF (itexpe.ge.nterun-1)                                   THEN
             al1IB(1,1,1) =   al1IB(1,1,1) / max(epsi,swdIB(1,1))
        END IF
        IF (itexpe.eq.0)                                          THEN
            write (31,3330) 
 3330       format (
     .        ' +-------------------------------------------------',
     .        '------------------------------------------------+',/,
     .        ' |                                     Main Variabl',
     .        'es (Simulated)                                    |',/,
     .        ' +-------------+---------------+------------------+',
     .        '-----------------+------+------+--------+-------+',/,
     .        ' | dd mmm yyyy |    albedo     |  Mass Balance    |',
     .        '  Snow Height    | Ice  | Slush|  Dens  |  LWC  |',/,
     .        ' +-------------+---------------+------------------+',
     .        '-----------------+------+------+--------+-------+') 
            write (*,*)
            write (*,*) 'Simulation...' 
            write (*,*) '==================='
            write (*,*)
            write (*,660)
 660        format (
     .        ' +--------------------------------------------------',
     .        '----------------+',/,
     .        ' |                       Main variables simulated   ',
     .        '                |',/,
     .        ' +-------------+---------------+------------------+',
     .        '-----------------+',/,
     .        ' | dd mmm yyyy |    albedo     |   Mass balance   |',
     .        '   Snow height   |',/,
     .        ' +-------------+---------------+------------------+',
     .        '-----------------+') 
        END IF
        IF (itexpe.ge.10.AND.itexpe.lt.nterun-1)                  THEN
c          IF (ETH_1990)                                           THEN 
c            write (31,3331)  
c     .        jdarGE,labmGE(mmarGE),iyrrGE,
c     .        albedo______90(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_90(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_90(dt_ETH_2)  ,height_snow,
c     .        sihsSN(1,1,1),sshsSN(1,1,1),
c     .        ravsSN(1,1,1),wavsSN(1,1,1)
            write (31,3331)
     .        jdarGE,labmGE(mmarGE),iyrrGE,
     .        al1IB(1,1,1)*100,
     .        smbsSN(1,1,1)/10,
     .        height_snow,
     .        sihsSN(1,1,1),sshsSN(1,1,1),
     .        ravsSN(1,1,1),wavsSN(1,1,1)
c            write (*,662) 
c     .        jdarGE,labmGE(mmarGE),iyrrGE,
c     .        albedo______90(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_90(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_90(dt_ETH_2)  ,height_snow  
            write (*,662)
     .        jdarGE,labmGE(mmarGE),iyrrGE,
     .        al1IB(1,1,1)*100,
     .        smbsSN(1,1,1)/10,
     .        height_snow
c            write (35,3337) 
c     .        albedo______90(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_90(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_90(dt_ETH_2)  ,height_snow,
c     .        sihsSN(1,1,1),sshsSN(1,1,1),
c     .        ravsSN(1,1,1),wavsSN(1,1,1)
            write (35,3337)
     .        al1IB(1,1,1)*100,smbsSN(1,1,1)/10,height_snow,
     .        sihsSN(1,1,1),sshsSN(1,1,1),
     .        ravsSN(1,1,1),wavsSN(1,1,1)
c          ELSE
c            write (31,3331)  
c     .        jdarGE,labmGE(mmarGE),iyrrGE,
c     .        albedo______91(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_91(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_91(dt_ETH_2)  ,height_snow,        
c     .        sihsSN(1,1,1),sshsSN(1,1,1),
c     .        ravsSN(1,1,1),wavsSN(1,1,1)
c            write (*,662)    
c     .        jdarGE,labmGE(mmarGE),iyrrGE,
c     .        albedo______91(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_91(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_91(dt_ETH_2)  ,height_snow
c            write (35,3337) 
c     .        albedo______91(dt_ETH_2-1), al1IB(1,1,1) *100,
c     .        Bilan_masse_91(dt_ETH_2)  ,smbsSN(1,1,1)/10,
c     .        height_snow_91(dt_ETH_2)  ,height_snow,        
c     .        sihsSN(1,1,1),sshsSN(1,1,1),
c     .        ravsSN(1,1,1),wavsSN(1,1,1)
c          END IF
        END IF
C + 
      ! Diagnostics: RESET
      ! ------------------
              totime      =  0.0
               swdIB(1,1) =  0.0
               al1IB(1,1,1) =  0.0
C +
        IF (itexpe.ge.nterun)                                   THEN
          write (31,3329)
 3329     format (
     .    ' +-------------+---------------+------------------+',
     .    '-----------------+------+------+--------+-------+')
          write (*,661)
  661     format (
     .    ' +-------------+---------------+------------------+',
     .    '-----------------+')

          write (*,*)
          write (*,*) 'End simulation ...'
          write (*,*) '======================='
        END IF
      END IF
c 3331     format(' | ',i2,1x,a3,1x,i4,' | ',f4.1,2x,f7.4,' | '
c     .                ,f6.2,2x,f8.4,  ' | ',f5.1,2x,f8.4,' | ' 
c     .              ,f4.2,' | ',f4.2, ' | ',        f6.2,' | ',
c     .               f5.2,' |') 
 3331     format(' | ',i2,1x,a3,1x,i4,' | ',6x,f7.4,     ' | '
     .                ,2x,f14.4,       ' | ',1x,f14.4,     ' | '
     .                ,f4.2,' | ',f4.2, ' | ',f6.2,      ' | ',
     .                 f5.2,' |')
c  662     format(' | ',i2,1x,a3,1x,i4,' | ',f4.1,2x,f7.4,' | '
c     .                ,f6.2,2x,f8.4,  ' | ',f5.1,2x,f8.4,' | ') 
  662     format(' | ',i2,1x,a3,1x,i4,' | ',6x,f7.4,' | '
     .                ,2x,f14.4,       ' | ',1x,f14.4,' | ')
c 3337     format(f4.1,1x,f7.4,2x,f6.2,1x,f8.4,2x,f5.1,1x
c     .          ,f8.4,2x,f4.2,2x,f4.2,2x,f6.2,2x,f5.2)      
 3337     format(f7.4,2x,f8.4,2x,f8.4,2x,f4.2,2x,f4.2,2x
     .          ,f6.2,2x,f5.2)
C +
      ! Temp?rature de surface
      ! ----------------------
C +
      IF (minuGE.eq. 0 .or.  minuGE.eq.30)                        THEN 
          write (32,3332) jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE,
     .                    tsrf  ,tsrfSL(1,1,1)
 3332     format(i2,1x,a3,1x,i4,1x,i2,':',i2,3x,f7.3,3x,f7.3)
      END IF
C +
      ! Sondage
      ! -------
C +   
      IF (jhurGE.eq.18 .and. minuGE.eq. 0)                        THEN
          upper_ice  = zSNOW(nisSNo(1,1,1)+1)
        DO teller1   = 14,1,-1
        DO teller2   =       nisSNo(1,1,1),1,-1
          upper      = zSNOW(teller2+1) - upper_ice
          lower      = zSNOW(teller2)   - upper_ice
          ab_upper   = zSNOW(teller2+2) - upper_ice
          middlelayer= (upper    + lower) / 2.0
          ab_middle  = (ab_upper + upper) / 2.0
          IF (temp_z_mes(teller1) .GT. ab_middle     .and.
     .        temp_z_mes(teller1) .LE.    middlelayer      )      THEN
              dtemp = tisSNo(ni,nj,nm,teller2) 
     .              - tisSNo(ni,nj,nm,teller2+1)
              temp_mod(teller1) 
     .              = tisSNo(ni,nj,nm,teller2+1)
     .              + dtemp * (temp_z_mes(teller1)-ab_middle)
     .                      / (middlelayer        -ab_middle) 
          END IF
        END DO
        END DO
          write(33,3333)jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE, 
     .                 (temp_mod(i)-273.15,i = 14,1,-1)
 3333     format(i2,1x,a3,1x,i4,1x,i2,':',i2,3x,14(f7.2))
      END IF
C +
      ! Moyenne
      ! -------
      IF   (jhurGE.eq.0 .and. minuGE.eq.6
     .                  .or.  itexpe.ge.nterun-1)                 THEN
        IF (itexpe.le.10)                                         THEN
          write (34,3334)
 3334     format (
     .     '+---------------------------------------------------',
     .     '------------------------------------------------+' ,/,
     .     '|                                        Mean values',
     .     ' (modeled)                                      |' ,/,
     .     '+-------------+--------+--------+--------+--------+-',
     .     '--------+---------+---------+---------+---------+' ,/,
     .     '| dd mmm yyyy |  Tmin  |  Tmax  |  Tavg  |  Vavg  | ',
     .     ' S down | IR down |  IR up  |  sens   |   lat   |',/,
     .     '+-------------+--------+--------+--------+--------+-',
     .     '--------+---------+---------+---------+---------+')
        END IF
        IF (itexpe.gt.10.and.itexpe.lt.nterun-2)                  THEN
          write (34,3335) jdarGE,labmGE(mmarGE),iyrrGE 
     .             ,  mintIB(1,1,ml)-tfsnow,maxtIB(1,1,ml)-tfsnow
     .             , avtemIB(1,1)-tfsnow,avwinIB(1,1)
     .             ,   swdIB(1,1)        , lwdIB(1,1) , lwuIB(1,1)
     .             ,   shfIB(1,1)        , lhfIB(1,1)  
 3335     format ('| ',i2,1x,a3,1x,i4,1x,4('|',f7.3,1x),
     .                                   5('|',f8.3,1x),'|') 
          Temper_ETH(1) =  mintIB(1,1,ml)-tfsnow  + Temper_ETH(1)
          Temper_ETH(2) =  maxtIB(1,1,ml)-tfsnow  + Temper_ETH(2)
          Temper_ETH(3) = avtemIB(1,1)-tfsnow  + Temper_ETH(3)
          Temper_ETH(4) = avwinIB(1,1)         + Temper_ETH(4)
          Temper_ETH(5) =   swdIB(1,1)         + Temper_ETH(5)
          Temper_ETH(6) =   lwdIB(1,1)         + Temper_ETH(6)
          Temper_ETH(7) =   lwuIB(1,1)         + Temper_ETH(7)
          Temper_ETH(8) =   shfIB(1,1)         + Temper_ETH(8)
          Temper_ETH(9) =   lhfIB(1,1)         + Temper_ETH(9)
          Temper_ETH2   =  1                   + Temper_ETH2
        END IF
        IF (itexpe.eq.nterun-1)                                   THEN
          write (34,3335) jdarGE,labmGE(mmarGE),iyrrGE 
     .                  ,  mintIB(1,1,ml)             -tfsnow 
     .                  ,  maxtIB(1,1,ml)             -tfsnow
     .                  , avtemIB(1,1)/(itexpe-itrdIB)-tfsnow
     .                  , avwinIB(1,1)/(itexpe-itrdIB)
     .                  ,   swdIB(1,1)/(itexpe-itrdIB)
     .                  ,   lwdIB(1,1)/(itexpe-itrdIB)
     .                  ,   lwuIB(1,1)/(itexpe-itrdIB)
     .                  ,   shfIB(1,1)/(itexpe-itrdIB)
     .                  ,   lhfIB(1,1)/(itexpe-itrdIB) 
          Temper_ETH(1) =  mintIB(1,1,ml)/            -tfsnow 
     .                  + Temper_ETH(1)
          Temper_ETH(2) =  maxtIB(1,1,ml)/            -tfsnow 
     .                  + Temper_ETH(2)
          Temper_ETH(3) = avtemIB(1,1)/(itexpe-itrdIB)-tfsnow 
     .                  + Temper_ETH(3)
          Temper_ETH(4) = avwinIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(4)
          Temper_ETH(5) =   swdIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(5)
          Temper_ETH(5) =   lwdIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(6)
          Temper_ETH(7) =   lwuIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(7)
          Temper_ETH(8) =   shfIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(8)
          Temper_ETH(9) =   lhfIB(1,1)/(itexpe-itrdIB)        
     .                  + Temper_ETH(9)
          Temper_ETH2   = 1
     .                  + Temper_ETH2
          write (34,3336)(Temper_ETH(i)/Temper_ETH2,i=1,9)
 3336     format(
     .    '+-------------+--------+--------+--------+--------+-',
     .    '--------+---------+---------+---------+---------+',/,
     .    '|   Moyenne   ',4('|',f7.3,1x),5('|',f8.3,1x),'|',/,
     .    '+-------------+--------+--------+--------+--------+-',
     .    '--------+---------+---------+---------+---------+')
        END IF
      END IF
C +
      ! Profiles: Depth, Temperature, Density, Liquid Water
      ! ---------------------------------------------------
C +
C      write (*,*), "nsno ",nsno
      IF (minuGE.eq. 0 .or.  minuGE.eq.30)                        THEN
          write (71,3338) jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE,
     .                (dzsSNo(ni,nj,nm,nk),nk=nsno,1,-1)                
          write (72,3338) jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE,
     .                (tisSNo(ni,nj,nm,nk),nk=nsno,1,-1)
          write (73,3338) jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE,
     .                (rosSNo(ni,nj,nm,nk),nk=nsno,1,-1)
          write (74,3338) jdarGE,labmGE(mmarGE),iyrrGE,jhurGE,minuGE,
     .                (wasSNo(ni,nj,nm,nk),nk=nsno,1,-1)
 3338     format(i2,1x,a3,1x,i4,1x,i2,':',i2,3x,*(f10.4))
      END IF
c+
  
C +
  404 continue
      RETURN
      END 
