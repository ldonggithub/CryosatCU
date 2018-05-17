C  -----------------------------------------------------------------------
C            libUN : User level NetCDF READ / WRITE routines
C
C                    Creation     : 1996 
C                    Last major revision : 04 2000 
C                         minor revision : 01 2002
C                    See the file dev_history for further info.
C
C                    Compatible with NetCDF version 3.x (or above).
C  -----------------------------------------------------------------------
C
C   Purpose : Simplify access to NetCDF (No need to manage 'identifiers') 
C   --------- + enable writting of clear output routines. 
C             + no need for 'include' statements for NetCDF outside this.
C
C   Main routines provided :
C   ------------------------
C     UNcreate  : Simple file creation routine, 
C                    non-staggered (x,y,lev,time) grid
C                    (this code is older and less complete than UNscreate)
C
C     UNscreate : General file creation routine,
C                    defining multiple dimensions + attributes
C
C     UNwrite   : General variables writting routine
C                    (also updates 'range' attribute and variable if present)
C                 Note: Use UNlwrite to write 2D planes in 3D variables
C                             ^
C     UNwratt   : Real attributes writting 
C                    (Not necessary for 'actual_range' attribute:
C                     updated by UNwrite)
C     UNwcatt   : Characters attributes creation & writing
C                    (Not necessary for 'units', written by UNscreate)
C                    WARNING: this routine uses a temporary disk space
C                             equal to the file length (duplicate the file)
C                             (its use is NOT recommended)
C
C     UNread    : Reading routine (grid coordinates + variable)
C
C     UNsread   : Simplified (less returned information) reading routine.
C                 Warning: this is a recent addition - future changes
C                 or corrections may be necessary.
C
C   Complementary routines :
C   ------------------------
C     UNwopen   : re-open file for writting
C     UNropen   : open file for reading
C     UNgtime   : Find time index for a given time value
C     UNgindx   : Generalization of UNgtime: find value in any 1D data.   
C     UNclose   : close the NetCDF file
C  -----------------------------------------------------------------------
C                                    Contact: philippe.marbaix@advalvas.be
C
C
C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNcreate :  +                                         +
C**  +-------------------------+                                         +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +   FILEnam                : The name of the file to be created.    +
C**  +   title                                                           +
C**  +   Ni    ,Nj   ,Nlev,Nit  : # discrete values of dims(x,y,lev,time)+
C**  +   Rx(Ni),Ry...,Rlev,Rtime: Values of the dimensions: x,y,lev,time.+
C**  +                                                                   +
C**  +   Dvs, Nvs                                                        +
C**  +   unit_vs                : physical units for each var            +
C**  +   Ndim_vs  = 4 -> 3D space + time                                 +
C**  +            = 3 -> 2D space + time (for surface fields s.as. ps)   +
C**  +            = 2 -> 2D space (constant in time)                     +
C**  +                    (e.g. for latitude and longitude of grid pts)  +
C**  +            = 1 -> 1D time (special case: e.g. for a 'date' vector)+
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +   FILEid                                                          +
C**  +                                                                   +
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNcreate (FILEnam, title,
     &           Ni  ,Nj  ,Nlev ,Nit  ,
     &           Rx  ,Ry  ,Rlev ,Rtime,
     &           Dvs, Nvs , name_vs, Ndim_vs, unit_vs,
     &           FILEid )

C +
      IMPLICIT NONE
 
      INCLUDE 'NetCDF.inc'

C +
      INTEGER icheck
      INTEGER Lfnam, Ltit
      PARAMETER (Lfnam= 80, Ltit= 90)

C +   INPUT:      
C +   - - -
      INTEGER Ni, Nj, Nlev, Nit, Nvs, Dvs
      INTEGER Ndim_vs(Dvs) 
      REAL*8 Rx(Ni), Ry(Nj), Rlev(Nlev), Rtime(Nit)
      CHARACTER*(Lfnam) FILEnam
      CHARACTER*(Ltit)  title       
      CHARACTER*(*)  unit_vs(Dvs)
      CHARACTER*(*) name_vs(Dvs)

C +   OUTPUT:    
C +   - - - -
      INTEGER FILEid 

C +   LOCAL:
C +   - - -
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER verDID, timeDID
      INTEGER   xDID,    yDID
      INTEGER verVID, timeVID
      INTEGER   xVID,    yVID
      INTEGER VIDvs  
      INTEGER dimID(4)
      INTEGER start(4), count(4)
      INTEGER ivs
      INTEGER VNlen
      INTEGER Ierro

      icheck= 0   ! "debugging" level.

C     1 Create a NetCDF file and enter define mode :
C     ------------------------------------------------
      IF (icheck.ge.2) WRITE(*,*) 'FILEnam :', FILEnam

C     ** getting FILEnam size :
      VNlen = VARSIZE(FILEnam)
      IF (icheck.ge.3) WRITE(*,*) 'VNlen  :',VNlen

      Ierro =  NF_CREATE(FILEnam(1:VNlen), NF_CLOBBER,  FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
C     ** identif.      =>overwrite =error

C*    2 Time coordinate definition.
C     -------------------------------

C     ** Dimension definition :
      IF (icheck.ge.3) WRITE(*,*) 'Nit=',Nit
      Ierro=NF_DEF_DIM(FILEid , 'time', Nit, timeDID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

C     ** Define variable for the time coordinate values :
      dimID(1) = timeDID
      Ierro=NF_DEF_VAR(FILEid , 'time', NF_FLOAT,1 , dimID, timeVID)
C                        FILEid  var name  type #3 dims  ID   VARid
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro  2', Ierro

C     3 Vertical level definition.
C     ----------------------------

C     **  Dimension definition :
      Ierro=NF_DEF_DIM(FILEid , 'level', Nlev, verDID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro 3a', Ierro

C     ** Define var . for vertical of levels :
      dimID(1) = verDID
      Ierro=NF_DEF_VAR(FILEid , 'level', NF_FLOAT,1 , dimID, verVID)
C     **                 var     att.name   type   len  value  VARid
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro 3b', Ierro


C     4. Define the grids : DIMs, VARs (locations)
C     ---------------------------------------------
C     ** define a regular x(i) , y(j) grid
C
      Ierro=NF_DEF_DIM(FILEid , 'x', Ni,xDID )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      Ierro=NF_DEF_DIM(FILEid , 'y', Nj,yDID ) 
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro 4:', Ierro

C     **  Ierro=NF_DEF_VAR(FILEid , 'xxxxx', NF_FLOAT,1   , DIMS, *VID)
C     **                     id/file    name    type   #dims  dims  VARid
      dimID(1) = xDID  
      Ierro=NF_DEF_VAR(FILEid ,'x',NF_FLOAT,1 , dimID, xVID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      dimID(1) = yDID  
      Ierro=NF_DEF_VAR(FILEid ,'y',NF_FLOAT,1 , dimID,  yVID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

C     5. Define the fields. 
C     ---------------------

      DO ivs = 1,Nvs         ! **BEGIN LOOP on var. num.

C       ** All field types have the 2 x,y space dimensions:
        dimID(1) = xDID
        dimID(2) = yDID

C       ** Define fields according to their dimension:

        IF (Ndim_vs(ivs).eq.1) THEN
C         **1D time coordinate (special case)
          dimID(1) = timeDID
          Ierro=NF_DEF_VAR(FILEid , name_vs(ivs),
     &                     NF_FLOAT, 1, dimID,VIDvs)
          IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

        ELSE IF (Ndim_vs(ivs).eq.2) THEN  
C         **2D space field:
          Ierro=NF_DEF_VAR(FILEid , name_vs(ivs),
     &                       NF_FLOAT, 2, dimID,VIDvs)
          IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

        ELSE IF (Ndim_vs(ivs).eq.3) THEN
C         **2D space + time field:
          dimID(3) = timeDID
          Ierro=NF_DEF_VAR(FILEid , name_vs(ivs),
     &                       NF_FLOAT, 3, dimID, VIDvs)
          IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

        ELSE IF (Ndim_vs(ivs).eq.4) THEN
C         **3D space + time field:
          dimID(3) = verDID
          dimID(4) = timeDID
          Ierro=NF_DEF_VAR(FILEid , name_vs(ivs),
     &                       NF_FLOAT, 4, dimID,VIDvs)
          IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

        ELSE
          WRITE (*,*) 'UNcreate - ERROR: var. dimension ?', Ndim_vs(ivs)
        END IF  

        IF (icheck.ge.2) WRITE(*,*) 'Ierro 5:', Ierro

C     6. Set the variable's attributes : 
C     ----------------------------------

C     ** Units: 
        Ierro=NF_PUT_ATT_TEXT(FILEid, VIDvs, 'units',
     &                          10, unit_vs(ivs))
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

        IF (icheck.ge.2) WRITE(*,*) 'Ierro 6:', Ierro

      END DO                 ! **END   LOOP on var. num.

C     7 Global attribute(s).
C     ----------------------

C     ** General file descriptor (title) :
      Ierro =  NF_PUT_ATT_TEXT(FILEid, NF_GLOBAL, 'title',
     &                         Ltit,title)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

C     8 Leave define mode (!file remains open )
C     -----------------------------------------
      Ierro=NF_ENDDEF(FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)

C     9. Writing of grid-points coordinates.
C     --------------------------------------

      start(1)= 1
      count(1)= Nit
      Ierro=NF_PUT_VARA_DOUBLE(FILEid, timeVID, start, count, Rtime)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro9.1', Ierro

      start(1)= 1
      count(1)= Nlev
      Ierro=NF_PUT_VARA_DOUBLE(FILEid, verVID, start, count, Rlev)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro9.2', Ierro

      count(1)= Ni  
      Ierro=NF_PUT_VARA_DOUBLE(FILEid, xVID, start, count, Rx)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro9.3', Ierro

      count(1)= Nj           
      Ierro=NF_PUT_VARA_DOUBLE(FILEid, yVID, start, count, Ry)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNcreate', Ierro)
      IF (icheck.ge.2) WRITE(*,*) 'Ierro9.4', Ierro
C +
      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNwrite :   +                                         +
C**  +-------------------------+                                         +
C**  +  * Writes a variable into a NetCDF file,                          +
C**  +    (the NetCDF file must have been created (or re-opened) and     +
C**  +     closed after all writing operations).                         +
C**  +  * Automatically updates attribute 'actual_range' if available    +
C**  +          "          "    special var. '[var]_range'    "          +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : input file identifier (from UNcreate OR NetCDF open) +
C**  +    VARname : name given to the variable to write (must be in file)+
C**  +    itime   : No of time step to write to                          +
C**  +    Ni,Nj,Nlev: dimensions of 'var'                                +
C**  +              ! Nlev= 1 for 2D and 1D input variables.             + 
C**  +                Nj  = 1 for 1D input variables.                    +
C**  +              NB: can not write 1 level of 3D var only (->UNlwrite)+
C**  +                                                                   +
C**  +    var     : The variable to be writen                            +
C**  +                                                                   +
C**  +  REMARK :                                                         +
C**  +    Truncation of input data is permited:                          +
C**  +    If the dim of "var" > dim in the NetCDF file,                  +
C**  +    "var" is automatically truncted. However, this => WARNING      +
C**  +    message, UNLESS a specific truncation was "announced"          +
C**  +    in var:                                                        +
C**  +       To truncate the first dim to Li, let var(Ni,1,1) = Li       +
C**  +       To truncate the 2nd   dim to Lj, let var(1,Nj,1) = Lj       +
C**  +       ... (this has no effect exept cancel the "WARNING" message) +
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNwrite (FILEid , VARname , itime,
     &                    Ni,  Nj, Nlev, var)

      IMPLICIT NONE

      INCLUDE 'NetCDF.inc'

      INTEGER icheck

      INTEGER Lvnam 
      PARAMETER (Lvnam=20)

C     ** input 
      INTEGER FILEid 
      INTEGER itime
      INTEGER Ni,  Nj, Nlev 
      CHARACTER *(*) VARname 
      REAL*8 var(Ni, Nj, Nlev)

C     ** local :
      INTEGER    MXlv
      PARAMETER (MXlv=500) 
C                ^^^^Maximal # levels for a special output
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER NVRi,  NVRj, NVRlev
      INTEGER Ierro, TTerr, Nvatts, vtype
      INTEGER dimID(4), dimSIZ(4), count(4)    
      INTEGER start(4),stride(4),imap(4)
      CHARACTER*(Lvnam) dimNAM(4) 
      CHARACTER*(Lvnam) recname
      CHARACTER*(30) tmpchr
      INTEGER varVID
      INTEGER VNlen, NDIMvar, NSDIvar, tiDI, itmp
      INTEGER iz, ii, jj, ll
      INTEGER iUNLIMDIM
      REAL*8 chkdim
      REAL*8 Arange(2)
      REAL*8 Srange(MXlv,2)
      REAL*8 LMissVal
      LOGICAL OkRange

      icheck= 0     !** 'debugging' level
      TTerr = 0     !** 'total number of errors
 
      IF (icheck.ge.1) WRITE(*,*) 'UNwrite : Begin'

C*    1. Get the variable field  and dims IDs
C     ----------------------------------------

      IF (icheck.ge.2) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting VARname  size :
      VNlen = VARSIZE (VARname)
      IF (icheck.ge.3) WRITE(*,*) 'VNlen  :', VNlen
      IF (icheck.ge.2) WRITE(*,*) 'VARname   :', VARname (1:VNlen)

C     ** variable field ID :
      Ierro=NF_INQ_VARID (FILEid, VARname (1:VNlen), varVID)

C     ** Cancel writing if an error occured : variable undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNwrite  Info  : Variable ',VARname(1:VNlen)
     &            ,' not found -> not written.' 
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNwrite_end


C     ** Inquire about the number of dimensions in var :
C     **
      Ierro=NF_INQ_VAR(FILEid , varVID, recname, vtype,
     &                   NDIMvar,  dimID,  Nvatts)
C     **  line1          id/file  id/var  var name var type
C     **  line2          # dims   id/dims #attributes
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)

      IF (icheck.ge.2) WRITE(*,*) 'Ierro1. ', Ierro


C*    2. Dimensions : inquire about file + compare with input data.
C     -------------------------------------------------------------

C     2.1 Inquire dimensions names and sizes :
C +   - - - - - - - - - - - - - - - - - - - - -
      DO iz = 1,4
        dimSIZ(iz)=0
        dimNAM(iz)='       '
C       ** Set any unused dimension to "0" size / no name
      END DO 
      DO iz = 1,NDIMvar
        Ierro=NF_INQ_DIM(FILEid , dimID(iz), dimNAM(iz), dimSIZ(iz))
C       **                 id/file  id/dim     dimname      dimsize    
C       **                                     !output      output
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)
      END DO
      IF (icheck.ge.3) WRITE(*,*) 'NDIMvar  ',NDIMvar
      IF (icheck.ge.3) WRITE(*,*) 'Ierro 2.0',Ierro  

C     2.2 Set writing region according to field dimension : 2D or 3D
C +   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     ** Set horizontal dimensions (default, for most data) :
      count(1) = Ni
      count(2) = Nj
C +   ** Other default values:
      count(3) = 0
      count(4) = 0
      start(1) = 1
      start(2) = 1
      start(3) = 1
      start(4) = 1

C +- ------3D+time variable in file-----------
      IF (NDIMvar.eq.4) THEN
C       ** 3D space + time: 
        NSDIvar = 3     ! # space dims
        tiDI    = 4     ! No. of the time dim
C       ** write 3D space:
        start(3) = 1    ! Start of index 3 in var (here = vert. levs)
        count(3) = Nlev ! # values of index 3 in var 
C       ** write one time step:
        start(4) = itime
        count(4) = 1
C +- ------3D *OR* 2D+time var in file--------
      ELSE IF (NDIMvar.eq.3) THEN
        IF (Nlev.EQ.1) THEN
C         ** 2D space + time (standard use of UNlib):
          NSDIvar = 2
          tiDI    = 3
C         ** ...write one time step:
          start(3) = itime
          count(3) = 1     
        ELSE
C         ** 3D space (no time):
          NSDIvar = 3
          tiDI    = 0
C         ** ...write 3rd dimension:
          start(3) = 1    
          count(3) = Nlev
        ENDIF
C +- ------2D *OR* 1D+time var in file--------
      ELSE IF (NDIMvar.eq.2) THEN
        IF (Nj.EQ.1 .AND. dimNAM(2)(1:4).EQ.'time') THEN
C         ** Write a 1D vector at time= itime:
          NSDIvar = 1
          tiDI    = 2
          start(2) = itime
          count(2) = 1
        ELSE
C         ** Usual MAR 2D space (no time):
          NSDIvar = 2
          tiDI    = 0
        END IF
C +- ------1D *OR* 0D+time var in file--------
      ELSE IF (NDIMvar.eq.1) THEN
C       ** 1D space or time
        IF (Ni.eq.1) THEN
C         ** Write a single element (at itime)
          start(1) = itime
          count(1) = 1
          count(2) = 0
          NSDIvar = 0
          tiDI    = 1
        ELSE
C         ** Write a vector (use only "space" dim 1)
          NSDIvar = 1
          tiDI    = 0
          count(2)= 0
        END IF
      ELSE
         WRITE(*,*) 'UNwrite ERROR : data field dimension ?'
         STOP
      END IF

C     2.3 Compare file dimensions to input data.
C +   - - - - - - - - - - - - - - - - - - - - - -
C     ** Save variable size for use as "valid" size (-> range):
      NVRi   = Ni
      NVRj   = Nj
      NVRlev = Nlev
C     ** Space dimensions :
      IF (NSDIvar.GT.0) THEN
      DO iz = 1,NSDIvar
        IF      (dimSIZ(iz).gt.count(iz)) THEN
          write(*,*) 'UNwrite - WARNING: '
          write(*,*) ' Your field ',VARname,' has an empty part.'
          write(*,*) ' (for the dimension:',dimNAM(iz),')'
        ELSE IF (dimSIZ(iz).lt.count(iz)) THEN
C         ** Do display "warning" only if truncation
C            was not "correctly announced" (see header)
C            (NVR... => stop here when updating the range attribute)
          IF (iz.EQ.1) THEN 
            chkdim = var(Ni,1,1) 
            NVRi   = dimSIZ(1) 
          ELSE IF (iz.EQ.2) THEN 
            chkdim = var(1,Nj,1)
            NVRj   = dimSIZ(2)
          ELSE IF (iz.EQ.3) THEN 
            chkdim = var(1,1,Nlev)
            NVRlev = dimSIZ(3)
          ELSE  
            chkdim = 0.0
          ENDIF
          IF (ABS(chkdim-dimSIZ(iz)).GT. 0.1 ) THEN
            write(*,*) 'UNwrite - WARNING: '
            write(*,*) ' Your field ',VARname,' will be truncated.'
            write(*,*) ' (for the dimension:',dimNAM(iz),')'
          ENDIF
          count(iz) = dimSIZ(iz)
        END IF
      END DO
      END IF

C     ** Time dimension (when defined):
      IF (tiDI.ne.0) THEN
       IF (itime.gt.dimSIZ(tiDI)) THEN
         IF (icheck.ge.1) WRITE(*,*) 'Time limit, ID', dimID(tiDI) 
         Ierro= NF_INQ_UNLIMDIM (FILEid, iUNLIMDIM) 
         IF (dimID(tiDI).NE.iUNLIMDIM) THEN
            WRITE(*,*) 'UNwrite - ERROR:   '
            WRITE(*,*) ' Time index out of range '                        
            STOP
         ENDIF
        END IF
      END IF

      IF (icheck.ge.2) WRITE(*,*) 'Ierro2. ', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'Dimension names :',dimNAM
      IF (icheck.ge.2) WRITE(*,*) 'dimSIZ :',dimSIZ
      IF (icheck.ge.2) WRITE(*,*) 'count  :',count
      IF (icheck.ge.2) WRITE(*,*) 'start  :',start
      IF (icheck.ge.2) WRITE(*,*) 'dimID  :',dimID 

C*    3. Write variable.
C     ------------------

C     ** Set 'imap' and WRITE with NCVPTG:
C     ** NOTE : since the arrays (grid_*) may be over-dimensionned,
C     **        we use the 'generalised' writing routine NCVPTG
C     ** (imap tells NetCDF about the memory locations of var)
      imap(1) = 1
      imap(2) = imap(1) * Ni      ! 1st dim of var = Ni 
      imap(3) = imap(2) * Nj      ! 2nd dim of var = Nj 
      imap(4) = 0                 ! (not used: 0 or 1 time step)   
      DO iz=1,4
        stride(iz)=1
      END DO
C     ** NOTE: stride is not used.

      Ierro=NF_PUT_VARM_DOUBLE(FILEid , varVID  , start      , count,
     &                         stride , imap    , var(1,1,1) )
C     **  line1:              id/file | id/var  |read from...|#data
C     **  line2:              step    |re-arrang|variable(beg.)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)

      IF (icheck.ge.2) WRITE(*,*) 'Ierro3.2', Ierro

C*    4. Update 'actual_range' attribute.               
C     ----------------------------------

C     If 'actual_range' available, get its current value:
C     - - - - - - - - - - - - - - - - - - - - - - - - - -

C     ** Get the old min and max values:
      Ierro=NF_GET_ATT_DOUBLE(FILEid ,varVID ,'actual_range' ,
     &                        Arange )
c     **line1 ^^^^^^^^^^^^^^  FILEid |var.id | attr.name
C     **line2                 value

C     ** Cancel if an error occured : attribute undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNwrite  Info : attribute actual_range ' 
     &             ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9990 !** Next section

C     Update the min an max
C     - - - - - - - - - - - 
      LMissVal = 1.E21          !Internally set at present.

C     **If this is the first pass, initialise min and max:
C     **(Constant fields shall not be accounted for)
      IF (Arange(1).ne.Arange(2)) THEN 
        OkRange = .true. 
      ELSE
        OkRange = .false.
      ENDIF

      DO ll=1, NVRlev
      DO jj=1, NVRj
      DO ii=1, NVRi 
        IF (ABS(var(ii,jj,ll)).LE.LMissVal) THEN
           IF (OkRange) THEN
              Arange(1) = MIN(Arange(1), var(ii,jj,ll))
              Arange(2) = MAX(Arange(2), var(ii,jj,ll))
           ELSE        
              Arange(1) = var(ii,jj,ll)
              Arange(2) = var(ii,jj,ll)
              OkRange = .true.
           ENDIF
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      IF (icheck.ge.2) WRITE(*,*) 'Arange',Arange

C     Set attribute.
C     - - - - - - - -

      Ierro=NF_PUT_ATT_DOUBLE(FILEid  ,varVID ,'actual_range' ,
     &                        NF_FLOAT,2      ,Arange)
c     **line1 ^^^^^^^^^^^^^^^ FILEid  |var.id | attr.name
C     **line2                 type    |len    | attr.value
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)
      TTerr = TTerr + ABS(Ierro)

C     ** Next section:
 9990 CONTINUE

C*    5. Update the optional '[var]_range' special variable.
C     ------------------------------------------------------
      IF (NDIMvar.eq.4.and.Nlev.lt.MXlv) THEN

C     If '[var]_range' available, get its current value:
C     - - - - - - - - - - - - - - - - - - - - - - - - - -

C     ** Get ID of variable [var]_range :
      tmpchr = VARname(1:VNlen)//'_range'
      itmp   = VNlen + 6
      Ierro=NF_INQ_VARID(FILEid, tmpchr(1:itmp), varVID)

C     ** Cancel if an error occured : undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNwrite  Info : [var]_range '
     &            ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNwrite_end

C     ** Get the old min and max values:
C     ** NOTE :
C     **        we use the 'generalised' reading routine NCVGTG
C     ** (imap tells NetCDF about the memory locations of var)
      imap(1) = 1
      imap(2) = imap(1) * MXlv   
      start(1)= 1
      start(2)= 1
      count(1)= Nlev
      count(2)= 2

C     ** (See UNread for explanations about NCVGTG)
      Ierro=NF_GET_VARM_DOUBLE(FILEid, varVID, start, count,   
     &                         stride,  imap , Srange(1,1) )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)

C     Update the min an max
C     - - - - - - - - - - -
C     **If this is the first pass, initialise min and max:
C     **(Constant fields shall not be accounted for)
      DO ll=1, Nlev
        IF (Srange(ll,1).eq.Srange(ll,2)) THEN
          Srange(ll,1) = var(1,1,ll)
          Srange(ll,2) = var(1,1,ll) 
        ENDIF
      ENDDO

      DO jj=1, NVRj
      DO ii=1, NVRi
       DO ll=1, NVRlev
        Srange(ll,1) = MIN(Srange(ll,1), var(ii,jj,ll))
        Srange(ll,2) = MAX(Srange(ll,2), var(ii,jj,ll))
       ENDDO
      ENDDO
      ENDDO
      IF (icheck.ge.4) WRITE(*,*) 'Srange',Srange


C     Set special variable [var]_range
C     - - - - - - - - - - - - - - - - -
C     **(See UNread for explanations abtout NCVPTG)

      Ierro=NF_PUT_VARM_DOUBLE(FILEid , varVID , start, count,
     &                         stride , imap   , Srange(1,1) )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwrite', Ierro)

      ENDIF  ! End Section 5.

C     UNwrite_end
C     -----------
      IF (icheck.ge.2) WRITE(*,*) 'Errors count:',TTerr
      IF (icheck.ge.2) WRITE(*,*) 'UNwrite : End'
 9999 CONTINUE
      RETURN
      END
C**
C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNlwrite :  +                                         +
C**  +-------------------------+                                         +
C**  +  * Writes a 2D horizontal LEVEL into a 3D+time NetCDF variable    +  
C**  +       OR  a 1D vector           into a 2D+time                    +
C**  +             --            ----         --                         +
C**  +    (SEE ALSO : UNwrite, for all dimensions - this a pecular case  +
C**  +     Note: 1D vectors are writen in the 1st dim of 2D+time)        +
C**  +                                                                   +
C**  +  * Automatically updates attribute 'actual_range' if available    +
C**  +          "          "    special var. '[var]_range'    "          +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : input file identifier (from UNcreate OR NetCDF open) +
C**  +    VARname : name given to the variable to write (must be in file)+
C**  +    itime   : No of time step to write to                          +
C**  +    level   : No of level     to write to                          +
C**  +    Ni,  Nj : dimensions of 'var'...                               +
C**  +    var     : A 2D variable to be writen                           +
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNlwrite (FILEid , VARname , itime,
     &                     ilev, Ni,  Nj, var)

      IMPLICIT NONE

      INCLUDE 'NetCDF.inc'

      INTEGER icheck

      INTEGER Lvnam 
      PARAMETER (Lvnam=20)

C     ** input 
      INTEGER FILEid 
      INTEGER itime, ilev
      INTEGER Ni,  Nj
      CHARACTER *(*) VARname 
      REAL*8 var(Ni, Nj)

C     ** local :
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER Ierro, TTerr, Nvatts, vtype
      INTEGER dimID(4), dimSIZ(4), count(4)    
      INTEGER start(4),stride(4),imap(4)
      INTEGER iUNLIMDIM
      CHARACTER*(Lvnam) dimNAM(4) 
      CHARACTER*(Lvnam) recname
      CHARACTER*(30) tmpchr
      INTEGER varVID
      INTEGER VNlen, NDIMvar, NSDIvar, tiDI, ilDI, itmp
      INTEGER iz, ii, jj
      REAL*8 Arange(2)
      REAL*8 Srange(2)
      
      icheck= 0     !** 'debugging' level
      TTerr = 0     !** 'total numbe of errors
 
      IF (icheck.ge.1) WRITE(*,*) 'UNlwrite : Begin'

C*    1. Get the variable field  and dims IDs
C     ----------------------------------------

      IF (icheck.ge.2) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting VARname  size :
      VNlen = VARSIZE (VARname)
      IF (icheck.ge.3) WRITE(*,*) 'VNlen  :',VNlen
      IF (icheck.ge.2) WRITE(*,*) 'VARname   :', VARname (1:VNlen)

C     ** variable field ID :
      Ierro=NF_INQ_VARID (FILEid, VARname (1:VNlen), varVID)

C     ** Cancel writing if an error occured : variable undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNlwrite  Info  : Variable ',VARname(1:VNlen)
     &            ,' not found -> not written.' 
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNlwrite_end


C     ** Inquire about the number of dimensions in var :
C     **
      Ierro=NF_INQ_VAR(FILEid , varVID, recname, vtype,
     &                   NDIMvar,  dimID,  Nvatts)
C     **  line1          id/file  id/var  var name var type
C     **  line2          # dims   id/dims #attributes
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)

      IF (icheck.ge.2) WRITE(*,*) 'Ierro1. ', Ierro


C*    2. Dimensions : inquire about file + compare with input data.
C     -------------------------------------------------------------

C     2.1 Inquire dimensions names and sizes :
C +   - - - - - - - - - - - - - - - - - - - - -
      DO iz = 1,4
        dimSIZ(iz)=0
        dimNAM(iz)='       '
C       ** Set any unused dimension to "0" size / no name
      END DO

      DO iz = 1,NDIMvar
        Ierro=NF_INQ_DIM(FILEid , dimID(iz), dimNAM(iz), dimSIZ(iz))
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)
C       **           id/file   id/dim    dimname     dimsize    error
C       **                               !output     output
      END DO
      IF (icheck.ge.3) WRITE(*,*) 'NDIMvar  ',NDIMvar
      IF (icheck.ge.3) WRITE(*,*) 'Ierro 2.0',Ierro  

C     2.2 Set writing region according to field dimension :  3D
C +   - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     ** Set horizontal dimensions (all field dims):
      count(1) = Ni
      count(2) = Nj
      start(1) = 1
      start(2) = 1
C +- ------ 3D+time var in file--------
      IF (NDIMvar.eq.4) THEN
        NSDIvar = 2     ! # input space dims (for a 2D level)
        tiDI    = 4     ! No. of the time dim
C       ** write one level (set the level No) :
        start(3) = ilev ! Start of index 3 in var 
        count(3) = 1    ! # values of index 3 in var 
        ilDI     = 3
C       ** write one time step:
        start(4) = itime
        count(4) = 1
C +- ------ 2D+time var in file--------
      ELSE IF (NDIMvar.eq.3) THEN
        NSDIvar = 1     ! # input space dims (for a 1D vector)
        tiDI    = 3     ! No. of the time dim
C       ** write one "level" - here a 1D vector in the 1st dim.
        start(2) = ilev ! Start of index 2 in var
        count(2) = 1    ! # values of index 3 in var
        ilDI     = 2
C       ** write one time step:
        start(3) = itime
        count(3) = 1
      ELSE
         WRITE(*,*) 'UNlwrite ERROR : data field dimension ?'
         WRITE(*,*) '  NB: UNlwrite = only for (2 or) 3D +time.'
         STOP
      END IF

C     2.3 Compare file dimensions to input data.
C +   - - - - - - - - - - - - - - - - - - - - - -
C     ** Space dimensions :
      DO iz = 1,NSDIvar
        IF      (dimSIZ(iz).gt.count(iz)) THEN
          write(*,*) 'UNlwrite - WARNING: '
          write(*,*) ' Your field ',VARname,' has an empty part.'
          write(*,*) ' (for the dimension:',dimNAM(iz),')'
        ELSE IF (dimSIZ(iz).lt.count(iz)) THEN
          write(*,*) 'UNlwrite - WARNING: '
          write(*,*) ' Your field ',VARname,' will be truncated.'
          write(*,*) ' (for the dimension:',dimNAM(iz),')'
          count(iz) = dimSIZ(iz)
        END IF
      END DO

C     ** Space dimensions - check if requested level exists:
      IF (dimSIZ(ilDI).lt.ilev) THEN
        write(*,*) 'UNlwrite - ERROR: '
        write(*,*) ' The requested level =',ilev
        write(*,*) ' does not exist in the field ',VARname
        write(*,*) ' (for the dimension:',dimNAM(ilDI),')'
        STOP
      END IF

C     ** Time dimension (when defined):
      IF (tiDI.ne.0) THEN
       IF (itime.gt.dimSIZ(tiDI)) THEN
         IF (icheck.ge.1) WRITE(*,*) 'Time limit, ID', dimID(tiDI) 
         Ierro= NF_INQ_UNLIMDIM (FILEid, iUNLIMDIM) 
         IF (dimID(tiDI).NE.iUNLIMDIM) THEN
            WRITE(*,*) 'UNlwrite - ERROR:  '
            WRITE(*,*) ' Time index out of range '                        
            STOP
         ENDIF
        END IF
      END IF

      IF (icheck.ge.2) WRITE(*,*) 'Ierro2. ', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'Dimension names :',dimNAM
      IF (icheck.ge.3) WRITE(*,*) 'dimSIZ :',dimSIZ
      IF (icheck.ge.3) WRITE(*,*) 'count  :',count
      IF (icheck.ge.3) WRITE(*,*) 'start  :',start
      IF (icheck.ge.3) WRITE(*,*) 'dimID  :',dimID 

C*    3. Write variable.
C     ------------------

C     ** Set 'imap' and WRITE with NCVPTG:
C     ** NOTE : since the arrays (grid_*) may be over-dimensionned,
C     **        we use the 'generalised' writing routine NCVPTG
C     ** (imap tells NetCDF about the memory locations of var)
      imap(1) = 1
      imap(2) = imap(1) * Ni      ! 1st dim of var = Ni 
      imap(3) = imap(2) * Nj      ! (not used: 1 level...)
      imap(4) = 0                 ! (not used: 0 or 1 time step)   
      DO iz=1,4
        stride(iz)=1
      END DO
C     ** NOTE: stride is not used.

      Ierro=NF_PUT_VARM_DOUBLE (FILEid  , varVID  , start      , count,
     &                          stride  , imap    , var(1,1)          )
C     **  line1:                id/file | id/var  |read from...|#data
C     **  line2:                step    |re-arrang|variable(beg.)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)

      IF (icheck.ge.2) WRITE(*,*) 'Ierro3.2', Ierro

C*    4. Update 'actual_range' attribute.               
C     ----------------------------------

C     If 'actual_range' available, get its current value:
C     - - - - - - - - - - - - - - - - - - - - - - - - - -

C     ** Get the old min and max values:
      Ierro=NF_GET_ATT_DOUBLE(FILEid ,varVID ,'actual_range' ,
     &                        Arange )
c     **line1 ^^^^^^^^^^^^^^^ FILEid |var.id | attr.name
C     **line2                 value

C     ** Cancel if an error occured : attribute undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNlwrite  Info : attribute actual_range ' 
     &             ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9990 !** Next section

C     Update the min an max
C     - - - - - - - - - - - 

C     **If this is the first pass, initialise min and max:
C     **(Constant fields shall not be accounted for)
      IF (Arange(1).eq.Arange(2)) THEN
        Arange(1) = var(1,1)
        Arange(2) = var(1,1)
      ENDIF

      DO jj=1, Nj
      DO ii=1, Ni
        Arange(1) = MIN(Arange(1), var(ii,jj))
        Arange(2) = MAX(Arange(2), var(ii,jj))
      ENDDO
      ENDDO
      IF (icheck.ge.2) WRITE(*,*) 'Arange',Arange

C     Set attribute.
C     - - - - - - - -

      Ierro=NF_PUT_ATT_DOUBLE(FILEid  ,varVID ,'actual_range' ,
     &                        NF_FLOAT,2      ,Arange )
c     **line1 ^^^^^^^^^^^^^^^ FILEid  |var.id | attr.name
C     **line2                 type    |len    | attr.value
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)
      TTerr = TTerr + ABS(Ierro)

C     ** Next section:
 9990 CONTINUE

C*    5. Update the optional '[var]_range' special variable.
C     ------------------------------------------------------
      IF (NDIMvar.eq.4) THEN

C     If '[var]_range' available, get its current value:
C     - - - - - - - - - - - - - - - - - - - - - - - - - -

C     ** Get ID of variable [var]_range :
      tmpchr = VARname(1:VNlen)//'_range'
      itmp   = VNlen + 6
      Ierro=NF_INQ_VARID (FILEid, tmpchr(1:itmp), varVID)

C     ** Cancel if an error occured : undefined ?
      IF (Ierro.ne.0.and.icheck.ge.1) THEN
         WRITE(*,*) 'UNlwrite  Info : [var]_range '
     &             ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNlwrite_end

C     ** Get the old min and max values:
C     ** NOTE :
C     **        we use the 'generalised' reading routine NCVGTG
C     ** (imap tells NetCDF about the memory locations of var)
      imap(1) = 1
      imap(2) = 0                ! Not used (write only 1 lev)
      start(1)= ilev
      count(1)= 1   
      start(2)= 1
      count(2)= 2

C     ** (See UNread for explanations abtout NCVGTG)
      Ierro=NF_GET_VARM_DOUBLE(FILEid, varVID, start      ,count,   
     &                         stride,  imap , Srange(1) )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)

C     Update the min an max
C     - - - - - - - - - - -
C     **If this is the first pass, initialise min and max:
C     **(Constant fields shall not be accounted for)
      IF (Srange(1).eq.Srange(2)) THEN
          Srange(1) = var(1,1)
          Srange(2) = var(1,1) 
      ENDIF

      DO jj=1, Nj
      DO ii=1, Ni
        Srange(1) = MIN(Srange(1), var(ii,jj))
        Srange(2) = MAX(Srange(2), var(ii,jj))
      ENDDO
      ENDDO
      IF (icheck.ge.4) WRITE(*,*) 'Srange',Srange


C     Set special variable [var]_range
C     - - - - - - - - - - - - - - - - -
C     **(See UNread for explanations abtout NCVPTG)

      Ierro=NF_PUT_VARM_DOUBLE(FILEid , varVID , start        , count,
     &                         stride , imap   , Srange(1)  )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNlwrite', Ierro)

      ENDIF  ! End Section 5.

C     UNlwrite_end
C     -----------
      IF (icheck.ge.2) WRITE(*,*) 'Errors count:',TTerr
      IF (icheck.ge.2) WRITE(*,*) 'UNlwrite : End'
 9999 CONTINUE
      RETURN
      END
C**
C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNread :    +                                         +
C**  +-------------------------+                                         +
C**  +  * Reads a model variable from a NetCDF file,                     +
C**  +    and reads the coordinates of the grid upon wich it is defined. +
C**  +    (the NetCDF file must have been opened and must be closed      +
C**  +     after all reading operations). May read an x-y subregion.     +
C**  +                                                                   +
C**  +  INPUT :                                                          + 
C**  +    FILEid  : input file identifier (from NetCDF open)             +
C**  +    VARname  : name of the requested variable.                     +
C**  +    time : [integer*4] is the time index of the data field to read +
C**  +    level: [integer*4] (usefull for 3D-space fields only) :        +
C**  +                       if not=0 --> = no of the level              +
C**  +                                      -> output is 2D (l_dim = 1)  +
C**  +                       if  =0   --> read ALL levels                +
C**  +                                      -> output is 3D              +
C**  +    i_dbeg, j_dbeg      : horizontal indexes of requested region   +
C**  +                          in input data file                       + 
C**  +    i_dim, j_dim, l_dim : ...the dimensions of 'var',              + 
C**  +                       = the dimensions of the sub-region to read  + 
C**  +                       ! l_dim = 1 if level not=0                  + 
C**  +                       ! j_dim = 1 if var is 1D                    + 
C**  +  OUTPUT :                                                         + 
C**  +    varax1[i_dim] (real  )                                         + 
C**  +    varax2[j_dim]: Horizontal coordinates in the file (lat/lon,...)+ 
C**  +    varlev[l_dim]: vertical coordinate of the levels               + 
C**  +                   (! when level not=0, only varlev(1) is defined) + 
C**  +    var_units                 : physical units of var.             + 
C**  +    var[i_dim,j_dim,l_dim]    :                                    + 
C**  +                            data field values                      + 
C**  +                            (var must be defined, and is REAL  )   + 
C**  +                                                                   + 
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNread
     &      (FILEid , VARname , time, level, i_dbeg, j_dbeg,
     &       i_dim   , j_dim   , l_dim    ,
     &       varax1  , varax2  , varlev,      
     &       var_units, var)

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      INTEGER icheck

      INTEGER Lvnam 
      PARAMETER (Lvnam=20)

C     ** input 
      INTEGER FILEid 
      INTEGER time, level, i_dbeg, j_dbeg
      INTEGER i_dim, j_dim, l_dim
      CHARACTER *(*) VARname 

C     ** output
      REAL*8    varax1(i_dim), varax2(j_dim), varlev(l_dim)
      CHARACTER *(*) var_units
      REAL*8 var (i_dim, j_dim, l_dim)

C     ** local :
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER Ierro, Nvatts, vtype
      INTEGER dimID(4), dimSIZ(4), dimREG(4)    
      INTEGER start(4),begREG(4),count(4),stride(4),imap(4)
      CHARACTER*(Lvnam) dimNAM(4) 
      CHARACTER*(Lvnam) dNAMsyg, dNAMtim
      CHARACTER*(Lvnam) recname
      CHARACTER*(10) Routine
      INTEGER ax1VID, ax2VID, sygVID, timVID, varVID
      INTEGER VNlen, varNUMDIM
      INTEGER ii, z
      
      icheck= 0
C*    0. Initialisations
C     ------------------
      Routine= 'UNread'
      IF (icheck.ge.1) WRITE(*,*) 'UNread : Begin'

      DO ii = 1,4 
        stride(ii) = 1
        begREG(ii) = 1
        start (ii) = 1
      ENDDO
 
C*    1. Get the variable field  and dims IDs
C     ----------------------------------------

      IF (icheck.ge.3) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting VARname  size :
      VNlen = VARSIZE(VARname)
      IF (icheck.ge.3) WRITE(*,*) 'VNlen  :',VNlen
      IF (icheck.ge.2) WRITE(*,*) 'VARname   :', VARname (1:VNlen)

C     ** variable field ID :
      Ierro=NF_INQ_VARID (FILEid , VARname (1:VNlen), varVID)
      IF (Ierro.NE.NF_NOERR) THEN 
         WRITE(*,*) 'Error reading variable: ', VARname(1:VNlen)
         CALL HANDLE_ERR(Routine,Ierro)
      ENDIF

C     ** Inquire about the number of dimensions in var :
C     **
      Ierro=NF_INQ_VAR(FILEid   , varVID, recname, vtype,
     &                   varNUMDIM, dimID , Nvatts )
C     **  line1          id/file    id/var  var name  var type
C     **  line2          # dims    id/dims #attributes
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)

      IF (icheck.ge.3) WRITE(*,*) 'Ierro1. ', Ierro


C*    2. Dimensions : in the reading region and in the file.
C     ------------------------------------------------------

C     ** inquire dimensions names and sizes :
      DO z = 1,varNUMDIM
        Ierro=NF_INQ_DIM(FILEid , dimID(z), dimNAM(z), dimSIZ(z))
C       **                 id/file  id/dim    dimname    dimsize
C       **                                    !output    output
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      END DO


C     ** In this version, we read only a xy subregion of the file :
      dimREG(1) = i_dim
      dimREG(2) = j_dim
      begREG(1) = i_dbeg
      begREG(2) = j_dbeg
      IF (begREG(1).lt.1)  begREG(1) = 1
      IF (begREG(2).lt.1)  begREG(2) = 1
      
C     ** Set reading region according to field dimension : 2D or 3D
      IF (varNUMDIM.eq.4) THEN
C       ** for 3D fields :
        IF (level.gt.0) THEN
C       ** one level is read :
          dimREG(3) = 1 
          begREG(3) = level
          dNAMsyg   = dimNAM(3)
        ELSE
C       ** all levels are read :
          dimREG(3) = l_dim
          begREG(3) = 1    
          dNAMsyg   = dimNAM(3)
        END IF
C       ** one time step is read:
        dimREG(4) = 1 
        begREG(4)  = time 
        dNAMtim   = dimNAM(4)
      ELSE IF (varNUMDIM.eq.3) THEN
C       ** for 2D space fields + time:
C       ** one time step is read:
        dimREG(3) = 1      
        begREG(3) = time
        dNAMtim   = dimNAM(3)
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE IF (varNUMDIM.eq.2) THEN
C       ** for 2D fields :
C       ** no time step is read:
        dimREG(3) = 0     
        begREG(3) = 0   
        dNAMtim   = 'none'   
        dimNAM(3) = 'none'
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE IF (varNUMDIM.eq.1) THEN
C       ** for 1D variable :
C       ** not assumed to be on a XYZ grid,       
C       ** just read a vector  
        dimREG(2) = 0
        begREG(2) = 0
        dimNAM(2) = 'none'
        dimREG(3) = 0
        begREG(3) = 0
        dimNAM(3) = 'none'
        dNAMtim   = 'none'
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE
        WRITE(*,*) 'UNread ERROR : data field dimension ?'
        STOP
      END IF

      DO z = 1,varNUMDIM
        IF (begREG(z).gt.dimSIZ(z)) THEN
          write(*,*) 'UNread - ERROR   : requested area out      '
          write(*,*) '                   of file area.          '
          write(*,*) '  (for the dimension:' , dimNAM(z) , ')'
          STOP
        END IF
        IF (dimSIZ(z).lt.(dimREG(z)+begREG(z)- 1) ) THEN
          write(*,*) 'UNread - WARNING : empty portion in field, '
          write(*,*) '  requested region > file contents       '
          write(*,*) '  (for the dimension:' , dimNAM(z) , ')'
          dimREG(z) = dimSIZ(z) - begREG(z) + 1
        END IF
      END DO

      IF (icheck.ge.3) WRITE(*,*) 'Ierro2. ', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'Dimension names :',dimNAM
      IF (icheck.ge.2) WRITE(*,*) 'dimSIZ :',dimSIZ
      IF (icheck.ge.2) WRITE(*,*) 'dimREG :',dimREG
      IF (icheck.ge.2) WRITE(*,*) 'begREG :',begREG
      IF (icheck.ge.3) WRITE(*,*) 'dimID  :',dimID 

C*    3. Get the variables IDs for the grid points locations. 
C     -------------------------------------------------------

      IF (varNUMDIM.ge.2) THEN
        Ierro=NF_INQ_VARID (FILEid, dimNAM(1), ax1VID)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
        Ierro=NF_INQ_VARID (FILEid, dimNAM(2), ax2VID)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      ENDIF
      IF (varNUMDIM.ge.3) THEN
        Ierro=NF_INQ_VARID (FILEid, dNAMtim, timVID)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      END IF
      IF (varNUMDIM.eq.4) THEN
        Ierro=NF_INQ_VARID (FILEid, dNAMsyg, sygVID)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      END IF
C     **                      id/file  name    id/var

      IF (icheck.ge.3) WRITE(*,*) 'Ierro3. ', Ierro

C*    4. Get attributes.         
C     ------------------

      IF (varNUMDIM.ge.2) THEN   !Not for 1D vectors (special case)
C       ** units attribute 
        Ierro=NF_GET_ATT_TEXT (FILEid , varVID, 'units', 
     &                           var_units) 
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)

        IF (icheck.ge.2) WRITE(*,*) 'var_units :', var_units
        IF (icheck.ge.3) WRITE(*,*) 'Ierro4. ', Ierro
      ENDIF

C*    5. Get values.
C     --------------
C*    5.1 ...for the grid points locations.
C     -------------------------------------
      
C     ** Horizontal : always read, except for 1D vectors
      IF (varNUMDIM.ge.2) THEN  
        count(1)=dimREG(1)
        start(1)=begREG(1)
        Ierro=NF_GET_VARA_DOUBLE(FILEid ,ax1VID,start,count,varax1)
C       **                       id/file id/var from  #data data
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
        count(1)=dimREG(2)
        start(1)=begREG(2)
        Ierro=NF_GET_VARA_DOUBLE(FILEid ,ax2VID,start,count,varax2)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      ENDIF

C     ** vertical :  only for 3D fields.
      IF (varNUMDIM.eq.4) THEN
        start(1) =begREG(3)
        count(1) =dimREG(3)
        Ierro =  NF_GET_VARA_DOUBLE(FILEid ,sygVID,start,count,varlev)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)
      END IF

      IF (icheck.ge.3) WRITE(*,*) 'Ierro5.1', Ierro

C*    5.2 ...for the the variable.
C     ----------------------------

C     ** Set 'imap' and READ with NCVGTG:
C     ** NOTE :                                                  
C     **        we use the 'generalised' reading routine NCVGTG 
C     ** (imap tells NetCDF about the memory locations of var) 
      imap(1) = 1
      imap(2) = imap(1) * i_dim  ! 1st dim of var = i_dim
      imap(3) = imap(2) * j_dim  ! 2nd dim of var = j_dim 
      imap(4) = 0                !  Should NEVER be used        

      Ierro=NF_GET_VARM_DOUBLE(FILEid   ,  varVID ,begREG      , dimREG,
     &                         stride   ,   imap  ,var(1,1,1)          )
C     **  line1:               id/file  | id/var  |read from...|#data 
C     **  line2:               step     |re-arrang|variable(beg.)
C     ** NOTE: stride is not used here. 
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR(Routine, Ierro)

      IF (icheck.ge.3) WRITE(*,*) 'Ierro5.2', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'UNread : End' 

      END

C**
C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNsread :   +                                         +
C**  +-------------------------+                                         +
C**  +  * Reads a model variable from a NetCDF file,                     +
C**  +    SIMPLIFIED VERSION of  UNread  : does NOT read coordinates.    +
C**  +                                                                   +
C**  +                                                                   +
C**  +  INPUT :                                                          + 
C**  +    FILEid  : input file identifier (from NetCDF open)             +
C**  +    VARname  : name of the requested variable.                     +
C**  +    time : [integer*4] is the time index of the data field to read +
C**  +    level: [integer*4] (usefull for 3D-space fields only) :        +
C**  +                       if not=0 --> = no of the level              +
C**  +                                      -> output is 2D (l_dim = 1)  +
C**  +                       if  =0   --> read ALL levels                +
C**  +                                      -> output is 3D              +
C**  +    i_dbeg, j_dbeg      : horizontal indexes of requested region   +
C**  +                          in input data file                       + 
C**  +    i_dim, j_dim, l_dim : ...the dimensions of 'var',              + 
C**  +                       = the dimensions of the sub-region to read  + 
C**  +                       ! l_dim = 1 if level not=0                  + 
C**  +                       ! j_dim = 1 if var is 1D                    + 
C**  +  OUTPUT :                                                         + 
C**  +    var_units                 : physical units of var.             + 
C**  +    var[i_dim,j_dim,l_dim]    :                                    + 
C**  +                            data field values                      + 
C**  +                            (var must be defined, and is REAL  )   + 
C**  +                                                                   + 
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNsread
     &      (FILEid, VARname, time, level, i_dbeg, j_dbeg,
     &                                     i_dim , j_dim , l_dim,
     &       var_units, var)

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      INTEGER icheck

      INTEGER Lvnam 
      PARAMETER(Lvnam=20)

C     ** input 
      INTEGER FILEid 
      INTEGER time, level, i_dbeg, j_dbeg
      INTEGER i_dim, j_dim, l_dim
      CHARACTER *(*) VARname 

C     ** output
      CHARACTER *(*) var_units
      REAL*8 var (i_dim, j_dim, l_dim)

C     ** local :
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER Ierro, Nvatts, vtype
      INTEGER dimID(4), dimSIZ(4), dimREG(4)    
      INTEGER start(4),begREG(4),count(4),stride(4),imap(4)
      CHARACTER*(Lvnam) dimNAM(4) 
      CHARACTER*(Lvnam) dNAMlev, dNAMtim
      CHARACTER*(Lvnam) recname
      INTEGER varVID
      INTEGER VNlen, varNUMDIM
      INTEGER ii, z
      
      icheck= 0
C*    0. Initialisations
C     ------------------
      IF (icheck.ge.1) WRITE(*,*) 'UNsread : Begin'

      DO ii = 1,4 
        stride(ii) = 1
        begREG(ii) = 1
        start (ii) = 1
      ENDDO
 
C*    1. Get the variable field  and dims IDs
C     ----------------------------------------

      IF (icheck.ge.3) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting VARname  size :
      VNlen = VARSIZE(VARname)
      IF (icheck.ge.3) WRITE(*,*) 'VNlen  :',VNlen
      IF (icheck.ge.2) WRITE(*,*) 'VARname   :', VARname (1:VNlen)

C     ** variable field ID :
      Ierro=NF_INQ_VARID (FILEid, VARname (1:VNlen), varVID)
      IF (Ierro.NE.NF_NOERR) THEN
         WRITE(*,*) 'Error reading variable: ', VARname(1:VNlen)
         CALL HANDLE_ERR('UNsread', Ierro)
      ENDIF

C     ** Inquire about the number of dimensions in var :
C     **
      Ierro=NF_INQ_VAR(FILEid   , varVID, recname, vtype,
     &                   varNUMDIM, dimID, Nvatts )
C     **  line1          id/file    id/var  var name  var type
C     **  line2          # dims    id/dims #attributes
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNsread', Ierro)

      IF (icheck.ge.3) WRITE(*,*) 'Ierro1. ', Ierro


C*    2. Dimensions : in the reading region and in the file.
C     ------------------------------------------------------

C     ** inquire dimensions names and sizes :
      DO z = 1,varNUMDIM
        Ierro=NF_INQ_DIM(FILEid , dimID(z), dimNAM(z), dimSIZ(z))
C       **                 id/file  id/dim    dimname    dimsize
C       **                                    !output    output
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNsread', Ierro)
      END DO


C     ** In this version, we read only a xy subregion of the file :
      dimREG(1) = i_dim
      dimREG(2) = j_dim
      begREG(1) = i_dbeg
      begREG(2) = j_dbeg
      IF (begREG(1).lt.1)  begREG(1) = 1
      IF (begREG(2).lt.1)  begREG(2) = 1
      
C     ** Set reading region according to field dimension : 2D or 3D
      IF (varNUMDIM.eq.4) THEN
C       ** for 3D fields :
        IF (level.gt.0) THEN
C       ** one level is read :
          dimREG(3) = 1 
          begREG(3) = level
          dNAMlev   = dimNAM(3)
        ELSE
C       ** all levels are read :
          dimREG(3) = l_dim
          begREG(3) = 1    
          dNAMlev   = dimNAM(3)
        END IF
C       ** one time step is read:
        dimREG(4) = 1 
        begREG(4) = time 
        dNAMtim   = dimNAM(4)
      ELSE IF (varNUMDIM.eq.3) THEN
C       ** for 2D space fields + time:
C       ** one time step is read:
        dimREG(3) = 1      
        begREG(3) = time
        dNAMtim   = dimNAM(3)
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE IF (varNUMDIM.eq.2) THEN
C       ** for 2D fields :
C       ** no time step is read:
        dNAMtim   = 'none'
        dimREG(3) = 0     
        begREG(3) = 0   
        dimNAM(3) = 'none'
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE IF (varNUMDIM.eq.1) THEN
C       ** for 1D variable :
C       ** not assumed to be on a XYZ grid,       
C       ** just read a vector  
        dNAMtim   = 'none'
        dimREG(2) = 0
        begREG(2) = 0
        dimNAM(2) = 'none'
        dimREG(3) = 0
        begREG(3) = 0
        dimNAM(3) = 'none'
        dimREG(4) = 0
        begREG(4) = 0
        dimNAM(4) = 'none'
      ELSE
         WRITE(*,*) 'UNsread ERROR : data field dimension ?'
         STOP
      END IF

      DO z = 1,varNUMDIM
        IF (begREG(z).gt.dimSIZ(z)) THEN
          write(*,*) 'UNsread - ERROR   : requested area out      '
          write(*,*) '                    of file area.        '
          write(*,*) '  (for the dimension:' , dimNAM(z) , ')'
          STOP
        END IF
        IF (dimSIZ(z).lt.(dimREG(z)+begREG(z)- 1) ) THEN
          write(*,*) 'UNsread - WARNING : empty portion in field, '
          write(*,*) '  requested region > file contents       '
          write(*,*) '  (for the dimension:' , dimNAM(z) , ')'
          dimREG(z) = dimSIZ(z) - begREG(z) + 1
        END IF
      END DO

      IF (icheck.ge.3) WRITE(*,*) 'Ierro2. ', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'Dimension names :',dimNAM
      IF (icheck.ge.2) WRITE(*,*) 'dimSIZ :',dimSIZ
      IF (icheck.ge.2) WRITE(*,*) 'dimREG :',dimREG
      IF (icheck.ge.2) WRITE(*,*) 'begREG :',begREG
      IF (icheck.ge.3) WRITE(*,*) 'dimID  :',dimID 

C*    4. Get attributes.         
C     ------------------

      IF (varNUMDIM.ge.2) THEN   !Not for 1D vectors (special case)
C       ** units attribute 
        Ierro=NF_GET_ATT_TEXT (FILEid , varVID, 'units', 
     &                           var_units) 
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNsread', Ierro)

        IF (icheck.ge.2) WRITE(*,*) 'var_units :', var_units
        IF (icheck.ge.3) WRITE(*,*) 'Ierro4. ', Ierro
      ENDIF

C*    5. Get value for the the variable.
C     ----------------------------------

C     ** Set 'imap' and READ with NCVGTG:
C     ** NOTE :                                                  
C     **        we use the 'generalised' reading routine NCVGTG 
C     ** (imap tells NetCDF about the memory locations of var) 
      imap(1) = 1
      imap(2) = imap(1) * i_dim  ! 1st dim of var = i_dim
      imap(3) = imap(2) * j_dim  ! 2nd dim of var = j_dim 
      imap(4) = 0                !  Should NEVER be used        

      Ierro=NF_GET_VARM_DOUBLE(FILEid   ,  varVID ,begREG      ,dimREG,
     &                         stride   ,   imap  ,var(1,1,1)         )
C     **  line1:               id/file  | id/var  |read from...|#data 
C     **  line2:               step     |re-arrang|variable(beg.)
C     ** NOTE: stride is not used here. 
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNsread', Ierro)

      IF (icheck.ge.3) WRITE(*,*) 'Ierro5. ', Ierro
      IF (icheck.ge.2) WRITE(*,*) 'UNsread : End' 

      END


C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNscreate : +                                         +
C**  +-------------------------+                                         +
C**  +  * Purpose :                                                      +
C**  +     Create a NetCDF file, general version.                        +
C**  +     (Staggered grids + other extensions to UNcreate)              +
C**  +                                                                   +
C**  +  * How it works : calling routine must provide                    +
C**  +    -a list of dimensions                                          +
C**  +     (size of each dimens., names, units and values of coordinates)+
C**  +    -a list of variables                                           +
C**  +     (units, number of dimensions, names of selected dimensions)   +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +  -------                                                          +
C**  +                                                                   +
C**  +  General :                                                        +
C**  +   FILEnam          [char]: Name of the file to be created.        +
C**  +   title            [char]: Title attribute                        +
C**  +                                                                   +
C**  +  Dimensions:                                                      +
C**  +   TND                    : Total Number of SPATIAL dimensions     +
C**  +                            Notice : Set "time" to dimension No 0  +
C**  +   DFdim(0:TND)           : # discrete values for each dimension   +
C**  +                            Notice : DFdim(0).eq.0                 +
C**  +                            -> UNLIMITED TIME (coord. not defined) +
C**  +                               WARNING: In this case, the NetCDF   +
C**  +                               use a temporary space to duplicate  +
C**  +                               the file -> NOT RECOMMENDED         +
C**  +   MXdim                  : Maximum value of DFdim, = arrays size  +
C**  +   NAMdim(0:TND)    [char]: Name of dimensions, except time        +
C**  +   UNIdim(0:TND)    [char]: Units of dimensions (attribute)        +
C**  +   VALdim(MXdim,0:TND)[R4]: Values of coordinate for each dimension+
C**  +                                                                   +
C**  +  Variables:                                                       +
C**  +   Dvs                    : Variable's definitions array sizes,    +
C**  +   Nvs                    : Number of defined variables(Nvs.le.Dvs)+
C**  +   name_vs (Dvs)    [char]: name of variable.                      +
C**  +   unit_vs (Dvs)    [char]: physical units of variable (attribute) +
C**  +   Sdim_vs (4,Dvs)  [char]: name of Selected dims (in above list)  +
C**  +                            Blanked or '-' elements = not used     +
C**  +   lnam_vs (Dvs)    [char]: Long_name attribute (descript. of var.)+
C**  +                                                                   +
C**  +  List of real attributes to all variables:                        +
C**  +   Nra                    : Number of Real Attributes (.ge.1 !)    +  
C**  +   NAMrat(Nra)      [char]: NAMes of Real ATtributes  (''=none)    +
C**  +                            (initial value= 0; set it with UNwratt)+
C**  +   Nvals(Nra)             : Number of values of these attributes.  +
C**  +   ! Currently limited to 1 value (scalar) or 2 (2 elements vector)+
C**  +   ! EXCEPTION: Setting the last attribute name to '[var]_range'   +
C**  +                does create a variable (!) for level-by-level range+
C**  +                (very usefull for 3D + time fields)                +
C**  +                                                                   +
C**  +  NB : [char] variables may have any length.                       +
C**  +       blanks characters are NOT ALLOWED in any variable,          +
C**  +          except the "title".                                      +
C**  +          and the NetCDF variables defined here are always real*4  +
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +  --------                                                         +
C**  +   FILEid                 : Index of the NetCDF file (remains open)+
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNscreate (FILEnam, title,
     &      TND, DFdim, MXdim, NAMdim, UNIdim, VALdim, 
     &      Dvs, Nvs, name_vs, Sdim_vs, unit_vs, lnam_vs,
     &      Nra, NAMrat, Nvals,
     &      FILEid )

C +
      IMPLICIT NONE
 
      INCLUDE 'NetCDF.inc'

C +
      INTEGER icheck, MXND
C     ** Maximum number of dimensions 
      parameter (MXND = 100) 

C +   INPUT:      
C +   - - -
      CHARACTER *(*) FILEnam
      CHARACTER *(*) title  

      INTEGER TND, DFdim(0:TND), MXdim
      CHARACTER *(*) NAMdim(0:TND)   
      CHARACTER *(*) UNIdim(0:TND)
      REAL*8 VALdim(MXdim,0:TND)

      INTEGER Nvs, Dvs
      CHARACTER *(*) name_vs(Dvs)
      CHARACTER *(*) Sdim_vs(4,Dvs)
      CHARACTER *(*) unit_vs(Dvs)
      CHARACTER *(*) lnam_vs(Dvs)

      INTEGER Nra
      CHARACTER *(*) NAMrat(Dvs)
      INTEGER Nvals(Nra)

C +   OUTPUT:    
C +   - - - -
      INTEGER FILEid 

C +   LOCAL:
C +   - - -
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      CHARACTER*(30) tmpchr
      INTEGER dimDID(0:MXND)
      INTEGER dimVID(0:MXND), vsVID, vrVID
      INTEGER dID(4), start(4), count(4), rdID(2)
      INTEGER mimaID
      INTEGER stride(4),imap(4)
      INTEGER Ndim_vs
      INTEGER ivs, igd, idi, ira, itmp
      INTEGER Nlen
      INTEGER dNlen(0:MXND)  
      INTEGER Ierro, TTerr, ii
      REAL*8   zero1(1), zero2(2)

      icheck= 0 !Debugging level
C*    0. Initialisations
C     ------------------
      IF (icheck.ge.1) WRITE(*,*) 'UNscreate : Begin'

      DO ii = 1,4
        stride(ii) = 1
      ENDDO
      zero1(1) = 0.
      zero2(1) = 0.
      zero2(2) = 0.
      TTerr = 0 !Total of error flags

      IF (TND .gt. MXND) THEN
        write(*,*)'UNscreate - Error: so much dimensions ?',TND
      END IF

C     Create a NetCDF file and enter define mode :
C     --------------------------------------------
      IF (icheck.ge.2) WRITE(*,*) 'FILEnam :', FILEnam

C     ** getting FILEnam [char] size :
      Nlen = VARSIZE(FILEnam)

      Ierro=NF_CREATE(FILEnam(1:Nlen), NF_CLOBBER , FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
C     ** identif.                       =>overwrite =error

C*    Time coordinate definition.
C     ---------------------------

C     ** Define dimension :    
      IF (icheck.ge.3) WRITE(*,*) '# time iters.:', DFdim(0)
      IF (DFdim(0).eq.0.) THEN
        Ierro=NF_DEF_DIM(FILEid , 'time', NF_UNLIMITED, dimDID(0))
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)
      ELSE
        Ierro=NF_DEF_DIM(FILEid , 'time', DFdim(0), dimDID(0))
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)
      END IF
      dNlen(0)= 4  ! 4 characters in the name 'time'...
      IF (NAMdim(0)(1:4).ne.'time') THEN
        WRITE(*,*) 'Sorry, NAMdim(0) must be ''time'' .'
        STOP
      END IF
       
C     ** Define variable for the time coordinate values :
      dID(1)    = dimDID(0)
      Ierro=NF_DEF_VAR(FILEid , 'time', NF_FLOAT,1 , dID,  dimVID(0))
C     **      ^^^^^^^^^^ FILEid  var name  type  dims  DIMid VARid  
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
      TTerr = TTerr + ABS(Ierro)


C     Spatial coordinates definitions : DIMS and VARs (locations).
C     ------------------------------------------------------------
C
      DO igd = 1,TND            !** BEGIN LOOP over all spatial dims

C       ** getting NAMdim [char] size :
        Nlen = VARSIZE(NAMdim(igd)) 
        dNlen(igd) = Nlen  !For further use of NAMdim

        Ierro=NF_DEF_DIM(FILEid    , NAMdim(igd)(1:Nlen),
     &                     DFdim(igd),dimDID(igd))
C       **line1 ^^^^^^^^^^ FILEid    | dim name            
C       **line2            # values  | VARid
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)

        dID(1)     = dimDID(igd)  
        Ierro=NF_DEF_VAR(FILEid    , NAMdim(igd)(1:Nlen),
     &                     NF_FLOAT  ,    1  , dID     ,dimVID(igd))
C       **line1 ^^^^^^^^^^ FILEid    | dim name            
C       **line2            type      | #dims | dimsIDs | VARid 
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)

      END DO                    !** END   LOOP over all spatial dims

C     Special coordinate definition: MinMax (for [var]_range)
C     -------------------------------------------------------
      IF (NAMrat(Nra)(1:11).eq.'[var]_range') THEN

        Ierro=NF_DEF_DIM(FILEid, 'MinMax', 2, mimaID)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
      ENDIF

C     Define the fields. 
C     ------------------

      DO ivs = 1,Nvs             !**BEGIN LOOP on var. num.
        IF (icheck.ge.3)
     &    WRITE (*,*) 'Defining variable ',name_vs(ivs)


C       Set space and time dimensions
C       - - - - - - - - - - - - - - -
C        ** Initialise number of dimensions :
         Ndim_vs= 0 

         DO idi = 1, 4           !** BEGIN LOOP on var dims.
         IF  (Sdim_vs(idi,ivs)(1:1).ne.' '
     &   .and.Sdim_vs(idi,ivs)(1:1).ne.'-') THEN !**skip undefined. 

C         ** getting Sdim_vs [char] size :
          Nlen =  VARSIZE(Sdim_vs(idi,ivs))

C         ** Searching for the dimension index from its name (Sdim_vs)    
          igd = 0
          DO WHILE (Sdim_vs(idi,ivs)(1:Nlen)
     &        .ne. NAMdim(igd)(1:dNlen(igd)) )
            IF (igd.eq.TND) THEN 
              write(*,*)'UNscreate-ERROR: Dimension not found:',
     &              Sdim_vs(idi,ivs)(1:Nlen)
              STOP
            END IF
            igd = igd + 1
          END DO               
C         ** Construct the dimensions id's for that variable (ivs):
          IF (icheck.ge.3)
     &       WRITE (*,*) 'using dimension ',NAMdim(igd), dimDID(igd)
          Ndim_vs      = Ndim_vs + 1
          dID(Ndim_vs) = dimDID(igd) 

        END IF
        END DO                   !** END   LOOP on var dims.

C       Define our special [var]_range field for 4D variables
C       - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF  (Ndim_vs.eq.4 
     &  .and.NAMrat(Nra)(1:11).eq.'[var]_range') THEN 

          Nlen = VARSIZE(name_vs(ivs))
          rdID(1)  = dID (3)  !(4D variable, 3th dim = level)
          rdID(2)  = mimaID   !(for min, max)
          tmpchr = name_vs(ivs)(1:Nlen)//'_range'
          itmp   = Nlen + 6
          Ierro =  NF_DEF_VAR(FILEid,tmpchr(1:itmp),
     &                        NF_FLOAT, 2, rdID, vrVID)
          IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
          TTerr = TTerr + ABS(Ierro)

        ENDIF

C       Define fields :
C       - - - - - - - -
        Nlen = VARSIZE(name_vs(ivs))
        Ierro=NF_DEF_VAR(FILEid , name_vs(ivs)(1:Nlen),
     &                     NF_FLOAT, Ndim_vs, dID     , vsVID)
C       **line1 ^^^^^^^^^^ FILEid | variable name
C       **line2            type   | #dims   | dimsIDs | VARid
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)


C     Set the variable's attributes : 
C     -------------------------------

C       ** Units: 
C       - - - - - 
C       ** getting unit_vs [char] size :
        Nlen = VARSIZE(unit_vs(ivs))

        Ierro=NF_PUT_ATT_TEXT(FILEid , vsVID ,'units',
     &                          Nlen   ,unit_vs(ivs)(1:Nlen))
c       **line1 ^^^^^^^^^^^^^^^ FILEid |var.id | attr.name
C       **line2                 length | attr.value
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)

C       ** "long_name":
C       - - - - - - - -
C       ** getting unit_vs [char] size :
        Nlen = VARSIZE(lnam_vs(ivs))

        IF (icheck.ge.3)
     &    WRITE (*,*) 'Write long_name ',lnam_vs(ivs)(1:Nlen)

        Ierro=NF_PUT_ATT_TEXT(FILEid , vsVID ,'long_name',
     &                          Nlen  ,lnam_vs(ivs)(1:Nlen)     )
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)


C       ** List of real attributes: 
C       - - - - - - - - - - - - - -
C
        DO ira = 1, Nra
        IF   (NAMrat(ira)(1:1).ne.' '
     &   .and.NAMrat(ira)(1:11).ne.'[var]_range') THEN 
C          ** getting NAMrat [char] size :
           Nlen = VARSIZE(NAMrat(ira))

           IF (Nvals(ira).eq.1) THEN
             Ierro=NF_PUT_ATT_DOUBLE(FILEid,vsVID,NAMrat(ira)(1:Nlen),
     &                               NF_FLOAT,   Nvals  , zero1      )
             IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
             TTerr = TTerr + ABS(Ierro)
           ELSE IF (Nvals(ira).eq.2) THEN
             Ierro=NF_PUT_ATT_DOUBLE(FILEid,vsVID,NAMrat(ira)(1:Nlen),
     &                               NF_FLOAT, Nvals  , zero2        )
             IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
             TTerr = TTerr + ABS(Ierro)
c
           END IF
        END IF
        END DO

      END DO                     ! **END   LOOP on var. num.

C     Set 'unit' attribute for the dimensions:
C     ----------------------------------------

      DO igd = 0,TND         !** BEGIN LOOP over all spatial dims

C       ** getting NAMdim [char] size :
        Nlen = VARSIZE(UNIdim(igd))

        Ierro=NF_PUT_ATT_TEXT(FILEid , dimVID(igd),'units',
     &                          Nlen   , UNIdim(igd)        )
c       **line1 ^^^^^^^^^^^^^^^ FILEid | var.id      | attr.name
c       **line2                 length |attr.value
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
        TTerr = TTerr + ABS(Ierro)

      ENDDO

C     Global attribute(s).
C     --------------------

C     ** Title (some general file descriptor) :
C     ** getting unit_vs [char] size :
      Nlen = VARSIZE(title)

      Ierro=NF_PUT_ATT_TEXT(FILEid ,NF_GLOBAL,'title',
     &                      Nlen    ,title(1:Nlen)       )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
      TTerr = TTerr + ABS(Ierro)


C     Leave define mode (!file remains open )
C     ---------------------------------------
      Ierro=NF_ENDDEF(FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)
      TTerr = TTerr + ABS(Ierro)


C     Writing of dimensions coordinates.
C     ----------------------------------

C     ** Time :
C     - - - - -

      start(1)= 1          !Vector of starting indexes values
      count(1)= DFdim(0)   !Vector of total # indexes values
        IF (icheck.ge.3)
     &    WRITE (*,*) 'Write coords for ',NAMdim(0),count(1)

C     ** Set 'imap' to write with NCVPTG; NCVPT could be enough ?
C     ** (imap tells NetCDF about the memory locations of var,
C     **  we choose NCVPTG because 
C     **  only a portion of VALdim is written.)
      imap(1) = 1
      imap(2) = 0                 ! Not used : write only 1 coord.

      Ierro=NF_PUT_VARM_DOUBLE(FILEid ,dimVID(0), start        , count,
     &                         stride , imap    , VALdim(1,0)         )
C     **line 1 ^^^^^^^^^^^^^^^ ID file| id var. |read from...  |#data
C     **line 2                 step   |re-arrang|variable(beg.)
C     **                      (^^^^stride is not used)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)


C     ** Space coordinates :
C     - - - - - - - - - - - -

      DO igd = 1,TND          !** BEGIN LOOP over all spatial dims

        start(1)= 1
        count(1)= DFdim(igd)
        IF (icheck.ge.3)
     &    WRITE (*,*) 'Write coords for ',NAMdim(igd),count(1)


        Ierro=NF_PUT_VARM_DOUBLE(FILEid ,dimVID(igd),start , count,
     &                           stride , imap      ,VALdim(1,igd))
C       **      ^^^^^^^^^^^^^^^^ see above
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNscreate', Ierro)

        TTerr = TTerr + ABS(Ierro)
 
      END DO                  !** END   LOOP over all spatial dims

C     Stop if an error occured.
C     -------------------------

      IF (TTerr.ne.0) THEN 
        STOP 'UNscreate : Sorry, an error occured.'
      ENDIF

C +
      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNwcatt :   +                                         +
C**  +-------------------------+                                         +
C**  +  *Character Attributes creation and (over)writing                 +
C**  +    (the NetCDF file must be open, in data mode)                   +
C**  +  *WARNING: this routine uses a temporary disk space               +
C**  +            equal to the file length (duplicate the file)          +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : input file identifier (from UNcreate OR NetCDF open) +
C**  +    varnam  : name of variable to which attribute shall be attached+
C**  +    attnam  : name of writen attribute.                            +
C**  +    attval  : string to be assigned to attribute.                  +
C**  +              (never inclulde more than 3 consecutive blanks !)    +
c**  +                                                                   +
C**  +  Note : all arguments except FILEid  are strings of any length    +
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNwcatt (FILEid , varnam, attnam, attval)

      INCLUDE 'NetCDF.inc'

C     **Input:

      INTEGER FILEid 
      CHARACTER*(*) varnam
      CHARACTER*(*) attnam
      CHARACTER*(*) attval

C     **Local:
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER Nlen, Ierro, varVID, Vlen, TTerr
      INTEGER icheck
      icheck= 0     !** 'debugging' level

      IF (icheck.ge.1) WRITE(*,*) 'UNwcatt : Begin'

C*    Get the variable ID
C     -------------------

      IF (icheck.ge.2) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting varnam size :
      Nlen = VARSIZE(varnam)

C     ** variable ID :
      Ierro=NF_INQ_VARID (FILEid , varnam(1:Nlen), varVID)
      TTerr = ABS(Ierro)

C     ** Cancel writing if an error occured : variable undefined ?
      IF (Ierro.ne.0) THEN
         WRITE(*,*) 'UNwcatt -ERROR : Variable ',varnam(1:Nlen)
     &            ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNwcatt_end

C     Switch to Define Mode, 
C       because attribute may be created or change size.
C     --------------------------------------------------
      Ierro=NF_REDEF (FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwcatt', Ierro)

C     Set attribute.
C     --------------

C     ** getting attnam [char] size :
      Nlen = VARSIZE(attnam)
C     ** getting attval [char] size :
      Vlen = VARSIZE(attval)

      Ierro=NF_PUT_ATT_TEXT(FILEid ,varVID ,attnam(1:Nlen),
     &                       Vlen  ,attval(1:Vlen)      )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwcatt', Ierro)
c     **line1^^^^ FILEid |var.id | attr.name
C     **line2     type   | len   | attr.value | flag
      TTerr = TTerr + ABS(Ierro)


C     Leave define mode (!file remains open )
C     ---------------------------------------
      Ierro=NF_ENDDEF(FILEid )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwcatt', Ierro)

 9999 continue
      RETURN
      END 

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNwratt :   +                                         +
C**  +-------------------------+                                         +
C**  +  *Real   attributes writing  - ! Can not create new attrib !      +
C**  +    (the NetCDF file must be open)                                 +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : input file identifier (from UNcreate OR NetCDF open) +
C**  +    varnam  : name given to the variable to write (must be in file)+
C**  +    attnam  : name of treated attribute.                           +
c**  +    Nvals   : Number of values of that attribute                   +
C**  +    atvalsi(Nvals) : Real   vector of values for attribute.        +
c**  +                                                                   +
C**  +-------------------------------------------------------------------+

      SUBROUTINE UNwratt (FILEid , varnam, attnam, Nvals, atvals)

      INCLUDE 'NetCDF.inc'

C     **Input:

      INTEGER FILEid , Nvals
      CHARACTER*(*) varnam
      CHARACTER*(*) attnam
      REAL*8   atvals(Nvals)

C     **Local:
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      INTEGER Nlen, Ierro, varVID
      INTEGER icheck, TTerr
      icheck= 0     !** 'debugging' level
      TTerr = 0

      IF (icheck.ge.1) WRITE(*,*) 'UNwratt : Begin'

C*    Get the variable ID
C     -------------------
      IF (icheck.ge.2) WRITE(*,*) 'FILEid  :', FILEid 

C     ** getting varnam size :
      Nlen = VARSIZE(varnam)

C     ** variable ID :
      Ierro=NF_INQ_VARID(FILEid , varnam(1:Nlen), varVID)
      TTerr = TTerr + ABS(Ierro)

C     ** Cancel writing if an error occured : variable undefined ?
      IF (Ierro.ne.0) THEN
         WRITE(*,*) 'UNwratt -ERROR : Variable ',varnam(1:Nlen)
     &            ,' not found -> not written.'
      END IF
      IF (Ierro.ne.0) GOTO 9999 !** UNwratt_end


C     Set attribute.
C     --------------

C     ** getting attnam [char] size :
      Nlen = VARSIZE(attnam)

      Ierro=NF_PUT_ATT_DOUBLE(FILEid ,varVID ,attnam(1:Nlen),
     &           NF_FLOAT,nvals  ,atvals  )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNwratt', Ierro)
c     **line1^^^^FILEid |var.id | attr.name
C     **line2    type   | attr.value | flag
      TTerr = TTerr + ABS(Ierro)


 9999 continue
      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNwopen :   +                            libUN (0896) +
C**  +-------------------------+-----------------------------------------+
C**  +  * Open a NetCDF file for writing.                                +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEnam : file name                                            +
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +    FILEid  : NetCDF file identifier ('logical unit')              +
C**  +---------------------------------------------------------------7++++
 
      SUBROUTINE UNwopen (FILEnam, FILEid )

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

C     ** input
      CHARACTER*(*) FILEnam

C     ** output
      INTEGER FILEid

C     ** local :
      INTEGER Ierro
      INTEGER icheck

      icheck=0
C     ** Open NetCDF file, for read-only:
C     -----------------------------------
      Ierro=NF_OPEN(FILEnam,NF_WRITE,FILEid)
      IF (Ierro.NE.NF_NOERR) THEN
         WRITE(*,*) 'Error opening file: ', FILEnam            
         CALL HANDLE_ERR('UNwopen', Ierro)
      ENDIF


9999  continue
      RETURN
      END



C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNropen :   +                            libUN (0896) +
C**  +-------------------------+-----------------------------------------+
C**  +  * Open a NetCDF file for reading,                                +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEnam : file name                                            +
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +    FILEid  : NetCDF file identifier ('logical unit')              +
C**  +    FILEtit : title of the NetCDF file                             +
C**  +              ! [CHAR], must be defined (length > length(title) !) +
C**  +---------------------------------------------------------------7++++

      SUBROUTINE UNropen (FILEnam, FILEid , FILEtit)

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

C     ** input
      CHARACTER*(*) FILEnam

C     ** output
      INTEGER FILEid      
      CHARACTER*(*) FILEtit

C     ** local :
      INTEGER Ierro
      INTEGER icheck

      icheck=0

      IF (icheck.ge.2) WRITE(*,*) 'UNropen: Begin'
      IF (icheck.ge.2) WRITE(*,*) 'FILEnam: ', FILEnam

C     ** Open NetCDF file, for read-only:
C     -----------------------------------
      Ierro=NF_OPEN(FILEnam,NF_NOWRITE,FILEid)
      IF (Ierro.NE.NF_NOERR) THEN
         WRITE(*,*) 'Error opening file: ', FILEnam
         CALL HANDLE_ERR('UNropen', Ierro)
      ENDIF


C     ** Read title attribute, 
C     ------------------------

C     ** Read attribute:
      Ierro=NF_GET_ATT_TEXT(FILEid, NF_GLOBAL, 'title', 
     &             FILEtit)

C     ** Display message if an error occured : 
C     **  no title or title too long ? 
      IF (Ierro.ne.0) THEN
         WRITE(*,*) 'UNropen WARNING: no title or title too long' 
      END IF
      IF (icheck.ge.2) WRITE(*,*) 'UNropen: End'

9999  continue
      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNgtime :   +                            libUN (0896) +
C**  +-------------------------+-----------------------------------------+
C**  +  * From a given value of desired 'time' coordinate,               +
C**  +    gets the coordinate index ('iteration no') + found time value  +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : NetCDF file identifier (from UNropen)                +
C**  +    RQtime  : ReQuested time                                       +
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +    RDtime  : The last time for wich RDtime .le. RQtime            +
C**  +    Ftime   : The next time value Following RDtime                 +
C**  +              (-1 if it would be after end-of-file)                +
C**  +    it      : The time index : RDtime = time(it)                   +
C**  +---------------------------------------------------------------7++++

      SUBROUTINE UNgtime (FILEid, RQtime, RDtime, Ftime, it) 

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      INTEGER Lvnam
      PARAMETER (Lvnam=20)

C     ** input
      INTEGER FILEid 
      REAL*8    RQtime

C     ** output
      REAL*8    RDtime, Ftime
      INTEGER it

C     ** local :
      INTEGER Ierro, timVID
      INTEGER timDID
      REAL*8    gtim
      INTEGER K, KHI, KLO, Kmax
      INTEGER Mindex(1)
      INTEGER icheck
      CHARACTER*(Lvnam) dimNAM(1)

      icheck= 0

C     ** Kmax= nb pas de temps dans le fichier, = dim(time):
C     ** - - - - - - - - - - - - - - - - - - - - - - - - - -
C     
      Ierro=NF_INQ_DIMID(FILEid, 'time', timDID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)
C     **^^ Dimension'time' NetCDF index

      Ierro=NF_INQ_DIM(FILEid, timDID , dimNAM, Kmax  )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)
C     **         id/file  id/dim   dimname dimsize  error
C     **                           !output output

C     ** Read/Search the requested time step.
C     ** - - - - - - - - - - - - - - - - - - -

      Ierro=NF_INQ_VARID(FILEid, 'time',timVID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)
C                                         **^^ Variable 'time' NetCDF index

      KLO=1
      KHI=Kmax

 1    IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2

C       ** Set the position of the needed time step:
        Mindex(1)= K
C       ** Get 1 time value (gtim = time(K)):
        Ierro=NF_GET_VAR1_DOUBLE(FILEid, timVID, Mindex, gtim)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)

        IF(gtim.GT.RQtime)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      it= KLO
C     ** read RDtime= time(KLO)
      Mindex(1)= KLO
      Ierro=NF_GET_VAR1_DOUBLE(FILEid, timVID, Mindex, RDtime)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)
C     ** read Ftime= time(KHI)
      Mindex(1)= KHI
      Ierro=NF_GET_VAR1_DOUBLE(FILEid, timVID, Mindex, Ftime)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgtime', Ierro)
 
C     ** IF the last available time step is before
C     **     the requested time, then KHI and KLO are the
C     **     two last available time step. Correct this :
      IF (RQtime.ge.Ftime) THEN
        RDtime= Ftime                   
        it = KHI
        Ftime= -1.0
      ENDIF

      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNgindx :   +                            libUN (0199) +
C**  +-------------------------+-----------------------------------------+
C**  +  * From a given value of a desired coordinate,                    +
C**  +    gets the coordinate index + found the coresp. coordinate value +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : NetCDF file identifier (from UNropen)                +
C**  +    Cname   : The name of the coordinate                           +
C**  +    RQval   : The requested value for that coordinate              +
C**  +                                                                   +
C**  +  OUTPUT :                                                         +
C**  +    RDval   : The last value for wich RDval .le. RQval             +
C**  +    Fval    : The next val value Following RDval                   +
C**  +              (-1 if it would be after end-of-file)                +
C**  +    indx    : The val index : RDval = value_of_Cname(it)           +
C**  +---------------------------------------------------------------7++++

      SUBROUTINE UNgindx (FILEid, Cname, RQval, RDval, Fval, indx)

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      INTEGER Lvnam
      PARAMETER (Lvnam=20)

C     ** input
      INTEGER FILEid
      CHARACTER *(*) Cname
      REAL*8    RQval

C     ** output
      REAL*8    RDval, Fval
      INTEGER indx 

C     ** local :
      INTEGER  VARSIZE
      EXTERNAL VARSIZE
      REAL*8    gval
      INTEGER Ierro
      INTEGER varDID, VNlen, varVID, varNUMDIM
      INTEGER Nvatts, vtype
      INTEGER K, KHI, KLO, Kmax
      INTEGER Mindex(1), dimID(4)
      INTEGER icheck
      CHARACTER*(Lvnam) dimNAM(4)
      CHARACTER*13 recname

      icheck= 0

C     ** Kmax= nb pas de temps dans le fichier, = dim(val):
C     ** - - - - - - - - - - - - - - - - - - - - - - - - - -
C     ** get Cname string size :
      VNlen = VARSIZE (Cname)
C
C     ** get variable ID :
      Ierro=NF_INQ_VARID(FILEid , Cname (1:VNlen), varVID)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)
C
C     ** Inquire about the id of the dimension:
C     **
      Ierro=NF_INQ_VAR(FILEid , varVID, recname, vtype,
     &          varNUMDIM, dimID , Nvatts)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)
C     **  line1  id/file   id/var  var name  var type
C     **  line2   # dims   id/dims #attributes
      varDID = dimID(1) 
C     ^^^At last, the id of the relevant dimension.

      Ierro=NF_INQ_DIM(FILEid, varDID , dimNAM, Kmax  )
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)
C     **         id/file  id/dim   dimname dimsize  error
C     **                           !output output
C     ** (Kmax is what we needed: size of the dimension)

C     ** Read/Search the requested val step.
C     ** - - - - - - - - - - - - - - - - - - -

      KLO=1
      KHI=Kmax

 1    IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2

C       ** Set the position of the needed val step:
        Mindex(1)= K
C       ** Get 1 val value (gval = val(K)):
        Ierro=NF_GET_VAR1_DOUBLE(FILEid, varVID, Mindex, gval)
        IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)

        IF(gval.GT.RQval)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      indx= KLO
C     ** read RDval= val(KLO)
      Mindex(1)= KLO
      Ierro=NF_GET_VAR1_DOUBLE(FILEid, varVID, Mindex, RDval)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)
C     ** read Fval= val(KHI)
      Mindex(1)= KHI
      Ierro=NF_GET_VAR1_DOUBLE(FILEid, varVID, Mindex, Fval)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNgindex', Ierro)

C     ** IF the last available val step is before
C     **     the requested val, then KHI and KLO are the
C     **     two last available val step. Correct this :
      IF (RQval.ge.Fval) THEN
        RDval= Fval
        indx = KHI
        Fval= -1.0
      ENDIF

      RETURN
      END

C**  +-------------------------+-----------------------------------------+
C**  +  Subroutine UNclose :   +                            libUN (0300) +
C**  +-------------------------+-----------------------------------------+
C**  +  * Close the desired file                                         +
C**  +    Created to suppress the need the directly call a netcdf        +
C**  +    routine from a program                                         +
C**  +                                                                   +
C**  +  INPUT :                                                          +
C**  +    FILEid  : NetCDF file identifier (from UNropen)                +
C**  +---------------------------------------------------------------7++++

      SUBROUTINE UNCLOSE(FILEid)

      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      integer Ierro, FILEid

      Ierro=NF_CLOSE(FILEid)
      IF (Ierro.NE.NF_NOERR) CALL HANDLE_ERR('UNclose', Ierro)

      END

C**  +-------------------------------------------------------------------+
      FUNCTION VARSIZE(CHAvar)
C**  +-------------------------------------------------------------------+
      IMPLICIT NONE
      integer maxcha,iz,VARSIZE
      parameter (maxcha=127)
      character*(*)      CHAvar
      character*(maxcha) CHAtmp

      WRITE(CHAtmp,'(A)') CHAvar
      iz = 0
      do while ((CHAtmp(iz+1:iz+3).ne.'   ').and.(iz+3.le.maxcha))
        iz = iz + 1
      end do
      VARSIZE =  iz

      RETURN
      END


C**  +-------------------------------------------------------------------+
      SUBROUTINE HANDLE_ERR(LOCATION, STATUS)
C**  +-------------------------------------------------------------------+
      IMPLICIT NONE
      INCLUDE 'NetCDF.inc'

      character*(*) LOCATION
      integer STATUS
      IF (STATUS.NE.NF_NOERR) THEN
        WRITE(*,*) 'IN ROUTINE ', LOCATION
        WRITE(*,*) NF_STRERROR(STATUS)
        STOP 'Stopped'
      ENDIF
      END

