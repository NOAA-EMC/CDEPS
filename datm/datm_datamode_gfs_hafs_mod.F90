module datm_datamode_gfs_hafs_mod

  use ESMF             , only : ESMF_State, ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_sys_mod      , only : shr_sys_abort
  use shr_precip_mod   , only : shr_precip_partition_rain_snow_ramp
  use shr_const_mod    , only : shr_const_tkfrz, shr_const_rhofw, shr_const_rdair
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: datm_datamode_gfs_hafs_advertise
  public  :: datm_datamode_gfs_hafs_init_pointers
  public  :: datm_datamode_gfs_hafs_advance
  public  :: datm_datamode_gfs_hafs_restart_write
  public  :: datm_datamode_gfs_hafs_restart_read

  ! export state data
  real(r8), pointer :: Sd_pslv(:)           => null()
  real(r8), pointer :: Sd_u10m(:)           => null()
  real(r8), pointer :: Sd_v10m(:)           => null()
  real(r8), pointer :: Faxd_swvdr(:)        => null()
  real(r8), pointer :: Faxd_swvdf(:)        => null()
  real(r8), pointer :: Faxd_swndr(:)        => null()
  real(r8), pointer :: Faxd_swndf(:)        => null()
  real(r8), pointer :: Faxd_lwnet(:)        => null()
  real(r8), pointer :: Faxd_taux(:)         => null()
  real(r8), pointer :: Faxd_tauy(:)         => null()
  real(r8), pointer :: Faxd_sen(:)          => null()
  real(r8), pointer :: Faxd_lat(:)          => null()
  real(r8), pointer :: Faxd_rain(:)         => null()

  ! stream data
  real(r8), pointer :: strm_rain(:)         => null()

  real(r8) :: rain_min ! rain value detector

  real(r8) , parameter :: tKFrz    = SHR_CONST_TKFRZ
  real(r8) , parameter :: rdair    = SHR_CONST_RDAIR ! dry air gas constant ~ J/K/kg
  real(r8) , parameter :: rhofw    = SHR_CONST_RHOFW ! density of fresh water ~ kg/m^3
  
  character(*), parameter :: nullstr = 'undefined'
  character(*), parameter :: rpfile  = 'rpointer.atm'
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_gfs_hafs_advertise(exportState, fldsexport, &
       flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sd_pslv'    )
    call dshr_fldList_add(fldsExport, 'Sd_u10m'    )
    call dshr_fldList_add(fldsExport, 'Sd_v10m'    )
    call dshr_fldList_add(fldsExport, 'Faxd_swvdr'  )
    call dshr_fldList_add(fldsExport, 'Faxd_swvdf'  )
    call dshr_fldList_add(fldsExport, 'Faxd_swndr' )
    call dshr_fldList_add(fldsExport, 'Faxd_swndf' )
    call dshr_fldList_add(fldsExport, 'Faxd_lwnet'  )
    call dshr_fldList_add(fldsExport, 'Faxd_taux'   )
    call dshr_fldList_add(fldsExport, 'Faxd_tauy'   )
    call dshr_fldList_add(fldsExport, 'Faxd_sen'    )
    call dshr_fldList_add(fldsExport, 'Faxd_lat'    )
    call dshr_fldList_add(fldsExport, 'Faxd_rain'   )

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm '//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_gfs_hafs_advertise

  !===============================================================================
  subroutine datm_datamode_gfs_hafs_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers for module level stream arrays
    !call shr_strdata_get_stream_pointer( sdat, 'Sa_mask'   , strm_mask , rc)
    call shr_strdata_get_stream_pointer( sdat, 'Faxd_rain'   , strm_rain , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! get export state pointers
    call dshr_state_getfldptr(exportState, 'Sd_pslv'    , fldptr1=Sd_pslv    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sd_u10m'    , fldptr1=Sd_u10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sd_v10m'    , fldptr1=Sd_v10m    , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_swvdr' , fldptr1=Faxd_swvdr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_swvdf' , fldptr1=Faxd_swvdf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_swndr' , fldptr1=Faxd_swndr , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_swndf' , fldptr1=Faxd_swndf , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_lwnet' , fldptr1=Faxd_lwnet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_taux'  , fldptr1=Faxd_taux  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_tauy'  , fldptr1=Faxd_tauy  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_sen'  , fldptr1=Faxd_sen  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_lat'  , fldptr1=Faxd_lat  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxd_rain'  , fldptr1=Faxd_rain  , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine datm_datamode_gfs_hafs_init_pointers

  !===============================================================================
  subroutine datm_datamode_gfs_hafs_advance(exportstate, mainproc, logunit, mpicom, target_ymd, target_tod, model_calendar, rc)
  use ESMF, only: ESMF_VMGetCurrent, ESMF_VMAllReduce, ESMF_REDUCE_MIN, ESMF_VM

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: mpicom
    integer                , intent(in)    :: target_ymd
    integer                , intent(in)    :: target_tod
    character(len=*)       , intent(in)    :: model_calendar
    integer                , intent(out)   :: rc

    ! local variables
    logical  :: first_time = .true.
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    real(r8) :: rtmp(2)
    type(ESMF_VM) :: vm
    character(len=*), parameter :: subname='(datm_datamode_gfs_hafs_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(strm_rain)

    if (first_time) then
       call ESMF_VMGetCurrent(vm, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! determine minimum Faxd_rain (see below for use)
       rtmp(1) = minval(strm_rain(:))
       call ESMF_VMAllReduce(vm, rtmp, rtmp(2:), 1, ESMF_REDUCE_MIN, rc=rc)
       rain_min = rtmp(2)
       if (mainproc) write(logunit,*) trim(subname),' rain_min = ',rain_min

       ! reset first_time
       first_time = .false.
    end if

    do n = 1, lsize
       !--- Faxd_rain is positive ---
       if (associated(Faxd_rain)) then
         if (rain_min < 0.0_r8) Faxd_rain(n) = max(0.0_r8,Faxd_rain(n))
       end if
    end do

  end subroutine datm_datamode_gfs_hafs_advance

  !===============================================================================
  subroutine datm_datamode_gfs_hafs_restart_write(case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat)
    
    ! input/output variables
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_write(rpfile, case_name, 'datm', inst_suffix, ymd, tod, &
         logunit, my_task, sdat)

  end subroutine datm_datamode_gfs_hafs_restart_write

  !===============================================================================
  subroutine datm_datamode_gfs_hafs_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine datm_datamode_gfs_hafs_restart_read

end module datm_datamode_gfs_hafs_mod
