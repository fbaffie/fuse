! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Martyn Clark
! Modified by Brian Henn to include snow model, 6/2013
! ---------------------------------------------------------------------------------------
MODULE PAR_INSERT_MODULE
IMPLICIT NONE
CONTAINS
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
SUBROUTINE PUT_PARSET(PARSET)
! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Martyn Clark, 2008
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
! Inserts an entire parameter set into a data structure, using the list of parameters
! in LPARAM
! ---------------------------------------------------------------------------------------
USE nrtype                                            ! variable types, etc.
USE multiparam                                        ! model parameters
IMPLICIT NONE
! input
REAL(SP), INTENT(IN), DIMENSION(:)     :: PARSET      ! parameter set
! local
INTEGER(I4B)                           :: IPAR        ! looping
! ---------------------------------------------------------------------------------------
DO IPAR=1,NUMPAR  ! NUMPAR is stored in module multiparam
 CALL PAR_INSERT(PARSET(IPAR),LPARAM(IPAR)%PARNAME)
END DO
! ---------------------------------------------------------------------------------------
END SUBROUTINE PUT_PARSET
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
SUBROUTINE PAR_INSERT(XVAR,PARNAME)
! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Martyn Clark, 2007
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
! Inserts parameter value into data structurs
! ---------------------------------------------------------------------------------------
USE nrtype                                            ! variable types, etc.
USE multiparam                                        ! model parameters
IMPLICIT NONE
! input
REAL(SP), INTENT(IN)                   :: XVAR        ! parameter value
CHARACTER(*), INTENT(IN)               :: PARNAME     ! parameter name
! ---------------------------------------------------------------------------------------
! model parameters
SELECTCASE(TRIM(PARNAME))
CASE('RFERR_ADD');  MPARAM%RFERR_ADD  = XVAR
CASE('RFERR_MLT');  MPARAM%RFERR_MLT  = XVAR
CASE('RFH1_MEAN');  MPARAM%RFH1_MEAN  = XVAR
CASE('RFH2_SDEV');  MPARAM%RFH2_SDEV  = XVAR
CASE('RH1P_MEAN');  MPARAM%RH1P_MEAN  = XVAR
CASE('RH1P_SDEV');  MPARAM%RH1P_SDEV  = XVAR
CASE('RH2P_MEAN');  MPARAM%RH2P_MEAN  = XVAR
CASE('RH2P_SDEV');  MPARAM%RH2P_SDEV  = XVAR
CASE('MAXWATR_1');  MPARAM%MAXWATR_1  = XVAR
CASE('MAXWATR_2');  MPARAM%MAXWATR_2  = XVAR
CASE('FRACTEN');    MPARAM%FRACTEN    = XVAR
CASE('FRCHZNE');    MPARAM%FRCHZNE    = XVAR
CASE('FPRIMQB');    MPARAM%FPRIMQB    = XVAR
CASE('RTFRAC1');    MPARAM%RTFRAC1    = XVAR
CASE('PERCRTE');    MPARAM%PERCRTE    = XVAR
CASE('PERCEXP');    MPARAM%PERCEXP    = XVAR
CASE('SACPMLT');    MPARAM%SACPMLT    = XVAR
CASE('SACPEXP');    MPARAM%SACPEXP    = XVAR
CASE('PERCFRAC');   MPARAM%PERCFRAC   = XVAR
CASE('FRACLOWZ');   MPARAM%FRACLOWZ   = XVAR
CASE('IFLWRTE');    MPARAM%IFLWRTE    = XVAR
CASE('BASERTE');    MPARAM%BASERTE    = XVAR
CASE('QB_POWR');    MPARAM%QB_POWR    = XVAR
CASE('QB_PRMS');    MPARAM%QB_PRMS    = XVAR
CASE('QBRATE_2A');  MPARAM%QBRATE_2A  = XVAR
CASE('QBRATE_2B');  MPARAM%QBRATE_2B  = XVAR
CASE('SAREAMAX');   MPARAM%SAREAMAX   = XVAR
CASE('AXV_BEXP');   MPARAM%AXV_BEXP   = XVAR
CASE('LOGLAMB');    MPARAM%LOGLAMB    = XVAR
CASE('TISHAPE');    MPARAM%TISHAPE    = XVAR
CASE('TIMEDELAY');  MPARAM%TIMEDELAY  = XVAR
CASE('MBASE');      MPARAM%MBASE      = XVAR
CASE('MFMAX');      MPARAM%MFMAX      = XVAR
CASE('MFMIN');      MPARAM%MFMIN      = XVAR
CASE('PXTEMP');     MPARAM%PXTEMP     = XVAR
CASE('OPG');        MPARAM%OPG        = XVAR
CASE('LAPSE');      MPARAM%LAPSE      = XVAR
! derived parameters
CASE('MAXTENS_1');  DPARAM%MAXTENS_1  = XVAR
CASE('MAXTENS_1A'); DPARAM%MAXTENS_1A = XVAR
CASE('MAXTENS_1B'); DPARAM%MAXTENS_1B = XVAR
CASE('MAXFREE_1');  DPARAM%MAXFREE_1  = XVAR
CASE('MAXTENS_2');  DPARAM%MAXTENS_2  = XVAR
CASE('MAXFREE_2');  DPARAM%MAXFREE_2  = XVAR
CASE('MAXFREE_2A'); DPARAM%MAXFREE_2A = XVAR
CASE('MAXFREE_2B'); DPARAM%MAXFREE_2B = XVAR
CASE('QBSAT');      DPARAM%QBSAT      = XVAR
CASE('RTFRAC2');    DPARAM%RTFRAC2    = XVAR
CASE('POWLAMB');    DPARAM%POWLAMB    = XVAR
CASE('MAXPOW');     DPARAM%MAXPOW     = XVAR
CASE DEFAULT; STOP ' parameter name does not exist '
ENDSELECT
! ---------------------------------------------------------------------------------------
END SUBROUTINE PAR_INSERT

SUBROUTINE PAR_INSERT_2D(XVAR,PARNAME)
! ---------------------------------------------------------------------------------------
! Creator:
! --------
! Nans Addor, 2021
! ---------------------------------------------------------------------------------------
! Purpose:
! --------
! Inserts parameter value into data 2D structurs
! ---------------------------------------------------------------------------------------
USE nrtype                                            ! variable types, etc.
USE multiparam                                        ! model parameters
IMPLICIT NONE
! input
REAL(SP), INTENT(IN)                   :: XVAR(:, :)  ! parameter value
CHARACTER(*), INTENT(IN)               :: PARNAME     ! parameter name
! ---------------------------------------------------------------------------------------
! model parameters
SELECTCASE(TRIM(PARNAME))
CASE('RFERR_ADD');  MPARAM_2D%RFERR_ADD  = XVAR
CASE('RFERR_MLT');  MPARAM_2D%RFERR_MLT  = XVAR
CASE('RFH1_MEAN');  MPARAM_2D%RFH1_MEAN  = XVAR
CASE('RFH2_SDEV');  MPARAM_2D%RFH2_SDEV  = XVAR
CASE('RH1P_MEAN');  MPARAM_2D%RH1P_MEAN  = XVAR
CASE('RH1P_SDEV');  MPARAM_2D%RH1P_SDEV  = XVAR
CASE('RH2P_MEAN');  MPARAM_2D%RH2P_MEAN  = XVAR
CASE('RH2P_SDEV');  MPARAM_2D%RH2P_SDEV  = XVAR
CASE('MAXWATR_1');  MPARAM_2D%MAXWATR_1  = XVAR
CASE('MAXWATR_2');  MPARAM_2D%MAXWATR_2  = XVAR
CASE('FRACTEN');    MPARAM_2D%FRACTEN    = XVAR
CASE('FRCHZNE');    MPARAM_2D%FRCHZNE    = XVAR
CASE('FPRIMQB');    MPARAM_2D%FPRIMQB    = XVAR
CASE('RTFRAC1');    MPARAM_2D%RTFRAC1    = XVAR
CASE('PERCRTE');    MPARAM_2D%PERCRTE    = XVAR
CASE('PERCEXP');    MPARAM_2D%PERCEXP    = XVAR
CASE('SACPMLT');    MPARAM_2D%SACPMLT    = XVAR
CASE('SACPEXP');    MPARAM_2D%SACPEXP    = XVAR
CASE('PERCFRAC');   MPARAM_2D%PERCFRAC   = XVAR
CASE('FRACLOWZ');   MPARAM_2D%FRACLOWZ   = XVAR
CASE('IFLWRTE');    MPARAM_2D%IFLWRTE    = XVAR
CASE('BASERTE');    MPARAM_2D%BASERTE    = XVAR
CASE('QB_POWR');    MPARAM_2D%QB_POWR    = XVAR
CASE('QB_PRMS');    MPARAM_2D%QB_PRMS    = XVAR
CASE('QBRATE_2A');  MPARAM_2D%QBRATE_2A  = XVAR
CASE('QBRATE_2B');  MPARAM_2D%QBRATE_2B  = XVAR
CASE('SAREAMAX');   MPARAM_2D%SAREAMAX   = XVAR
CASE('AXV_BEXP');   MPARAM_2D%AXV_BEXP   = XVAR
CASE('LOGLAMB');    MPARAM_2D%LOGLAMB    = XVAR
CASE('TISHAPE');    MPARAM_2D%TISHAPE    = XVAR
CASE('TIMEDELAY');  MPARAM_2D%TIMEDELAY  = XVAR
CASE('MBASE');      MPARAM_2D%MBASE      = XVAR
CASE('MFMAX');      MPARAM_2D%MFMAX      = XVAR
CASE('MFMIN');      MPARAM_2D%MFMIN      = XVAR
CASE('PXTEMP');     MPARAM_2D%PXTEMP     = XVAR
CASE('OPG');        MPARAM_2D%OPG        = XVAR
CASE('LAPSE');      MPARAM_2D%LAPSE      = XVAR
! derived parameters
CASE('MAXTENS_1');  DPARAM_2D%MAXTENS_1  = XVAR
CASE('MAXTENS_1A'); DPARAM_2D%MAXTENS_1A = XVAR
CASE('MAXTENS_1B'); DPARAM_2D%MAXTENS_1B = XVAR
CASE('MAXFREE_1');  DPARAM_2D%MAXFREE_1  = XVAR
CASE('MAXTENS_2');  DPARAM_2D%MAXTENS_2  = XVAR
CASE('MAXFREE_2');  DPARAM_2D%MAXFREE_2  = XVAR
CASE('MAXFREE_2A'); DPARAM_2D%MAXFREE_2A = XVAR
CASE('MAXFREE_2B'); DPARAM_2D%MAXFREE_2B = XVAR
CASE('QBSAT');      DPARAM_2D%QBSAT      = XVAR
CASE('RTFRAC2');    DPARAM_2D%RTFRAC2    = XVAR
CASE('POWLAMB');    DPARAM_2D%POWLAMB    = XVAR
CASE('MAXPOW');     DPARAM_2D%MAXPOW     = XVAR
CASE DEFAULT; STOP ' parameter name does not exist '
ENDSELECT
! ---------------------------------------------------------------------------------------
END SUBROUTINE PAR_INSERT_2D
END MODULE PAR_INSERT_MODULE
