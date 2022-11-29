*&---------------------------------------------------------------------*
*& Report zbc_tr_check
*&---------------------------------------------------------------------*
*& Transport List
*&---------------------------------------------------------------------*
*& Purpose
*&  Support release management monitoring Transport Request status
*&  in target systems.
*&
*& Features
*&  - List transports after certain release date
*&  - Shows release date and import Return Codes(RC) in target systems
*&  - Shows which request contains OSS Notes
*&  - Double-click on the line shows transport request
*& Setup
*&  - Set your system IDs in constants in gc_sysid_dev/qua/prd
*&  - In case You have Pre-Production system set gc_sysid_prpr as well
*&
*&  Enjoy (;
*&---------------------------------------------------------------------*
REPORT zbc_tr_check.
" For older systems type-pools needs explicitly mentioned.
"TYPE-POOLS: ctslg, slis, sctsc, abap.

*&---------------------------------------------------------------------*
*& GLOBAL DECLARATION
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_trcheckui,
         trkorr         TYPE e070-trkorr,
         as4text        TYPE e07t-as4text,
         trfunction     TYPE e070-trfunction,
         note           TYPE boole_d,
         trstatus       TYPE e070-trstatus,
         tarsystem      TYPE e070-tarsystem,
         korrdev        TYPE e070-korrdev,
         as4user        TYPE e070-as4user,
         as4date        TYPE e070-as4date,
         as4time        TYPE e070-as4time,
         strkorr        TYPE e070-strkorr,
         is_develop     TYPE boole_d,
         retcode_d      TYPE trretcode,
         export_date    TYPE dats,
         export_time    TYPE tims,
         is_quality     TYPE boole_d,
         retcode_q      TYPE trretcode,
         import_date_q  TYPE dats,
         import_time_q  TYPE tims,
         is_preproduct  TYPE boole_d,
         retcode_q1     TYPE trretcode,
         import_date_q1 TYPE dats,
         import_time_q1 TYPE tims,
         retcode_p      TYPE trretcode,
         is_production  TYPE boole_d,
         import_date_p  TYPE dats,
         import_time_p  TYPE tims,
         cell_colors    TYPE lvc_t_scol,
         deleted        TYPE flag,
       END OF ty_trcheckui.
TYPES tty_trcheckui TYPE TABLE OF ty_trcheckui WITH EMPTY KEY.

*CONSTANTS:
*  gc_sysid_dev  TYPE trtarsys VALUE 'BMD',
*  gc_sysid_qua  TYPE trtarsys VALUE 'BMQ',
*  gc_sysid_prpr TYPE trtarsys VALUE '',   "PreProduction
*  gc_sysid_prd  TYPE trtarsys VALUE 'BMP'.

DATA:
  gt_request          TYPE tty_trcheckui,
  gs_request_info     TYPE trwbo_request,
  gs_trattrib         TYPE e070a,
  gs_cofile           TYPE ctslg_cofile,
  gv_project          TYPE trkorr,
  gs_system           TYPE ctslg_system,
  gs_import_step      TYPE ctslg_step,
  gs_import_date_time TYPE ctslg_action,
  gt_event_exit       TYPE slis_t_event_exit,
  gs_event_exit       TYPE slis_event_exit,
  gs_truser           TYPE trdyse01cm,
  gt_fieldcat         TYPE slis_t_fieldcat_alv,
  gs_e070             TYPE e070,
  gv_trkorr           TYPE e070-trkorr,
  gt_trfunc_sel       TYPE RANGE OF trfunction,
  gs_trfunc_sel       LIKE LINE OF gt_trfunc_sel,
  gt_note_request     TYPE SORTED TABLE OF trkorr WITH UNIQUE KEY table_line.

DATA:
  gv_sysid_dev  TYPE trtarsys VALUE 'DEV',
  gv_sysid_qua  TYPE trtarsys VALUE 'QAS',
  gv_sysid_prpr TYPE trtarsys VALUE '',   "PreProduction
  gv_sysid_prd  TYPE trtarsys VALUE 'PRD'.

DATA:
  gv_version TYPE tcevers-version.

FIELD-SYMBOLS:
  <gs_request> TYPE ty_trcheckui,
  <gs_fcat>    TYPE slis_fieldcat_alv.

CLASS:
  lcl_main DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE tblk1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) tx_expdt FOR FIELD p_expdat.
    PARAMETERS p_expdat TYPE as4date. "OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (28) tx_trkor FOR FIELD s_trkorr.
    SELECT-OPTIONS s_trkorr  FOR gv_trkorr.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (28) tx_user FOR FIELD s_user.
    SELECT-OPTIONS s_user    FOR gs_truser-username DEFAULT sy-uname.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (28) tx_trfun FOR FIELD s_trfunc.
    SELECT-OPTIONS s_trfunc  FOR gs_e070-trfunction DEFAULT 'K'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE tblk2.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_modify AS CHECKBOX DEFAULT abap_true.
    SELECTION-SCREEN COMMENT (30) tx_modif FOR FIELD p_modify.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_releas AS CHECKBOX DEFAULT abap_true.
    SELECTION-SCREEN COMMENT (30) tx_relea FOR FIELD p_releas.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_trkorr-low.
  CALL FUNCTION 'TR_F4_REQUESTS'
    IMPORTING
      ev_selected_request = s_trkorr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_trkorr-high.
  CALL FUNCTION 'TR_F4_REQUESTS'
    IMPORTING
      ev_selected_request = s_trkorr-high.

*&---------------------------------------------------------------------*
*& Include lcl_main DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    "! Get data of List of Transport Requests
    CLASS-METHODS get_data.
    CLASS-METHODS refresh_data.

    "! Display List of Transport Requests as ALV
    CLASS-METHODS display_tr_list.
  PRIVATE SECTION.
    "! Prepare ALV List Configuration
    CLASS-METHODS _prepare_alv.
    "! Prepare ALV Field Catalog
    CLASS-METHODS _prepare_field_catalog.
    "! Setup D-Q-P Status column colors
    CLASS-METHODS _prepare_column_colors.
    "! Setup D-Q-P Status column colors
    CLASS-METHODS _prepare_callback_functions.
    "! Show ALV List
    CLASS-METHODS _display_alv.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Include lcl_main IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD get_data.
    CONCATENATE sy-sysid '%' INTO gv_trkorr.
    CONDENSE gv_trkorr NO-GAPS.

    SELECT * FROM e070 INTO CORRESPONDING FIELDS OF TABLE gt_request
      WHERE trkorr     LIKE gv_trkorr
        AND trkorr     IN s_trkorr
        AND trfunction IN s_trfunc
        AND as4user    IN s_user.

    LOOP AT gt_request ASSIGNING <gs_request>.
      CLEAR gs_request_info.

      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          iv_read_e07t       = 'X'
          iv_read_e070c      = 'X'
          iv_read_e070m      = 'X'
          iv_read_attributes = 'X'
          iv_trkorr          = <gs_request>-trkorr
        CHANGING
          cs_request         = gs_request_info
        EXCEPTIONS
          error_occured      = 1
          no_authorization   = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
      ENDIF.

      IF  <gs_request>-trfunction NA sctsc_types_projects.
        CALL FUNCTION 'TR_READ_ATTRIBUTES'
          EXPORTING
            iv_request    = <gs_request>-trkorr
          IMPORTING
            et_attributes = gs_request_info-attributes
          EXCEPTIONS
            OTHERS        = 1.
      ENDIF.


      READ TABLE gs_request_info-attributes INTO gs_trattrib
        WITH KEY attribute = 'EXPORT_TIMESTAMP'.

      <gs_request>-as4text    = gs_request_info-h-as4text.
      <gs_request>-is_develop = abap_true.

      IF sy-subrc EQ 0.
        <gs_request>-export_date = gs_trattrib-reference(8).
        <gs_request>-export_time = gs_trattrib-reference+8(8).
      ENDIF.

      IF <gs_request>-export_date LT p_expdat.
*     AND <gs_request>-export_date IS NOT INITIAL.
*        DELETE gt_request.
        <gs_request>-deleted = abap_true.
        CONTINUE.
      ENDIF.

      CLEAR gs_cofile.
      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr  = <gs_request>-trkorr
        IMPORTING
          es_cofile  = gs_cofile
          ev_project = gv_project.

      LOOP AT gs_cofile-systems INTO gs_system.
        CASE gs_system-systemid.

          WHEN gv_sysid_dev.
            <gs_request>-retcode_d  = gs_system-rc.

          WHEN gv_sysid_qua.
            READ TABLE gs_system-steps INTO gs_import_step WITH KEY stepid = 'I'."Import
            IF sy-subrc EQ 0.
              <gs_request>-retcode_q = gs_system-rc.
              READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-import_date_q = gs_import_date_time-date.
                <gs_request>-import_time_q = gs_import_date_time-time.
                <gs_request>-is_quality    = abap_true.
              ENDIF.
            ELSE.
              READ TABLE gs_system-steps INTO gs_import_step INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-retcode_q = gs_system-rc.
                READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
                IF sy-subrc EQ 0.
                  <gs_request>-import_date_q = gs_import_date_time-date.
                  <gs_request>-import_time_q = gs_import_date_time-time.
                  <gs_request>-is_quality    = abap_true.
                ENDIF.
              ENDIF.
            ENDIF.

          WHEN gv_sysid_prpr.
            READ TABLE gs_system-steps INTO gs_import_step WITH KEY stepid = 'I'."Import
            IF sy-subrc EQ 0.
              <gs_request>-retcode_q1 = gs_system-rc.
              READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-import_date_q1 = gs_import_date_time-date.
                <gs_request>-import_time_q1 = gs_import_date_time-time.
                <gs_request>-is_preproduct  = abap_true.
              ENDIF.
            ELSE.
              READ TABLE gs_system-steps INTO gs_import_step INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-retcode_q1 = gs_system-rc.
                READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
                IF sy-subrc EQ 0.
                  <gs_request>-import_date_q1 = gs_import_date_time-date.
                  <gs_request>-import_time_q1 = gs_import_date_time-time.
                  <gs_request>-is_preproduct  = abap_true.
                ENDIF.
              ENDIF.
            ENDIF.

          WHEN gv_sysid_prd.
            READ TABLE gs_system-steps INTO gs_import_step WITH KEY stepid = 'I'."Import
            IF sy-subrc EQ 0.
              <gs_request>-retcode_p = gs_system-rc.
              READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-import_date_p = gs_import_date_time-date.
                <gs_request>-import_time_p = gs_import_date_time-time.
                <gs_request>-is_production = abap_true.
              ENDIF.
            ELSE.
              READ TABLE gs_system-steps INTO gs_import_step INDEX 1.
              IF sy-subrc EQ 0.
                <gs_request>-retcode_p = gs_system-rc.
                READ TABLE gs_import_step-actions INTO gs_import_date_time INDEX 1.
                IF sy-subrc EQ 0.
                  <gs_request>-import_date_p = gs_import_date_time-date.
                  <gs_request>-import_time_p = gs_import_date_time-time.
                  <gs_request>-is_production = abap_true.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    DELETE gt_request WHERE deleted IS NOT INITIAL.

    IF p_modify IS NOT INITIAL.
      DATA(lt_req_mod) = gt_request.
      DELETE lt_req_mod WHERE export_date IS NOT INITIAL.
    ENDIF.

    IF p_releas IS NOT INITIAL.
      DATA(lt_req_rel) = gt_request.
      DELETE lt_req_rel WHERE export_date IS INITIAL.
    ENDIF.

    CLEAR gt_request.

    IF lt_req_mod[] IS NOT INITIAL.
      APPEND LINES OF lt_req_mod TO gt_request.
    ENDIF.
    IF lt_req_rel[] IS NOT INITIAL.
      APPEND LINES OF lt_req_rel TO gt_request.
    ENDIF.

    "Does the request contain Notes or Correcton Instructions ?
    IF gt_request IS NOT INITIAL.
      SELECT DISTINCT trkorr FROM e071 INTO TABLE gt_note_request
        FOR ALL ENTRIES IN gt_request
          WHERE trkorr EQ gt_request-trkorr
            AND object IN ( 'NOTE', 'CORR' ).

      LOOP AT gt_request ASSIGNING <gs_request>.
        IF line_exists( gt_note_request[ table_line = <gs_request>-trkorr ] ).
          <gs_request>-note = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.

*    SORT gt_request BY export_date export_time trkorr.
    SORT gt_request BY trkorr.
  ENDMETHOD.

  METHOD refresh_data.
    get_data( ).

    IF gt_request[] IS NOT INITIAL.
      _prepare_column_colors( ).
    ENDIF.
  ENDMETHOD.

  METHOD display_tr_list.
    _prepare_alv( ).
    _display_alv( ).
  ENDMETHOD.

  METHOD _prepare_alv.
    _prepare_callback_functions( ).
    _prepare_field_catalog( ).
    _prepare_column_colors( ).
  ENDMETHOD.

  METHOD _prepare_field_catalog.
    DATA: table TYPE REF TO data.
    CREATE DATA table LIKE gt_request.
    ASSIGN table->* TO FIELD-SYMBOL(<table>).
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
                                CHANGING  t_table      = <table>  ).
        DATA(lt_fcat_lvc) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                  r_columns      = salv_table->get_columns( )         " ALV Filter
                                  r_aggregations = salv_table->get_aggregations( ) ). " ALV Aggregations
        CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
          EXPORTING
            it_fieldcat_lvc         = lt_fcat_lvc
          IMPORTING
            et_fieldcat_alv         = gt_fieldcat
          TABLES
            it_data                 = gt_request
          EXCEPTIONS
            it_data_missing         = 1
            it_fieldcat_lvc_missing = 2
            OTHERS                  = 3.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_root.
    ENDTRY.

    LOOP AT gt_fieldcat ASSIGNING <gs_fcat>.
      CASE <gs_fcat>-fieldname.
        WHEN 'TRKORR'.
*          <gs_fcat>-fix_column = abap_true.
          <gs_fcat>-key        = abap_true.
          <gs_fcat>-hotspot    = abap_true.
        WHEN 'TRFUNCTION' OR 'TRSTATUS'.
          <gs_fcat>-outputlen = 3.
        WHEN 'TARSYSTEM'.
          <gs_fcat>-ref_fieldname = 'SYSNAME'.
          <gs_fcat>-ref_tabname   = 'TCESYST'.
        WHEN 'NOTE'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = 'Note'.
        WHEN 'STRKORR' OR 'KORRDEV'.
          <gs_fcat>-no_out = 'X'.
        WHEN 'AS4USER' OR 'AS4DATE' OR 'AS4TIME'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = 'Last change'.
        WHEN 'EXPORT_DATE' OR 'EXPORT_TIME'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_dev }-Release|.
          <gs_fcat>-no_zero = abap_true.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'IMPORT_DATE_Q' OR 'IMPORT_TIME_Q'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_qua }-Import|.
          <gs_fcat>-no_zero = abap_true.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'IMPORT_DATE_P' OR 'IMPORT_TIME_P'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prd }-Import|.
          <gs_fcat>-no_zero = abap_true.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'RETCODE_D'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_dev }-RC|.
          <gs_fcat>-outputlen = 7.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'RETCODE_Q'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_qua }-RC|.
          <gs_fcat>-outputlen = 7.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'RETCODE_P'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prd }-RC|.
          <gs_fcat>-outputlen = 7.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'IMPORT_DATE_Q1' OR 'IMPORT_TIME_Q1'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prpr }-Import|.
          <gs_fcat>-no_zero = abap_true.
          <gs_fcat>-hotspot = abap_true.

          IF gv_sysid_prpr IS INITIAL.
            <gs_fcat>-tech = 'X'.
          ENDIF.

        WHEN 'RETCODE_Q1'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prpr }-RC|.
          <gs_fcat>-outputlen = 7.
          <gs_fcat>-hotspot = abap_true.

          IF gv_sysid_prpr IS INITIAL.
            <gs_fcat>-tech = 'X'.
          ENDIF.

        WHEN 'IS_DEVELOP'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_dev }-System|.
          <gs_fcat>-no_out    = abap_true.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'IS_QUALITY'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_qua }-System|.
          <gs_fcat>-no_out    = abap_true.
          <gs_fcat>-hotspot = abap_true.

        WHEN 'IS_PREPRODUCT'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prpr }-System|.
          <gs_fcat>-no_out    = abap_true.
          <gs_fcat>-hotspot = abap_true.

          IF gv_sysid_prpr IS INITIAL.
            <gs_fcat>-tech = 'X'.
          ENDIF.

        WHEN 'IS_PRODUCTION'.
          <gs_fcat>-reptext_ddic =
          <gs_fcat>-seltext_s =
          <gs_fcat>-seltext_m =
          <gs_fcat>-seltext_l = |{ gv_sysid_prd }-System|.
          <gs_fcat>-no_out    = abap_true.
          <gs_fcat>-hotspot   = abap_true.

        WHEN 'DELETED'.
          <gs_fcat>-tech = 'X'.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD _prepare_column_colors.

    "Column(Cell) Colors
    DATA(gt_colors) = VALUE lvc_t_scol(
      ( fname   = 'IS_DEVELOP'
        color-col = 5
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'RETCODE_D'
        color-col = 5
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'EXPORT_DATE'
        color-col = 5
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'EXPORT_TIME'
        color-col = 5
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IS_QUALITY'
        color-col = 3
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'RETCODE_Q'
        color-col = 3
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_DATE_Q'
        color-col = 3
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_TIME_Q'
        color-col = 3
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IS_PREPRODUCT'
        color-col = 7
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'RETCODE_Q1'
        color-col = 7
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_DATE_Q1'
        color-col = 7
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_TIME_Q1'
        color-col = 7
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IS_PRODUCTION'
        color-col = 6
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'RETCODE_P'
        color-col = 6
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_DATE_P'
        color-col = 6
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
      ( fname   = 'IMPORT_TIME_P'
        color-col = 6
        color-int = 0
        color-inv = 0
        nokeycol  = abap_true )
    ).

    LOOP AT gt_request ASSIGNING <gs_request>.
      <gs_request>-cell_colors = gt_colors.
    ENDLOOP.
  ENDMETHOD.

  METHOD _prepare_callback_functions.
    gs_event_exit-ucomm = '&IC1'.        "(Double-click)
    gs_event_exit-after = abap_true.
    APPEND gs_event_exit TO gt_event_exit.

    gs_event_exit-ucomm = '&REFRESH'.        "(Refresh)
    gs_event_exit-after = abap_true.
    APPEND gs_event_exit TO gt_event_exit.
  ENDMETHOD.

  METHOD _display_alv.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'DCLICK'
        it_fieldcat              = gt_fieldcat
        it_event_exit            = gt_event_exit
        i_default                = abap_true
        i_save                   = abap_true
        is_layout                = VALUE slis_layout_alv(  no_input = abap_true
                                                           colwidth_optimize = abap_true
                                                           coltab_fieldname = 'CELL_COLORS' )
      TABLES
        t_outtab                 = gt_request.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  tblk1 = 'Selection criteria'.
  tblk2 = 'Request Status'.
  tx_expdt = 'Released since'.
  tx_trkor = 'Request/Task'.
  tx_user  = 'Owner'.
  tx_trfun = 'Type'.
  tx_modif = 'Modifiable'.
  tx_relea = 'Released'.

  gs_trfunc_sel-sign = 'I'.
  gs_trfunc_sel-option = 'EQ'.
  gs_trfunc_sel-low = 'W'.
  APPEND gs_trfunc_sel TO s_trfunc.

* fill lt_tcesystt with all active texts in the current language
  CALL FUNCTION 'TR_GET_CONFIG_VERSION'
    IMPORTING
      ev_active_version       = gv_version
    EXCEPTIONS
      no_active_version_found = 1.
  IF sy-subrc EQ 0.
    SELECT * FROM tcesyst WHERE version = @gv_version
      INTO TABLE @DATA(lt_tcesystt).
    IF lt_tcesystt[] IS NOT INITIAL.
      LOOP AT lt_tcesystt INTO DATA(ls_tcesystt).
        IF ls_tcesystt-sysname CA 'P'.      "Production
          gv_sysid_prd = ls_tcesystt-sysname.
        ELSEIF ls_tcesystt-sysname CA 'Q'.  "Quality
          gv_sysid_qua = ls_tcesystt-sysname.
        ELSEIF ls_tcesystt-sysname CA 'D'.  "Development
          gv_sysid_dev = ls_tcesystt-sysname.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_modify IS INITIAL
 AND p_releas IS INITIAL.
    MESSAGE s655(tk) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  lcl_main=>get_data( ).

* Display Data
  lcl_main=>display_tr_list( ).

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  DATA(lt_extab) = rt_extab.
  DELETE lt_extab WHERE fcode EQ '&REFRESH'.

  SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL' EXCLUDING lt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_UCOMM     text
*      -->CS_SELFIELD  text
*----------------------------------------------------------------------*
FORM dclick USING iv_ucomm    TYPE syucomm
                  cs_selfield TYPE slis_selfield.

  DATA:
    ls_request_ui  TYPE ty_trcheckui.

  CASE iv_ucomm.
    WHEN '&IC1'.
      READ TABLE gt_request INTO ls_request_ui INDEX cs_selfield-tabindex.
      CHECK sy-subrc EQ 0.

      CASE cs_selfield-fieldname.
        WHEN 'RETCODE_D'
          OR 'EXPORT_DATE'
          OR 'EXPORT_TIME'
          OR 'RETCODE_Q'
          OR 'IMPORT_DATE_Q'
          OR 'IMPORT_TIME_Q'
          OR 'RETCODE_Q1'
          OR 'IMPORT_DATE_Q1'
          OR 'IMPORT_TIME_Q1'
          OR 'RETCODE_P'
          OR 'IMPORT_DATE_P'
          OR 'IMPORT_TIME_P'
          OR 'IS_DEVELOP'
          OR 'IS_QUALITY'
          OR 'IS_PREPRODUCT'
          OR 'IS_PRODUCTION'.
          CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
            EXPORTING
              iv_trkorr = ls_request_ui-trkorr.
        WHEN 'TRKORR'.
          PERFORM display_single_request IN PROGRAM rddm0001 IF FOUND
            USING ls_request_ui-trkorr.
      ENDCASE.
    WHEN '&REFRESH'.
      lcl_main=>refresh_data( ).

      cs_selfield-row_stable = cs_selfield-col_stable = abap_true.
      cs_selfield-refresh = 'S'.
  ENDCASE.
ENDFORM.                    " L01_LOAD_USER_COMMAND
