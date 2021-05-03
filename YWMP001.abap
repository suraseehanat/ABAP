*&---------------------------------------------------------------------*
*& Report YWMP001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YWMP001.

TABLES: mcha, ausp, mch1 .

TYPES: BEGIN OF ty_output,
        chbox    TYPE char01,
        mat    TYPE mch1-matnr,
        batch  TYPE mch1-charg,
        weight TYPE tdline,       " ausp-atflv,
       END OF ty_output.

TYPES: BEGIN OF ty_mcha,
        werks    TYPE mcha-werks,
        charg    TYPE mcha-charg,
        matnr    TYPE mcha-matnr,
        cuobj_bm TYPE ausp-objek, "mch1-cuobj_bm,
       END OF ty_mcha.

TYPES: BEGIN OF ty_mch1,
        matnr    TYPE mch1-matnr,
        charg    TYPE mch1-charg,
        cuobj_bm TYPE mch1-cuobj_bm,
       END OF ty_mch1.

TYPES: BEGIN OF ty_ausp,
        atflv  TYPE ausp-atflv,
        objek  TYPE ausp-objek ,
        atinn  TYPE ausp-atinn,
        klart  TYPE ausp-klart,
       END OF ty_ausp.

TYPES: BEGIN OF ty_search,
        werks TYPE t001w-werks,
        name1 TYPE t001w-name1,
       END OF ty_search.

DATA: gt_output TYPE TABLE OF ty_output , gs_output  TYPE ty_output ,
      gt_mcha   TYPE TABLE OF ty_mcha   , gs_mcha    TYPE ty_mcha   ,
      gt_mch1   TYPE TABLE OF ty_mch1   , gs_mac1    TYPE ty_mch1   ,
      gt_ausp   TYPE TABLE OF ty_ausp   , gs_ausp    TYPE ty_ausp   ,

      gt_plant_re TYPE TABLE OF ddshretval ,
      gt_plant    TYPE TABLE OF ty_search , gs_plant TYPE ty_search ,
      gs_data   TYPE ty_output.
DATA: l_value   TYPE char16 ,
      p_id      TYPE ausp-objek .

" ----------------------- Data for ALV --------------------------------------
DATA : gs_fieldcat TYPE slis_t_fieldcat_alv,
       gs_layout   TYPE slis_layout_alv.

DATA: gt_fieldcat_lvc TYPE lvc_t_fcat,
      gs_layout_lvc   TYPE lvc_s_layo.

DATA: lt_alv TYPE STANDARD TABLE OF ty_output,
      ls_alv TYPE ty_output.
"----------------------------------------------------------------------------

" ----------------------- Smartforms ----------------------------------
DATA: lv_fname           TYPE rs38l_fnam,
      ls_control_param   TYPE ssfctrlop,
      ls_output_option   TYPE ssfcompop,
      ls_job_output_info TYPE ssfcrescl,
      ls_job_output_opt  TYPE ssfcresop.

CONSTANTS: gv_formname TYPE tdsfname VALUE 'YWMP001'.
" ---------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK sec_main.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) line1 FOR FIELD s_plant .
  SELECT-OPTIONS s_plant FOR mcha-werks NO INTERVALS NO-EXTENSION.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (10) line2 FOR FIELD s_batch.
  SELECT-OPTIONS s_batch FOR mcha-charg .
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sec_main.

INITIALIZATION.
  line1  = 'Plant'.
  line2  = 'Batch'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CS 's_batch-low' .
      screen-required = 2.
      MODIFY SCREEN.
    ELSEIF screen-name CS 's_plant-low'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_plant-low.
  PERFORM f4_help_for_plant USING 'S_PLANT'.

FORM f4_help_for_plant USING l_dynprofield.
  CLEAR gt_plant.
  SELECT werks name1
    FROM t001w
    INTO TABLE gt_plant
    WHERE werks = '1100'
       OR werks = '2100'.
  SORT gt_plant BY werks ASCENDING.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WERKS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_PLANT-WERKS'
      value_org   = 'S'
    TABLES
      value_tab   = gt_plant
      return_tab  = gt_plant_re.
ENDFORM.

START-OF-SELECTION.
  IF s_plant-low IS INITIAL OR s_batch-low IS INITIAL .
    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: gt_mcha, gt_ausp, gt_output .

  SELECT a~werks a~charg a~matnr
         b~cuobj_bm
    FROM mcha AS a
    INNER JOIN mch1 AS b ON a~matnr = b~matnr AND a~charg = b~charg
    INTO TABLE gt_mcha
    WHERE a~werks IN s_plant
      AND a~charg IN s_batch .

  IF gt_mcha IS INITIAL.
    MESSAGE 'Data not found' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT atflv objek atinn klart
    FROM ausp
    INTO CORRESPONDING FIELDS OF TABLE gt_ausp
    FOR ALL ENTRIES IN gt_mcha
    WHERE objek = gt_mcha-cuobj_bm
*      AND atinn = '0000000811'   " Test 400
      AND atinn = '0000000815'
      AND klart = '023' .

   IF gt_ausp IS INITIAL .
     MESSAGE 'Data not found' TYPE 'S' DISPLAY LIKE 'E'.
     EXIT.
   ENDIF.

LOOP AT gt_mcha INTO gs_mcha.
  CLEAR: gs_output.
  MOVE: gs_mcha-matnr  TO gs_output-mat,
        gs_mcha-charg  TO gs_output-batch.

  READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_mcha-cuobj_bm.
  IF sy-subrc = 0.
      IF gs_ausp-atinn = '0000000815'.
*      IF gs_ausp-atinn = '0000000811'.   " Test 400
        PERFORM chang_conversion USING gs_ausp-atflv CHANGING l_value.
        IF l_value IS NOT INITIAL.
          gs_output-weight =  l_value .
        ENDIF.
      ENDIF.
  ENDIF.

  IF gs_output-weight IS INITIAL.
    CLEAR: gs_output, gs_ausp, gs_mcha, l_value .
    CONTINUE.
  ENDIF.
  APPEND gs_output TO gt_output .
  CLEAR: gs_output, gs_ausp, gs_mcha, l_value .
ENDLOOP.

IF gt_output IS INITIAL .
  MESSAGE 'Data not found' TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

SORT gt_output ASCENDING BY mat batch.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
  PERFORM build_fieldcat.
  PERFORM build_layout.
  PERFORM call_alv_display.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat .
  CONSTANTS: lc_key(1)   TYPE c VALUE '1',
             lc_edit(1)  TYPE c VALUE '5',
             lc_chbox(1) TYPE c VALUE '6'.

  REFRESH: gt_fieldcat_lvc.

  PERFORM alv_fieldcat:
    USING  'CHBOX'   'CHAR1'   space     'Select'    lc_key lc_chbox,
    USING  'MAT'     'MCHA'   'MATNR'    'Material'  space  space,
    USING  'BATCH'   'MCHA'   'CHARG'    'Batch'     space  space,
    USING  'WEIGHT'  'MARA'   'NTGEW'    'น้ำหนักภาชนะ'         space  space.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM alv_fieldcat USING pv_name      TYPE lvc_fname " Field Name
                        pv_ref_field TYPE lvc_fname  " Ref field name
                        pv_ref_table TYPE lvc_tname  " Ref table name
                        pv_ltext     TYPE c          " Column Title
                        pv_type1     TYPE c          " Key, Q, C
                        pv_type2     TYPE c.         " Field Type
  DATA: ls_fieldcat_lvc TYPE lvc_s_fcat.

  ls_fieldcat_lvc-fieldname = pv_name.

  IF pv_ref_table IS INITIAL.
    IF NOT pv_ref_field IS INITIAL.
      ls_fieldcat_lvc-datatype = pv_ref_field(4).
      ls_fieldcat_lvc-intlen   = pv_ref_field+4(3).
    ENDIF.
  ELSE.
    ls_fieldcat_lvc-ref_field = pv_ref_field.
    ls_fieldcat_lvc-ref_table = pv_ref_table.
  ENDIF.

  IF pv_ltext IS NOT INITIAL.
    ls_fieldcat_lvc-scrtext_l = pv_ltext.                 " Long key word
    ls_fieldcat_lvc-scrtext_m = pv_ltext.                 " Middle key word
    ls_fieldcat_lvc-scrtext_s = pv_ltext.                 " Short key word

    ls_fieldcat_lvc-colddictxt = 'L'.
    ls_fieldcat_lvc-selddictxt = 'R'.
  ENDIF.

  CASE pv_type1.
    WHEN '1'. "Key field
      ls_fieldcat_lvc-key = 'X'.
    WHEN '2'.
      ls_fieldcat_lvc-no_zero = 'X'.
  ENDCASE.

* Check field type (1: normal, 2: hotstop, 3: hidden, 4: summary)
  CASE pv_type2.
    WHEN '1'.
      ls_fieldcat_lvc-just = 'C'.
*     Do nothing (normal type does not require additional parameter)
    WHEN '2'.
      ls_fieldcat_lvc-hotspot = 'X'.
    WHEN '3'.
      ls_fieldcat_lvc-no_out = 'X'.
    WHEN '4'.
      ls_fieldcat_lvc-do_sum = 'X'.
    WHEN '5'.
      ls_fieldcat_lvc-edit = 'X'.
    WHEN '6'.
      ls_fieldcat_lvc-edit = 'X'.
      ls_fieldcat_lvc-checkbox = 'X'.
*   WHEN OTHERS.
*     Do nothing (there are only 4 types sent to this routine)
  ENDCASE.

  APPEND ls_fieldcat_lvc TO gt_fieldcat_lvc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM build_layout .
  gs_layout_lvc-cwidth_opt = abap_true.
  gs_layout_lvc-zebra      = abap_true.
  gs_layout_lvc-stylefname = 'CELLTAB'.
*  gs_layout_lvc-no_rowmark = gc_x.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM call_alv_display .
  DATA: lv_repid TYPE sy-repid.

  lv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = lv_repid
      i_callback_pf_status_set = 'CALLBACK_PF_STATUS_SET'
      i_callback_user_command  = 'CALLBACK_USER_COMMAND'
      is_layout_lvc            = gs_layout_lvc
      it_fieldcat_lvc          = gt_fieldcat_lvc
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALLBACK_PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM callback_pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.

FORM transfer_smartforms CHANGING ps_data TYPE ty_output.
  ps_data = gs_data.
ENDFORM.

FORM callback_user_command USING r_ucomm TYPE sy-ucomm
                                 rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.
      EXIT.
    WHEN 'SALL'.
      PERFORM select_all.
    WHEN 'DALL'.
      PERFORM deselect_all.

    WHEN 'PRINT'.
      PERFORM refresh_alv.
      PERFORM print_form.
  ENDCASE.
* Refresh Screen
  rs_selfield-refresh = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_ALL
*&---------------------------------------------------------------------*
FORM select_all .
  FIELD-SYMBOLS: <ls_output> TYPE ty_output.

  LOOP AT gt_output ASSIGNING <ls_output>.
    <ls_output>-chbox = abap_true.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
FORM deselect_all .
  FIELD-SYMBOLS: <ls_output> TYPE ty_output.

  LOOP AT gt_output ASSIGNING <ls_output>.
    CLEAR: <ls_output>-chbox.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
FORM refresh_alv .
  DATA: lo_grid TYPE REF TO cl_gui_alv_grid.

  IF lo_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lo_grid.
  ENDIF.
  IF NOT lo_grid IS INITIAL.
    CALL METHOD lo_grid->check_changed_data.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
FORM print_form .
  DATA: lv_fname           TYPE rs38l_fnam,
        ls_control_param   TYPE ssfctrlop,
        ls_output_option   TYPE ssfcompop,
        ls_job_output_info TYPE ssfcrescl,
        ls_job_output_opt  TYPE ssfcresop.

*  DATA: lt_alv TYPE STANDARD TABLE OF ty_output,
*        ls_alv TYPE ty_output.

  lt_alv[] = gt_output[].
  DELETE lt_alv WHERE chbox IS INITIAL.

  IF lt_alv[] IS INITIAL.
    MESSAGE s000(38) WITH 'Please select at least one item' DISPLAY LIKE 'E'.

  ELSE.
*** Convert smartform name
    PERFORM call_ssf_function USING    gv_formname
                              CHANGING lv_fname.

*** Prepare parameters of smartform
    PERFORM set_form_parameter CHANGING ls_output_option
                                        ls_control_param.

    PERFORM open_form USING ls_output_option
                            ls_control_param
                            ls_job_output_opt.

    ls_control_param-no_close = abap_true.
    ls_control_param-no_open  = abap_true.

    LOOP AT lt_alv INTO ls_alv.
      MOVE-CORRESPONDING ls_alv TO gs_data.
******* Call smartform
        PERFORM call_smartform USING ls_control_param
                                     ls_output_option
                                     ls_job_output_info
                                     lv_fname.
    ENDLOOP.

    PERFORM close_form USING ls_job_output_info.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_SSF_FUNCTION
*&---------------------------------------------------------------------*
FORM call_ssf_function USING    pv_formname TYPE tnapr-sform
                       CHANGING pv_fname    TYPE rs38l_fnam.

  DATA lv_formname TYPE tdsfname.

  lv_formname = pv_formname.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_formname
    IMPORTING
      fm_name            = pv_fname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT_OPTION
*&---------------------------------------------------------------------*
FORM set_form_parameter CHANGING ps_output_option TYPE ssfcompop
                                 ps_control_param TYPE ssfctrlop.

  ps_output_option-tddest    = 'LOCL'.
  ps_output_option-tdimmed   = abap_true.
  ps_control_param-preview   = abap_true.
*  ps_control_param-no_dialog = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form OPEN_FORM
*&---------------------------------------------------------------------*
FORM open_form  USING    ps_output_option  TYPE ssfcompop
                         ps_control_param  TYPE ssfctrlop
                         ps_job_output_opt TYPE ssfcresop.
  CALL FUNCTION 'SSF_OPEN'
    EXPORTING
      user_settings      = space
      output_options     = ps_output_option
      control_parameters = ps_control_param
    IMPORTING
      job_output_options = ps_job_output_opt
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE 'E'.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_SMARTFORM
*&---------------------------------------------------------------------*
FORM call_smartform USING ps_control_param TYPE ssfctrlop
                          ps_output_option   TYPE ssfcompop
                          ps_job_output_info TYPE ssfcrescl
                          pv_fname TYPE rs38l_fnam.
  CALL FUNCTION pv_fname
    EXPORTING
      control_parameters = ps_control_param
      output_options     = ps_output_option
      user_settings      = ''
    IMPORTING
      job_output_info    = ps_job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE 'E'.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLOSE_FORM
*&---------------------------------------------------------------------*
FORM close_form  USING  ps_job_output_info TYPE ssfcrescl.
  CALL FUNCTION 'SSF_CLOSE'
    IMPORTING
      job_output_info  = ps_job_output_info
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE 'E'.
    LEAVE SCREEN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHANG_CONVERSION
*&---------------------------------------------------------------------*
FORM chang_conversion USING VALUE(a) TYPE any
      CHANGING VALUE(b) TYPE char16.
*        l_input = gs_ausp-atflv.
  CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
    EXPORTING
      i_number_of_digits   = 3
      i_fltp_value         = a "gs_ausp-atflv
*      i_screen_fieldlength = 16
*     I_VALUE_NOT_INITIAL_FLAG       = 'X'
*     I_SCREEN_FIELDLENGTH           = 16
    IMPORTING
      e_char_field       = b. "l_value.
                    .
ENDFORM.
