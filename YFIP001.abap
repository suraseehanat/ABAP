*&---------------------------------------------------------------------*
*& Report YFIP001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

* -------- 2018/01/17 ------------------------ *
*            1. Add field จำนวนครั้งที่ print        *Y
*            2. Save log print                 *
* -------------------------------------------- *

REPORT yfip001_n.

TABLES: bkpf, bsid, bsad, t052u, kna1, adrc, fitha_pbupl_d_t, yfilog.

INCLUDE yfip001_type.

DATA: gt_output TYPE TABLE OF ty_output, gs_output TYPE ty_output,
      gt_main   TYPE TABLE OF bsid,
      gs_main   TYPE bsid,
      gt_ref    TYPE TABLE OF bsid,
      gs_ref    TYPE bsid,
      gt_term   TYPE TABLE OF t052u,
      gs_term   TYPE t052u,
      gt_kna1   TYPE TABLE OF kna1,
      gs_kna1   TYPE kna1,
      gt_adrc   TYPE TABLE OF adrc,
      gs_adrc   TYPE adrc,
      gt_branch TYPE TABLE OF fitha_pbupl_d_t,
      gs_branch TYPE fitha_pbupl_d_t,
      gs_data   TYPE ty_output,
      gt_yfilog TYPE TABLE OF yfilog,
      gs_yfilog TYPE yfilog ,
      gt_log    TYPE TABLE OF yfilog ,
      gs_log    TYPE yfilog .

DATA: gv_tax_number TYPE  stcd1.
" ===  ALV
DATA: gt_fieldcat_lvc TYPE lvc_t_fcat,
      gs_layout_lvc   TYPE lvc_s_layo.

"== smartforms
DATA: gv_formname TYPE tdsfname.
CONSTANTS: gv_formname01 TYPE tdsfname VALUE 'YFIF001_01'.
CONSTANTS: gv_formname02 TYPE tdsfname VALUE 'YFIF001_02'.

DATA: lt_alv TYPE STANDARD TABLE OF ty_output,
      ls_alv TYPE ty_output.

DATA: count TYPE i.
FIELD-SYMBOLS <gs_output> TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK sec_radio WITH FRAME TITLE radio.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: p_r1 RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND rad.
SELECTION-SCREEN COMMENT (40) r1line1 FOR FIELD p_r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_r2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT (40) r1line2 FOR FIELD p_r2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sec_radio.


SELECTION-SCREEN BEGIN OF BLOCK sec_main WITH FRAME TITLE title1.
*SELECTION-SCREEN SKIP .
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line1 FOR FIELD s_org.
SELECT-OPTIONS s_org FOR bsid-bukrs  NO INTERVALS NO-EXTENSION ."DEFAULT '1000'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line2 FOR FIELD s_doc.
SELECT-OPTIONS s_doc  FOR bsid-belnr .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line3 FOR FIELD s_year.
SELECT-OPTIONS s_year  FOR bsid-gjahr  NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line4 FOR FIELD s_type.
SELECT-OPTIONS s_type  FOR bsid-blart NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line5 FOR FIELD s_date.
SELECT-OPTIONS s_date  FOR bsid-budat.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) b1line6 FOR FIELD s_cust.
SELECT-OPTIONS s_cust  FOR bsid-kunnr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK sec_main.

INITIALIZATION.
  radio = 'Selecttion Type'.
  r1line1 = 'Invoice'.
  r1line2 = 'Credit Note'.
  title1 = 'Selection Criteria'.
  b1line1 = 'Company Code'.
  b1line2 = 'Document No.'.
  b1line3 = 'Fiscal Year'.
  b1line4 = 'Document type'.
  b1line5 = 'Posting Date'.
  b1line6 = 'Customer No.'.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'S_ORG-LOW' OR screen-name = 'S_DATE-LOW' .
      screen-required = '2'.
    ENDIF.
    MODIFY SCREEN.
    CONTINUE.
  ENDLOOP.

AT SELECTION-SCREEN.

  IF sy-ucomm NE 'RAD'.
    IF s_org-low IS INITIAL.
*    SET CURSOR FIELD s_org DISPLAY OFFSET 0.
      MESSAGE e055(00).
      EXIT.
    ELSEIF s_date-low IS INITIAL.
*    SET CURSOR FIELD s_date DISPLAY OFFSET 0.
      MESSAGE e055(00).
      EXIT.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  IF p_r1 = 'X'.
    gv_formname = gv_formname01.
    SELECT * FROM bsid INTO TABLE gt_main
      WHERE bukrs IN s_org AND belnr IN s_doc AND gjahr IN s_year AND
            blart IN s_type AND ( blart = 'DD' OR blart = 'DR' OR blart = 'DN' ) AND
            budat IN s_date AND kunnr IN s_cust.

    SELECT * FROM bsad APPENDING TABLE gt_main
      WHERE bukrs IN s_org AND belnr IN s_doc AND gjahr IN s_year AND
            blart IN s_type AND ( blart = 'DD' OR blart = 'DR' OR blart = 'DN' ) AND
            budat IN s_date AND kunnr IN s_cust.
  ELSEIF p_r2 = 'X'.
    gv_formname = gv_formname02.
    SELECT * FROM bsid INTO TABLE gt_main
      WHERE bukrs IN s_org AND belnr IN s_doc AND gjahr IN s_year AND
            blart IN s_type AND ( blart = 'DG' ) AND
            budat IN s_date AND kunnr IN s_cust.

    SELECT * FROM bsad APPENDING TABLE gt_main
      WHERE bukrs IN s_org AND belnr IN s_doc AND gjahr IN s_year AND
            blart IN s_type AND ( blart = 'DG' ) AND
            budat IN s_date AND kunnr IN s_cust.
  ENDIF.

  SORT gt_main BY belnr.
  IF gt_main IS NOT INITIAL.
    SELECT * FROM bsid INTO TABLE gt_ref
      FOR ALL ENTRIES IN gt_main
        WHERE belnr = gt_main-rebzg AND bukrs = gt_main-bukrs AND gjahr = gt_main-rebzj.

    SELECT * FROM bsad APPENDING TABLE gt_ref
      FOR ALL ENTRIES IN gt_main
        WHERE  belnr = gt_main-rebzg AND bukrs = gt_main-bukrs AND gjahr = gt_main-rebzj.

    SELECT * FROM t052u INTO TABLE gt_term
      FOR ALL ENTRIES IN gt_main
        WHERE zterm = gt_main-zterm.

    SELECT * FROM kna1 INTO TABLE gt_kna1
      FOR ALL ENTRIES IN gt_main
        WHERE kunnr = gt_main-kunnr.

    IF gt_kna1 IS NOT INITIAL.
      SELECT * FROM adrc INTO TABLE gt_adrc
        FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.

      SELECT * FROM fitha_pbupl_d_t INTO TABLE gt_branch
        FOR ALL ENTRIES IN gt_kna1
          WHERE kunnr = gt_kna1-kunnr.
    ENDIF.
  ELSE.
    MESSAGE s001(00) WITH 'Data not found' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM yfilog
    INTO TABLE gt_yfilog
    FOR ALL ENTRIES IN gt_main
    WHERE belnr = gt_main-belnr .

  LOOP AT gt_main INTO gs_main.
    gs_output-org = gs_main-bukrs.
    gs_output-year = gs_main-gjahr.
    gs_output-doc = gs_main-belnr.
    gs_output-date = gs_main-budat.
    gs_output-doc_ref  = gs_main-xblnr.
    gs_output-date_ref = gs_main-bldat.
***
    gs_output-ref3 = gs_main-xref3.
***
    gs_output-qty = 1.
*    gs_output-ref_no = gs_main-rebzg.

    READ TABLE gt_ref INTO gs_ref WITH KEY belnr = gs_main-rebzg bukrs = gs_main-bukrs gjahr = gs_main-gjahr.
    IF sy-subrc = 0.
      gs_output-ref_no = gs_ref-xblnr.

    ENDIF.

    IF gs_output-ref_no IS NOT INITIAL.
      READ TABLE gt_ref INTO gs_ref WITH KEY belnr = gs_main-rebzg
                                             bukrs = gs_main-bukrs
                                             gjahr = gs_main-rebzj.
      IF sy-subrc = 0.
        IF gs_main-bukrs = '1000'.
          gs_output-ref_date = gs_ref-budat.
        ELSEIF gs_main-bukrs = '2000'.
          gs_output-ref_date = gs_ref-bldat.
        ENDIF.

        gs_output-ref_amt = gs_ref-skfbt.
      ENDIF.
    ENDIF.

    IF gs_main-zterm IS NOT INITIAL.
      CLEAR: gs_term.
      READ TABLE gt_term INTO gs_term WITH KEY zterm = gs_main-zterm.
      IF sy-subrc = 0.
        gs_output-term = gs_term-text1.
      ENDIF.
    ENDIF.

    gs_output-delivery = gs_main-budat.
    gs_output-cust = gs_main-kunnr.
    gs_output-price = gs_main-skfbt.
    gs_output-vat = gs_main-mwskz.
    gs_output-remark = gs_main-sgtxt.

    IF gs_main-kunnr IS NOT INITIAL.
      READ TABLE gt_kna1 INTO gs_kna1 WITH KEY kunnr = gs_main-kunnr.
      IF sy-subrc = 0.
        READ TABLE gt_adrc INTO gs_adrc WITH KEY addrnumber = gs_kna1-adrnr.
        IF sy-subrc = 0.
          gs_output-name1 = gs_adrc-name1.
          gs_output-name2 = gs_adrc-name2.
          gs_output-tax = gv_tax_number = gs_kna1-stcd3(13).
          gs_output-street = gs_adrc-street.
          gs_output-suppl3 = gs_adrc-str_suppl3.
          gs_output-location = gs_adrc-location.
          gs_output-city2 = gs_adrc-city2.
          gs_output-city1 = gs_adrc-city1.
          gs_output-post = gs_adrc-post_code1.
          gs_output-tel = gs_adrc-tel_number.
          gs_output-fax = gs_adrc-fax_number.
          READ TABLE gt_branch INTO gs_branch WITH KEY kunnr = gs_kna1-kunnr.
          IF sy-subrc = 0.
            gs_output-branch = gs_branch-description.
          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT gt_yfilog INTO gs_yfilog WHERE belnr = gs_main-belnr.
        count = count + 1 .
      ENDLOOP.
      gs_output-print = count .

      APPEND gs_output TO gt_output.
      CLEAR:gs_output, gs_ref, gs_kna1, gs_adrc, gs_term, gs_branch, count.
    ENDIF.
  ENDLOOP.

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
    USING 'CHBOX'  'CHAR1' space  'Select'       lc_key lc_chbox '',
*    USING 'Qty'   space space 'Quantity' space lc_edit,
    USING 'Doc'    'BELNR' 'BSID' space          space  space '',
    USING 'Date'   'BELNR' 'BSID' 'Posting Date' space  space '',
    USING 'Print'  'BELNR' 'BSID' 'จำนวนครั้งที่ปริ้น'             ''    ''  'C'.

*  IF rb_file EQ abap_true.
*    PERFORM alv_fieldcat:
*      USING 'MESSAGE' 'MESSAGE' 'BAPIRET2' space space space.
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM alv_fieldcat USING pv_name TYPE lvc_fname " Field Name
                        pv_ref_field TYPE lvc_fname  " Ref field name
                        pv_ref_table TYPE lvc_tname  " Ref table name
                        pv_ltext     TYPE c          " Column Title
                        pv_type1     TYPE c          " Key, Q, C
                        pv_type2     TYPE c          " Field Type

                        pv_dataalign TYPE c.

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

  "Set alignment column
  ls_fieldcat_lvc-just  = pv_dataalign.

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
      PERFORM print_form USING ''.
    WHEN 'PREV' .
      PERFORM refresh_alv.
      PERFORM print_form USING 'X'.
  ENDCASE.
* Refresh Screen
  rs_selfield-refresh = 'X'.
*
*  CASE fcode.
*    WHEN 'SPRI'.
*    WHEN OTHERS.
*  ENDCASE.
ENDFORM.

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
FORM print_form USING pv_preview TYPE c..
  DATA: lv_fname           TYPE rs38l_fnam,
        ls_control_param   TYPE ssfctrlop,
        ls_output_option   TYPE ssfcompop,
        ls_job_output_info TYPE ssfcrescl,
        job_output_info    TYPE ssfcrescl,
        ls_job_output_opt  TYPE ssfcresop,
        ls_f_code          TYPE sy-ucomm .

  DATA: ssfctrlop    TYPE ssfctrlop. " Prin Preview

  DATA: printer_param  TYPE ssfcompop,
        printing_infos TYPE ssfcrescl.

  lt_alv[] = gt_output[].
  DELETE lt_alv WHERE chbox IS INITIAL.

  IF lt_alv[] IS INITIAL.
    MESSAGE s000(38) WITH 'Please select at least one item' DISPLAY LIKE 'E'.

  ELSE.
*** Convert smartform name
    PERFORM call_ssf_function USING    gv_formname
                              CHANGING lv_fname.

*** Prepare parameters of smartform
    PERFORM set_form_parameter  USING    pv_preview
                                CHANGING ls_output_option
                                         ls_control_param.

***    PERFORM open_form USING ls_output_option
***                            ls_control_param
***                            ls_job_output_opt.

*    ls_control_param-no_close = abap_true.
*    ls_control_param-no_open  = abap_true.

    LOOP AT lt_alv INTO ls_alv.
      AT FIRST.
        ls_control_param-no_close = abap_true.
      ENDAT.
      AT LAST.
        CLEAR ls_control_param-no_close.
      ENDAT.
      MOVE-CORRESPONDING ls_alv TO gs_data.

      DO gs_data-qty TIMES.
****** Call smartform
        PERFORM call_smartform USING ls_control_param
                                     ls_output_option
                                     ls_job_output_info
                                     lv_fname.
      ENDDO.
    ENDLOOP.

*    PERFORM close_form USING ls_job_output_info ls_f_code  .
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_SMARTFORM
*&---------------------------------------------------------------------*
FORM call_smartform USING ps_control_param   TYPE ssfctrlop
                          ps_output_option   TYPE ssfcompop
                          ps_job_output_info TYPE ssfcrescl
                          pv_fname           TYPE rs38l_fnam.
  data : gwa_settings   TYPE ssfcompop.

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
  ELSE.

    ps_control_param-no_open = abap_true.
*     ---------- Save log table yfip001_ta01 ------------
    IF ps_job_output_info-outputdone EQ 'X'.
      LOOP AT lt_alv INTO ls_alv .
        gs_log-bukrs = ls_alv-org .
        gs_log-belnr = ls_alv-doc .
        gs_log-erdat = sy-datum .
        gs_log-eruhr = sy-uzeit .
        gs_log-tcode = sy-repid .
        gs_log-usnam = sy-uname .
        INSERT INTO yfilog VALUES gs_log.

        IF sy-subrc = 0.
          LOOP AT gt_output ASSIGNING <gs_output> WHERE doc = ls_alv-doc .
            <gs_output>-print = <gs_output>-print + 1.
          ENDLOOP.
        ENDIF.

        CLEAR: ls_alv, gs_log.
      ENDLOOP.
      MESSAGE 'บันทึกข้อมูลเรียบร้อยแล้ว' TYPE 'S' DISPLAY LIKE  'S'.
    ENDIF.

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
*& Form CLOSE_FORM
*&---------------------------------------------------------------------*
FORM close_form  USING ps_job_output_info TYPE ssfcrescl
                       fcode  TYPE sy-ucomm.

    CASE fcode.
    WHEN 'SPRI'.
    WHEN OTHERS.
  ENDCASE.

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
  ELSE.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT_OPTION
*&---------------------------------------------------------------------*
FORM set_form_parameter USING    pv_preview TYPE c
                        CHANGING ps_output_option TYPE ssfcompop
                                 ps_control_param TYPE ssfctrlop.

  ps_output_option-tddest    = 'LOCL'.
  ps_output_option-tdimmed   = abap_true.
**  ps_control_param-preview   = abap_true.

  IF pv_preview EQ abap_true.   "Preview
    ps_control_param-preview   = abap_true.
    ps_output_option-tdnoprint = abap_true.

  ELSE.                       "Print
*    ps_control_param-no_dialog = abap_true.
    ps_output_option-tdnoprev  = abap_true.
  ENDIF.
ENDFORM.
