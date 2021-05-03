*&---------------------------------------------------------------------*
*& Report zcds_wo_status_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcds_wo_status_report.
DATA : i_wo_status TYPE STANDARD TABLE OF ZDDLS_WO_STATUS.

selection-screen begin OF BLOCK a01 WITH FRAME TITLE text-001.
PARAMETERS : p_status TYPE j_status.
SELECTION-SCREEN END OF BLOCK a01.

START-OF-SELECTION.

PERFORM sub_get_data_form_cds.
END-OF-SELECTION.

PERFORM sub_display_data.

FORM sub_get_data_form_cds.
    select * from ZDDLS_WO_STATUS( P_STAT = @p_status ) into table @i_wo_status.

ENDFORM.

FORM sub_display_data.

DATA :
         lv_status_rel TYPE j_status VALUE 'I0002', " Release Status
         lr_functions TYPE REF TO cl_salv_functions, " ALV Functions
         lr_alv TYPE REF TO cl_salv_table,
         lr_display TYPE REF TO cl_salv_display_settings,
         lv_salv_msg TYPE REF TO cx_salv_msg.

 IF i_wo_status IS NOT INITIAL.
 TRY.
  cl_salv_table=>factory(  IMPORTING r_salv_table = lr_alv CHANGING t_table = i_wo_status ).
 CATCH cx_salv_msg INTO lv_salv_msg.
 MESSAGE lv_salv_msg TYPE 'E'.
 ENDTRY.

 lr_functions = lr_alv->get_functions(  ).
 lr_functions->set_all( abap_true ).
 lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
 lr_display->set_list_header( text-001 ).

 lr_alv->display(  ).
 ELSE.
 MESSAGE 'No data found' TYPE 'I'.
 LEAVE LIST-PROCESSING.
 ENDIF.

ENDFORM.
