*&---------------------------------------------------------------------*
*& Include          ZXKKEU11
*&---------------------------------------------------------------------*
DATA: lt_vbpa      TYPE TABLE OF vbpa,      
lw_copa_item TYPE ce01000,      
lw_ce11000   TYPE ce11000,
lw_vbrp      TYPE vbrp,
lw_cepc      TYPE cepc,
lv_symbols   TYPE string.
FIELD-SYMBOLS: <fs_vbrk>    TYPE vbrk,
               <fs_vbrp>    TYPE vbrp,             
<fs_likp>    TYPE likp,            
<fs_lips>    TYPE lips,
<fs_vbpa>    TYPE vbpa,               
<fs_ce11000> TYPE ce11000,
<fs_kstar>   TYPE kstar.
lv_symbols = |(RK2L1000)GS_ITEM|.
ASSIGN (lv_symbols) TO <fs_ce11000>.
IF sy-subrc EQ 0.
lw_copa_item = i_copa_item.
CASE i_step_id.
WHEN 'U001'.
SELECT * FROM vbpa INTO TABLE lt_vbpa WHERE vbeln EQ <fs_ce11000>-kaufn.
LOOP AT lt_vbpa ASSIGNING <fs_vbpa>.
CASE <fs_vbpa>-parvw.
WHEN 'ZM'.
lw_copa_item-ww001 = <fs_vbpa>-kunnr.
ENDCASE.
ENDLOOP.
WHEN 'U002'.
ASSIGN ('(SAPLKAIP)CSKBV-KSTAR') TO <fs_kstar>.
IF sy-subrc EQ 0.
lw_copa_item-crmcsty = <fs_kstar>.
ENDIF.
ENDCASE.
e_copa_item = lw_copa_item.
e_exit_is_active = 'X'.
ENDIF.
