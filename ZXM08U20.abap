*&---------------------------------------------------------------------*
*&  Include           ZXM08U20
*&---------------------------------------------------------------------*
FIELD-SYMBOLS <fs_ersba> TYPE any.
ASSIGN ('(RMMR1MRS)ERSBA') TO <fs_ersba>.
DATA: wa_mkpf    TYPE mkpf,
wa_bupla   LIKE t001w-j_1bbranch,
wa_selwenr TYPE ek08erswe,
wa_mseg    TYPE mseg,
wa_ekko    TYPE ekko,
wa_name    TYPE adrc-name1,
wa_country TYPE adrc-country,
wa_city    TYPE rbkp-ort01.
DATA : lv_name  LIKE thead-tdname,
it_lines LIKE tline OCCURS 0 WITH HEADER LINE.
IF <fs_ersba> IS ASSIGNED.
READ TABLE t_selwenr INTO wa_selwenr INDEX 1.
SELECT SINGLE * INTO wa_ekko FROM ekko WHERE ebeln = wa_selwenr-ebeln.
MOVE-CORRESPONDING i_rbkpv TO  e_rbkpv_ers_change.
SELECT SINGLE * INTO wa_mkpf FROM mkpf WHERE mblnr = wa_selwenr-lfbnr AND mjahr = wa_selwenr-lfgja.
SELECT SINGLE * INTO wa_mseg FROM mseg WHERE mblnr = wa_selwenr-lfbnr AND mjahr = wa_selwenr-lfgja.
SELECT SINGLE j_1bbranch INTO wa_bupla FROM t001w WHERE werks = wa_mseg-werks.
e_rbkpv_ers_change-bldat = wa_mkpf-bldat.
e_rbkpv_ers_change-budat = wa_mkpf-budat.
e_rbkpv_ers_change-xblnr = wa_mkpf-xblnr.
e_rbkpv_ers_change-bupla = wa_bupla.
e_change = 'X'.
*Add Text
lv_name = wa_selwenr-ebeln.
CALL FUNCTION 'READ_TEXT'
EXPORTING
client   = sy-mandt
id       = 'F01'
language = 'E'
name     = lv_name
object   = 'EKKO'
TABLES
lines    = it_lines.
LOOP AT it_lines.
e_rbkpv_ers_change-sgtxt = it_lines-tdline.
EXIT.
ENDLOOP.
ENDIF.
