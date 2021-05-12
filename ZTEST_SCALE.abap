*&---------------------------------------------------------------------*
*& Report ZTEST_SCALE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscaletest.

DATA: lv_weight TYPE string.

*PARAMETERS: p_weight TYPE string.
PARAMETERS: p_weight TYPE mseg-menge.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 70(10) TEXT-001 USER-COMMAND cli1.
SELECTION-SCREEN END OF LINE.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CLI1'.
      CLEAR lv_weight.

      CALL FUNCTION 'ZFUNC_INTERFACE_SERAIL_COMP'
        EXPORTING
          im_mode          = '0'
          im_commport      = '1'
          im_settings      = '9600,N,8,1'
*         IM_OUTPUT        =
*         IM_USB           = 'X'
        IMPORTING
          ex_input         = lv_weight
*         EX_EVENT_MSG     =
        EXCEPTIONS
          no_create_object = 1
          OTHERS           = 2.
      IF sy-subrc EQ 0.
*        p_weight = lv_weight.
        PERFORM read_weight USING    lv_weight
                                     'kg'
                                     '1'
                            CHANGING p_weight.
      ENDIF.
  ENDCASE.

*&---------------------------------------------------------------------*
*& Form READ_WEIGHT
*&---------------------------------------------------------------------*
FORM read_weight  USING    pv_input TYPE string
                           pv_str   TYPE c
                           pv_div   TYPE i
                  CHANGING pv_scale_menge TYPE mseg-menge.
  DATA: lv_kg01   TYPE mseg-menge,
        lv_kg02   TYPE mseg-menge,
        lv_kg03   TYPE mseg-menge,
        lv_kg04   TYPE mseg-menge,
        lv_kg05   TYPE mseg-menge,
        lv_kg06   TYPE mseg-menge,
        lv_kg07   TYPE mseg-menge,
        lv_kg08   TYPE mseg-menge,
        lv_kg09   TYPE mseg-menge,
        lv_kg10   TYPE mseg-menge,
        lv_kg11   TYPE mseg-menge,
        lv_kg12   TYPE mseg-menge,
        lv_kg13   TYPE mseg-menge,
        lv_kg14   TYPE mseg-menge,
        lv_kg15   TYPE mseg-menge,
        lv_kg16   TYPE mseg-menge,
        lv_kg17   TYPE mseg-menge,
        lv_kg18   TYPE mseg-menge,
        lv_kg19  TYPE mseg-menge,
        lv_kg20   TYPE mseg-menge,
        lv_kg01_t TYPE string,
        lv_kg02_t TYPE string,
        lv_kg03_t TYPE string,
        lv_kg04_t TYPE string,
        lv_kg05_t TYPE string,
        lv_kg06_t TYPE string,
        lv_kg07_t TYPE string,
        lv_kg08_t TYPE string,
        lv_kg09_t TYPE string,
        lv_kg10_t TYPE string,
        lv_kg11_t TYPE string,
        lv_kg12_t TYPE string,
        lv_kg13_t TYPE string,
        lv_kg14_t TYPE string,
        lv_kg15_t TYPE string,
        lv_kg16_t TYPE string,
        lv_kg17_t TYPE string,
        lv_kg18_t TYPE string,
        lv_kg19_t TYPE string,
        lv_kg20_t TYPE string,
        lv_weight TYPE mseg-menge.

  CLEAR: pv_scale_menge.

  BREAK abap01.

  CONDENSE pv_input.
  SPLIT pv_input AT space INTO lv_kg01_t lv_kg02_t
                               lv_kg03_t lv_kg04_t
                               lv_kg05_t lv_kg06_t.



  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg01_t WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg02_t WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg03_t WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg04_t WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg05_t WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_kg06_t WITH ''.






  lv_kg01 = lv_kg01_t.
  lv_kg02 = lv_kg02_t.
  lv_kg03 = lv_kg03_t.
  lv_kg04 = lv_kg04_t.
  lv_kg05 = lv_kg05_t.


CLEAR: lv_kg06_t.
  lv_kg06 = lv_kg06_t.

  IF lv_kg01 IS NOT INITIAL.
    lv_weight = lv_kg01 / pv_div.
  ELSEIF lv_kg02 IS NOT INITIAL.
    lv_weight = lv_kg02 / pv_div.
  ELSEIF lv_kg03 IS NOT INITIAL.
    lv_weight = lv_kg03 / pv_div.
  ELSEIF lv_kg04 IS NOT INITIAL.
    lv_weight = lv_kg04 / pv_div.
  ELSEIF lv_kg05 IS NOT INITIAL.
    lv_weight = lv_kg05 / pv_div.
  ELSEIF lv_kg06 IS NOT INITIAL.
    lv_weight = lv_kg06 / pv_div.

  ENDIF.

 ENDFORM.
*
*  pv_scale_menge = l
