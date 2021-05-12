FUNCTION ZMM_PRINT_PO_OTF
  IMPORTING
    IV_EBELN TYPE EBELN
  EXPORTING
    IT_ITCOO TYPE ZITCOO.




  DATA: l_druvo       LIKE t166k-druvo,
        nast          LIKE nast,
        l_nast        LIKE nast,
        l_from_memory,
        ent_screen,
        ent_retco     TYPE i,
        l_doc         TYPE meein_purchase_doc_print,
        lv_id(50)     TYPE c.


  DATA : toa_dara   TYPE toa_dara,
         arc_params TYPE arc_params,
         lv_form    TYPE tnapr-fonam VALUE 'ZMMF001',
         lt_otf     TYPE TABLE OF itcoo.

  CLEAR ent_retco.

  IF nast-aende EQ space.
    l_druvo = '1'.
  ELSE.
    l_druvo = '2'.
  ENDIF.
  SELECT SINGLE * FROM nast
    INTO nast
    WHERE objky = iv_ebeln
    AND   vstat = '0'.

  ent_screen = 'X'.

  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      ix_nast        = nast
      ix_screen      = ent_screen
    IMPORTING
      ex_retco       = ent_retco
      ex_nast        = l_nast
      doc            = l_doc
    CHANGING
      cx_druvo       = l_druvo
      cx_from_memory = l_from_memory.

  CHECK ent_retco EQ 0.

  CALL FUNCTION 'ZME_PRINT_PO_OTF'
    EXPORTING
      ix_nast        = l_nast
      ix_druvo       = l_druvo
      doc            = l_doc
      ix_screen      = ent_screen
      ix_from_memory = l_from_memory
      ix_toa_dara    = toa_dara
      ix_arc_params  = arc_params
      ix_fonam       = lv_form          "HW 214570
    IMPORTING
      ex_retco       = ent_retco.

  CONCATENATE 'OTF' l_doc-xekko-ebeln INTO lv_id.
  IMPORT lw_job_output_info-otfdata TO lt_otf FROM MEMORY ID lv_id.
  FREE MEMORY ID lv_id.

  it_itcoo[] = lt_otf[].

ENDFUNCTION.
