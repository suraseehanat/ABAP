METHOD if_ex_me_process_po_cust~post.
DATA: lwa_po_header TYPE mepoheader.
lwa_po_header = im_header->get_data( ).
IF  lwa_po_header-frgke = 'Z'.
SELECT SINGLE * FROM ztmm_po_email
WHERE ebeln = @lwa_po_header-ebeln
INTO @DATA(lv_ebeln).
IF sy-subrc <> 0.        DATA(lv_ztmm_po_email) = VALUE ztmm_po_email( ebeln = lwa_po_header-ebeln 
                                                                                          zflag = ''
                                                                                          ).
  INSERT ztmm_po_email FROM lv_ztmm_po_email.
  DATA : ls_jobname  TYPE  btcjob VALUE 'ZMMCHANGEDELDATE',
  ls_jobcount TYPE btcjobcnt,
  ls_sdate    TYPE btcxdate,
  ls_stime    TYPE btcxtime.
  CALL FUNCTION 'JOB_OPEN'
  EXPORTING
  jobname  = ls_jobname
  IMPORTING
  jobcount = ls_jobcount.
  SUBMIT zmmr011 VIA JOB ls_jobname NUMBER ls_jobcount
  WITH  s_pono = lwa_po_header-ebeln
  AND RETURN.
  MOVE : sy-datum TO ls_sdate.
  ls_stime = sy-uzeit + 10.
  CALL FUNCTION 'JOB_CLOSE'
  EXPORTING
  jobname  = ls_jobname
  jobcount = ls_jobcount
  sdlstrtdt = ls_sdate
  sdlstrttm  = ls_stime
  EXCEPTIONS
  cant_start_immediate = 1
  invalid_startdate    = 2
  jobname_missing      = 3
  job_close_failed     = 4
  job_nosteps          = 5
  job_notex            = 6
  lock_failed          = 7
  invalid_target       = 8
  OTHERS               = 9.
  ENDIF.
  ENDIF.
  ENDMETHOD.
