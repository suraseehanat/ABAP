*&---------------------------------------------------------------------*
*& Report zcoxy004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcoxy004.

TYPE-POOLS: slis.

tables: coss, afru,mcafpov,cobk,afko,afpo,
        s026, aufk.

DATA:    g_repid     LIKE sy-repid.
*
DATA: BEGIN OF gt_objnr OCCURS 100,
      objnr LIKE coep-objnr,
      END OF gt_objnr.

data: begin of i_list occurs 0,
      RUECK like afru-RUECK,              "操作完成的确认编号
      ERSDA like afru-ERSDA,              "确认输入日期
      BUDAT like afru-BUDAT,              "记帐日期
      ILE01 like afru-ILE01,              "要确认作业的计量单位
      ISM01 like afru-ISM01,              "当前需确认作业
      ILE02 like afru-ILE02,"
      ISM02 like afru-ISM02,"
      ILE03 like afru-ILE03,"
      ISM03 like afru-ISM03,"
      ILE04 like afru-ILE04,"
      ISM04 like afru-ISM04,"
      ILE05 like afru-ILE05,"
      ISM05 like afru-ISM05,"
      ILE06 like afru-ILE06,"
      ISM06 like afru-ISM06,"
      GMNGA like afru-GMNGA,              "按订单单位的先前确认结果
      LMNGA like afru-LMNGA,              "待确认的产量
      GMEIN like afru-GMEIN,              "基本计量单位
      WABLNR like afru-WABLNR,            "物料凭证编号
      AUFNR like afru-AUFNR,              "订单号
      objnr like aufk-objnr,              "对象号
      end of i_list.

data: begin of gt_list occurs 0,
      objnr like aufk-objnr,
      ILE01 like afru-ILE01,
      ISM01 like afru-ISM01,
      ILE02 like afru-ILE02,"
      ISM02 like afru-ISM02,"
      ILE03 like afru-ILE03,"
      ISM03 like afru-ISM03,"
      ILE04 like afru-ILE04,"
      ISM04 like afru-ISM04,"
      ILE05 like afru-ILE05,"
      ISM05 like afru-ISM05,"
      ILE06 like afru-ILE06,"
      ISM06 like afru-ISM06,"
      end of gt_list.

data: begin of i_coss occurs 0,
      objnr like coss-objnr,
      kstar like coss-kstar,               "成本要素
      wkg001 like coss-wkg001,
      wkg002 like coss-wkg002,
      wkg003 like coss-wkg003,
      wkg004 like coss-wkg004,
      wkg005 like coss-wkg005,
      wkg006 like coss-wkg006,
      wkg007 like coss-wkg007,
      wkg008 like coss-wkg008,
      wkg009 like coss-wkg009,
      wkg010 like coss-wkg010,
      wkg011 like coss-wkg011,
      wkg012 like coss-wkg012,
      wkg013 like coss-wkg013,
      wkg014 like coss-wkg014,
      wkg015 like coss-wkg015,
      wkg016 like coss-wkg016,

      end of i_coss.

data: begin of gt_coss occurs 0,
      objnr like coss-objnr,
      withdrawn_m LIKE s026-enwrt,          "机器
      withdrawn_l LIKE s026-enwrt,          "人工
      end of gt_coss.

data: begin of zyjg occurs 0,
      objnr like coss-objnr,
      withdrawn_01 LIKE s026-enwrt,          "机器作业价格
      withdrawn_02 LIKE s026-enwrt,          "人工作业价格
      end of zyjg.

data: begin of itab1 occurs 0,
      matnr(18)   type c,        "物料编码
      plnnr like mapl-plnnr ,    "任务清单组码
      plnkn like plpo-plnkn ,    "任务清单节点数
      vornr like plpo-vornr ,    "作业编号
      lar01 like plpo-lar01 ,    "活动类型
      vgw01 like plpo-vgw01 ,    "标准值
      vge01 like plpo-vge01 ,    "标准值计量单位
      vgw02 like plpo-vgw02 ,    "标准值
      vge02 like plpo-vge02 ,    "标准值计量单位
      lar02 like plpo-lar02 ,    "活动类型
      lar03 like plpo-lar03 ,    "活动类型
      vgw03 like plpo-vgw03 ,    "标准值
      vge03 like plpo-vge03 ,    "标准值计量单位
      lar04 like plpo-lar04 ,    "活动类型
      vgw04 like plpo-vgw04 ,    "标准值
      vge04 like plpo-vge04 ,    "标准值计量单位
      lar05 like plpo-lar05 ,    "活动类型
      vgw05 like plpo-vgw05 ,    "标准值
      vge05 like plpo-vge05 ,    "标准值计量单位
      lar06 like plpo-lar06 ,    "活动类型
      vgw06 like plpo-vgw06 ,    "标准值
      vge06 like plpo-vge06 ,    "标准值计量单位
      plnty like plpo-plnty ,    "任务清单类型
      zaehl like plpo-zaehl ,    "内部计数器
      loekz like plas-loekz ,    "删除标志
      werks like mapl-werks ,    "工厂
      arbpl like crhd-arbpl,     "工作中心
      objid like crhd-objid,     "资源的对象 id

      end of itab1.

*用作输出的内表
data: begin of gt_result occurs 0,
      objnr like aufk-objnr,
      aufnr like afru-aufnr,              "订单号
      matnr like afko-plnbez,
      plnnr like afko-plnnr,
      budat like afru-budat,
      gmnga like afru-gmnga,          "数量
      gmein like afru-gmein,          "单位
      ism01 like afru-ism01,          "机器时间
      ile01 like afru-ile01,          "时间单位-运算时转换为H
      ism01_q like afru-ism01,        "标准机器时间
      ism02 like afru-ism02,          "人工时间
      ile02 like afru-ile02,          "时间单位-运算时转换为H
      ism02_q like afru-ism02,        "标准人工时间
      withcost_m like s026-enwrt,
      withcost_l like s026-enwrt,
      withcost_q_m like s026-enwrt,
      withcost_q_l like s026-enwrt,
      with_ef_m type p decimals 2,    "机器效率
      with_ef_l type p decimals 2,    "人工效率

      end of gt_result.

*global value of alv display
DATA:
      gt_sort  TYPE slis_t_sortinfo_alv,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_event TYPE slis_alv_event OCCURS 0 WITH HEADER LINE, "Grid输出表头事件内表
      gt_head TYPE slis_t_listheader.     "Grid输出表头


*选择条件:
SELECTION-SCREEN BEGIN OF BLOCK sc_1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_rueck FOR afru-rueck ,
                s_aufnr for afru-aufnr MEMORY ID auf MODIF ID auf.
SELECTION-SCREEN END OF BLOCK sc_1.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.


*计算数据
  perform select_data.
  perform append_gt_result.

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .
  RANGES: lr_kstar FOR coep-kstar,
          lr_gjahr  FOR coep-gjahr,
          lr_perio  FOR coep-perio,
          lr_parob1  FOR coep-parob1,
          lr_objnr FOR coep-objnr.

*得到订单的确认消耗成本单位-分批的
select rueck
      ersda
      budat
      ile01 "
      ism01 "
      ile02 "
      ism02 "
      ile03 "
      ism03 "
      ile04 "
      ism04 "
      ile05 "
      ism05 "
      ile06 "
      ism06 "
      gmnga               "按订单单位的先前确认结果
      lmnga               "待确认的产量
      gmein               "基本计量单位
      wablnr              "物料凭证编号
      aufnr
      from afru
      into table i_list
      where aufnr in s_aufnr.

*求总消耗时间
loop at i_list.
    select single * from aufk where aufnr = i_list-aufnr.
    i_list-objnr = aufk-objnr.
    if i_list-ile01 = 'MIN'.
      gt_list-ism01 = i_list-ism01 / 60.   "机器工时
    else.
      gt_list-ism01 = i_list-ism01.
    endif.
    if i_list-ile02 = 'MIN'.
      gt_list-ism02 = i_list-ism02 / 60.
    else.
      gt_list-ism02 = i_list-ism02.       "人工工时
    endif.
    gt_list-ism03 = i_list-ism03.
    gt_list-ism04 = i_list-ism04.
    gt_list-ism05 = i_list-ism05.
    gt_list-ism06 = i_list-ism06.
    gt_list-objnr = i_list-objnr.

    CONCATENATE 'OR' i_list-aufnr INTO gt_objnr-objnr .

    APPEND gt_objnr.
    clear gt_objnr.
    modify i_list.
    collect gt_list.
    clear gt_list.
endloop.

**************************************************************
*取制造成本
  IF NOT gt_objnr[] IS INITIAL.
    lr_kstar-option = 'EQ'.
    lr_kstar-sign = 'I'.
    lr_kstar-low = '0000600100'.
    APPEND lr_kstar.
    lr_kstar-low = '0000600210'.
    append lr_kstar.
    lr_objnr-option = 'EQ'.
    lr_objnr-sign = 'I'.

    LOOP AT gt_objnr.
      lr_objnr-low = gt_objnr-objnr.
      APPEND lr_objnr.
    ENDLOOP.

    SELECT * INTO corresponding fields of table i_coss
      FROM coss
      WHERE objnr in lr_objnr
        AND kstar in lr_kstar
        AND wrttp = '04'.                         "实际成本消耗
*分离成本
    loop at i_coss.
        gt_coss-objnr = i_coss-objnr.

      CASE i_coss-kstar.
        WHEN  '0000600100'.
*实际机器制造费用-总
           gt_coss-withdrawn_m = i_coss-wkg001 + i_coss-wkg002 + i_coss-wkg003 +
                  i_coss-wkg004 + i_coss-wkg005 + i_coss-wkg006 +
                  i_coss-wkg007 + i_coss-wkg008 + i_coss-wkg009 +
                  i_coss-wkg010 + i_coss-wkg011 + i_coss-wkg012 +
                  i_coss-wkg013 + i_coss-wkg014 + i_coss-wkg015 +
                  i_coss-wkg016 .
        WHEN '0000600210'.
*实际DIP人工-总
           gt_coss-withdrawn_l = i_coss-wkg001 + i_coss-wkg002 + i_coss-wkg003 +
                  i_coss-wkg004 + i_coss-wkg005 + i_coss-wkg006 +
                  i_coss-wkg007 + i_coss-wkg008 + i_coss-wkg009 +
                  i_coss-wkg010 + i_coss-wkg011 + i_coss-wkg012 +
                  i_coss-wkg013 + i_coss-wkg014 + i_coss-wkg015 +
                  i_coss-wkg016 .

      ENDCASE.
      COLLECT gt_coss.
      CLEAR gt_coss.
    endloop.
  ENDIF.

*取标准工艺路线值-单值
  select mapl~matnr mapl~werks mapl~plnnr mapl~plnal plpo~vornr
         plpo~vgw01 plpo~vge01 plpo~vgw02 plpo~vge02 plpo~vgw03 plpo~vge03
         plpo~vgw04 plpo~vge04 plpo~vgw05 plpo~vge05 plpo~vgw06 plpo~vge06
         plpo~plnty plpo~zaehl plpo~plnkn crhd~arbpl crhd~objid
         into corresponding fields of table itab1
         from mapl
             inner join plas
             on mapl~plnal = plas~plnal
             and mapl~plnnr = plas~plnnr
             inner join plpo
             on  plpo~plnnr = mapl~plnnr
             and plpo~plnty = mapl~plnty
             and plpo~zaehl = plas~zaehl
             inner join crhd
             on  crhd~objid = plpo~arbid
        where mapl~loekz <> 'x'.

*计算订单数量作业价格
*总的实际消耗费用/总的消耗单位
loop at gt_coss.
    read table gt_list with key objnr = gt_coss-objnr.
    if sy-subrc eq 0.
      zyjg-objnr = gt_coss-objnr.
      zyjg-withdrawn_01 = gt_coss-withdrawn_m / gt_list-ism01.
      zyjg-withdrawn_02 = gt_coss-withdrawn_l / gt_list-ism02.
      append zyjg.
    endif.

endloop.

ENDFORM.                    " select_data

*&---------------------------------------------------------------------*
*&      Form  append_gt_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_gt_result .
*以i_list为模板填充输出内表
loop at i_list.
   gt_result-objnr = i_list-objnr.
   gt_result-aufnr = i_list-aufnr.
   gt_result-budat = i_list-budat.
   gt_result-gmnga = i_list-gmnga.
   gt_result-gmein = i_list-gmein.
   gt_result-ism01 = i_list-ism01.
   gt_result-ile01 = i_list-ile01.
   gt_result-ism02 = i_list-ism02.
   gt_result-ile02 = i_list-ile02.
*用于read的关键字
   select single * from afko where aufnr = gt_result-aufnr.
   gt_result-matnr = afko-plnbez.
   gt_result-plnnr = afko-plnnr.

   append gt_result.
   clear gt_result.
endloop.

*单位的统一
 loop at gt_result.
    read table itab1 with key matnr = gt_result-matnr
                              plnnr = gt_result-plnnr.
    if sy-subrc eq 0.
      if gt_result-ile01 eq 'H'.
        case itab1-vge01.
            when 'H'.
               gt_result-ism01_q = itab1-vgw01 * gt_result-gmnga.
            when 'MIN'.
               gt_result-ism01_q = itab1-vgw01 * gt_result-gmnga / 60.
        endcase.
      elseif gt_result-ile01 eq 'MIN'.
        case itab1-vge01.
            when 'H'.
               gt_result-ism01_q = itab1-vgw01 * gt_result-gmnga * 60.
            when 'MIN'.
               gt_result-ism01_q = itab1-vgw01 * gt_result-gmnga.
        endcase.
      else.
         gt_result-ism01_q = itab1-vgw01 * gt_result-gmnga..
      endif.

      if gt_result-ile02 eq 'H'.
        case itab1-vge02.
            when 'H'.
               gt_result-ism02_q = itab1-vgw02 * gt_result-gmnga.
            when 'MIN'.
               gt_result-ism02_q = itab1-vgw02 * gt_result-gmnga / 60.
        endcase.
      elseif gt_result-ile02 eq 'MIN'.
        case itab1-vge02.
            when 'H'.
               gt_result-ism02_q = itab1-vgw02 * gt_result-gmnga * 60.
            when 'MIN'.
               gt_result-ism02_q = itab1-vgw02 * gt_result-gmnga.
        endcase.
      else.
         gt_result-ism02_q = itab1-vgw02 * gt_result-gmnga..
      endif.
    endif.

    read table zyjg with key objnr = gt_result-objnr.
    if sy-subrc eq 0.
      if gt_result-ile01 = 'MIN'.
*实际机器费用 + 实际人工费用
        gt_result-withcost_m = ( zyjg-withdrawn_01 * gt_result-ism01 ) / 60.
        gt_result-withcost_l = ( zyjg-withdrawn_02 * gt_result-ism02 ) / 60.
*标准机器费用 + 标准人工费用
        gt_result-withcost_q_m = ( zyjg-withdrawn_01 * gt_result-ism01_q ) / 60.
        gt_result-withcost_q_l = ( zyjg-withdrawn_02 * gt_result-ism02_q ) / 60.
      else.
        gt_result-withcost_m = zyjg-withdrawn_01 * gt_result-ism01.
        gt_result-withcost_l = zyjg-withdrawn_02 * gt_result-ism02.
        gt_result-withcost_q_m = zyjg-withdrawn_01 * gt_result-ism01_q.
        gt_result-withcost_q_l = zyjg-withdrawn_02 * gt_result-ism02_q.
      endif.
    endif.

    if gt_result-withcost_m <> 0.
      gt_result-with_ef_m = gt_result-withcost_q_m / gt_result-withcost_m.
    else.
     gt_result-with_ef_m = 0.
    endif.
    if gt_result-withcost_l <> 0.
      gt_result-with_ef_l = gt_result-withcost_q_l / gt_result-withcost_l.
    else.
      gt_result-with_ef_l = 0.
    endif.
*去掉前导零
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gt_result-aufnr
      importing
        output = gt_result-aufnr.

    modify gt_result.
 endloop.

ENDFORM.                    " append_gt_result
***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.
*设置显示格式
  PERFORM f_fieldcat_init.
*
  PERFORM event_top_of_page TABLES gt_event.
*ALV显示报表
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = 'ZCOXY004'
      i_callback_user_command = 'USER_COMMAND'
      it_fieldcat             = gt_fieldcat[]
      it_sort                 = gt_sort[]
      it_events               = gt_event[]
    TABLES
      t_outtab                = gt_result.

************************************************************************
*  设置表输出表头事件
************************************************************************
FORM event_top_of_page  TABLES   ex_gt_event TYPE slis_t_event.
  REFRESH ex_gt_event.
  CLEAR ex_gt_event.
  ex_gt_event-name = slis_ev_top_of_page.
  ex_gt_event-form = 'WRITE_TOP_OF_PAGE'.
  APPEND ex_gt_event.
ENDFORM.                    "event_top_of_page

************************************************************************
*  设置明细表表头                                             *
************************************************************************
FORM write_top_of_page.
  DATA: wa_hline TYPE slis_listheader.

  REFRESH gt_head.
  CLEAR: wa_hline.
  DATA:  l_count TYPE i.

  wa_hline-typ = 'H'.
  wa_hline-info = '成品实际/标准成本明细分析'.
  APPEND wa_hline TO gt_head.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_head.
ENDFORM.                    "write_top_of_page

************************************************************************
* 双击
************************************************************************
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  READ TABLE GT_RESULT INDEX RS_SELFIELD-TABINDEX.
  CHECK SY-SUBRC = 0.
  CASE r_ucomm.
    WHEN '&IC1'.                       "doubleclick
      CASE RS_SELFIELD-SEL_TAB_FIELD.
        WHEN '1-AUFNR'.
        SET PARAMETER ID 'AUF' FIELD GT_RESULT-AUFNR .
          CALL TRANSACTION  'CO03'  AND  SKIP  FIRST  SCREEN.
        CLEAR r_ucomm.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "USER_COMMAND_SUM

***********************************************************************
*设置ALV输出报表的显示字段、排序字段及其相关属性                      *
***********************************************************************
FORM f_fieldcat_init .
  DATA: wa_fieldcat TYPE slis_fieldcat_alv,
        wa_sort TYPE slis_sortinfo_alv.

*显示字段
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'AUFNR'.
  wa_fieldcat-seltext_s =  '工单号'.
  wa_fieldcat-key       =  'X'.
  wa_fieldcat-fix_column = 'X'.
  wa_fieldcat-outputlen = '12'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'BUDAT'.
  wa_fieldcat-seltext_s =  '记账日期'.
  wa_fieldcat-key       =  'X'.
  wa_fieldcat-fix_column = 'X'.
  wa_fieldcat-outputlen = '10'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'GMNGA'.
  wa_fieldcat-seltext_l =  '工单确认数量'.
  wa_fieldcat-fix_column = 'X'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'GMEIN'.
  wa_fieldcat-seltext_l =  '单位'.
  wa_fieldcat-outputlen = '3'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ISM01'.
  wa_fieldcat-seltext_l =  '实际机器工时'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ISM01_Q'.
  wa_fieldcat-seltext_l =  '标准机器工时'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ILE01'.
  wa_fieldcat-seltext_l =  '单位'.
  wa_fieldcat-outputlen = '3'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ISM02'.
  wa_fieldcat-seltext_l =  '实际人工工时'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ISM02_Q'.
  wa_fieldcat-seltext_l =  '标准人工工时'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'ILE02'.
  wa_fieldcat-seltext_l =  '单位'.
  wa_fieldcat-outputlen = '3'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITHCOST_M'.
  wa_fieldcat-seltext_l =  '实际机器成本'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITHCOST_L'.
  wa_fieldcat-seltext_l =  '实际人工成本'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITHCOST_Q_M'.
  wa_fieldcat-seltext_l =  '标准机器成本'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITHCOST_Q_L'.
  wa_fieldcat-seltext_l =  '标准人工成本'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITH_EF_M'.
  wa_fieldcat-seltext_l =  '机器生产效率'.
  APPEND wa_fieldcat TO gt_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname =  'WITH_EF_L'.
  wa_fieldcat-seltext_l =  '人工生产效率'.
  APPEND wa_fieldcat TO gt_fieldcat.
ENDFORM.                    "f_fieldcat_init
