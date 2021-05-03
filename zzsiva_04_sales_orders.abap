*&---------------------------------------------------------------------*
*& Report ZZSIVA_04_SALES_ORDERS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzsiva_04_sales_orders.
TABLES : vbak,vbap.

select-options : s_vbeln FOR vbak-vbeln,
                 s_matnr FOR vbap-matnr.


PARAMETERS :
             p_erdat TYPE erdat,
             p_ernam TYPE ernam.


TYPES : BEGIN OF ty_order,
          vbeln  TYPE vbeln_va,
          ernam  TYPE ernam,
          erdat  TYPE erdat,
          posnr  TYPE posnr,
          matnr  TYPE matnr,
          kwmeng TYPE kwmeng,
          netwr  TYPE netwr,
        END OF ty_order.
DATA : it_order TYPE TABLE OF ty_order,
       wa_order TYPE ty_order.


SELECT vbak~vbeln vbak~ernam  vbak~erdat  vbap~posnr  vbap~matnr  vbap~kwmeng vbap~netwr
  FROM vbak AS vbak INNER JOIN vbap AS vbap
  ON vbak~vbeln = vbap~vbeln
  INTO TABLE it_order
  WHERE vbak~vbeln in  s_vbeln
  or vbap~matnr in s_matnr.


write  :/ 'Order Num' ,'Created by','Created On''Item','Material','Quantity','Value'.

LOOP AT it_order INTO wa_order.

    WRITE :/ wa_order-vbeln,  wa_order-ernam ,  wa_order-erdat,  wa_order-posnr ,  wa_order-matnr,
       wa_order-kwmeng,  wa_order-netwr.
ENDLOOP.
