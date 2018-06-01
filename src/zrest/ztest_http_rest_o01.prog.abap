*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STATUS_0100'.

  SET TITLEBAR 'TITLE_0100'.

*initialization :

  IF gw_trace_payload IS INITIAL .

    gw_trace_payload-name = 'Microsoft.ApplicationInsights.Dev.20254ebeb921479a8c16d2cdbe79f8f5.Event' .
    gw_trace_payload-time = '2015-10-22T17:10:00.1184637-05:00'.
    gw_trace_payload-ikey = '20254ebe-b921-479a-8c16-d2cdbe79f8f5'.

    gw_trace_payload-data-basetype = 'EventData' .
    gw_trace_payload-data-basedata-ver = '2'.
    gw_trace_payload-data-basedata-name = 'Order Successfully Created'.
    gw_trace_payload-data-basedata-properties-functionmodule = 'Z_ORDER_CREATE' .
    gw_trace_payload-data-basedata-properties-developermode = 'true' .
    gw_trace_payload-data-basedata-properties-scenario = 'order Creation' .
    gw_trace_payload-data-basedata-properties-totalsaptime = '30' .

    gv_url = 'http://dc.services.visualstudio.com' .

    gv_uri = '/v2/track/'.

  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT
