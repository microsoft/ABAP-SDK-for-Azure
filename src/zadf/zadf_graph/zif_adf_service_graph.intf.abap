INTERFACE zif_adf_service_graph
  PUBLIC .
  TYPES: BEGIN OF recipient,
           BEGIN OF emailaddress,
             name    TYPE string,
             address TYPE string,
           END OF emailaddress,
         END OF recipient,


         BEGIN OF datetime,      " Date/time in ISO format
           datetime TYPE string,
           timezone TYPE string,
         END OF datetime,


         BEGIN OF calendar_event,
           id                         TYPE string,
           subject                    TYPE string,
           organizer                  TYPE recipient,
           bodypreview                TYPE string,
           start                      TYPE datetime,
           end                        TYPE datetime,
           reminderminutesbeforestart TYPE integer,
           BEGIN OF body,
             contenttype TYPE string,
             content     TYPE string,
           END OF body,
         END OF calendar_event,

         BEGIN OF user,
           displayname       TYPE string,
           userprincipalname TYPE string,
           givenname         TYPE string,
           jobtitle          TYPE string,
           mail              TYPE string,
           mobilephone       TYPE string,
           officelocation    TYPE string,
           preferredlanguage TYPE string,
           surname           TYPE string,
           id                TYPE string,
         END OF user.

  TYPES: calendar_events TYPE STANDARD TABLE OF calendar_event WITH DEFAULT KEY,
         users           TYPE STANDARD TABLE OF user WITH DEFAULT KEY.


  METHODS create_calendar_event
    IMPORTING
      !iv_calendar_event    TYPE calendar_event
      VALUE(iv_aad_token)   TYPE string
    EXPORTING
      VALUE(ev_http_status) TYPE i
    RETURNING
      VALUE(response)       TYPE  calendar_event
    RAISING
      zcx_adf_service_graph
      zcx_adf_service.


  METHODS get_events
    IMPORTING
      iv_userprincipaltoken     TYPE string
      VALUE(iv_aad_token)       TYPE string
    EXPORTING
      VALUE(ev_http_status)     TYPE i
    RETURNING
      VALUE(rt_calendar_events) TYPE  calendar_events
    RAISING
      zcx_adf_service .

  METHODS get_users
    IMPORTING
      VALUE(iv_aad_token)   TYPE string
    EXPORTING
      VALUE(ev_http_status) TYPE i
    RETURNING
      VALUE(rt_users)       TYPE  users
    RAISING
      zcx_adf_service .


ENDINTERFACE.
