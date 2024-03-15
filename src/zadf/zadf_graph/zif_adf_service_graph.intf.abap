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


  TYPES:
    "! Type to represent a SharePoint Site
    BEGIN OF SharePointSite,
      createdDateTime      TYPE string,
      description          TYPE string,
      id                   TYPE string,
      lastModifiedDateTime TYPE string,
      name                 TYPE string,
      webUrl               TYPE string,
      displayName          TYPE string,
    END OF SharePointSite.

  TYPES:
    "! Type represents a single SharePoint List
    BEGIN OF SharePointList,
      createdDateTime      TYPE string,
      description          TYPE string,
      eTag                 TYPE string,
      id                   TYPE string,
      lastModifiedDateTime TYPE string,
      name                 TYPE string,
      webUrl               TYPE string,
      displayName          TYPE string,
    END OF SharePointList.

  TYPES:
      "! Table with SharePoint Lists
    SharePointLists TYPE STANDARD TABLE OF SharePointList WITH DEFAULT KEY.

  TYPES:
    "! Type representing a single List Item. Fields are available as JSON String
    BEGIN OF SharePointListItem,
      createdDateTime      TYPE string,
      description          TYPE string,
      eTag                 TYPE string,
      id                   TYPE string,
      lastModifiedDateTime TYPE string,
      webUrl               TYPE string,
      fields               TYPE /ui2/cl_json=>json,
    END OF SharePointListItem.

  TYPES:
      "! Table containing SharePoint List items
      SharePointListItems TYPE STANDARD TABLE OF SharePointListItem WITH DEFAULT KEY.

  TYPES:
    "! Type representing a single SharePoint Drive
    BEGIN OF SharePointDrive,
      createdDateTime      TYPE string,
      description          TYPE string,
      id                   TYPE string,
      lastModifiedDateTime TYPE string,
      name                 TYPE string,
      webUrl               TYPE string,
      driveType            TYPE string,
    END OF SharePointDrive.

  TYPES:
      "! Table containing SharePoint Drives
      SharePointDrives TYPE STANDARD TABLE OF SharePointDrive WITH DEFAULT KEY.

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

  "! <p class="shorttext synchronized" lang="en">Get information of a SharePoint Site</p>
  "!
  "! @parameter iv_aad_token | <p class="shorttext synchronized" lang="en">Access Token V2.0</p>
  "! @parameter iv_hostname | <p class="shorttext synchronized" lang="en">Hostname</p>
  "! @parameter iv_site | <p class="shorttext synchronized" lang="en">Name of Site</p>
  "! @parameter ev_http_status | <p class="shorttext synchronized" lang="en">response http status</p>
  "! @parameter rt_sp_site | <p class="shorttext synchronized" lang="en">Information about SharePoint Site</p>
  METHODS get_SPSite_by_Name
    IMPORTING
      VALUE(iv_aad_token)   TYPE string
      VALUE(iv_hostname)    TYPE string
      VALUE(iv_site)        TYPE string
    EXPORTING
      VALUE(ev_http_status) TYPE i
    RETURNING
      VALUE(rt_sp_site)     TYPE sharepointsite .

  "! <p class="shorttext synchronized" lang="en">Get all lists of a SharePoint Site</p>
  "!
  "! @parameter iv_aad_token | <p class="shorttext synchronized" lang="en">Access Token V2.0</p>
  "! @parameter iv_site_id | <p class="shorttext synchronized" lang="en">ID of SharePoint Site</p>
  "! @parameter ev_http_status | <p class="shorttext synchronized" lang="en">response http status</p>
  "! @parameter rt_sp_lists | <p class="shorttext synchronized" lang="en">Table containing metadata of all Lists</p>
  METHODS get_SPLists
    IMPORTING
              VALUE(iv_aad_token)   TYPE string
              VALUE(iv_site_id)     TYPE string
    EXPORTING
              VALUE(ev_http_status) TYPE i
    RETURNING VALUE(rt_sp_lists)    TYPE sharepointlists.

  "! <p class="shorttext synchronized" lang="en">Get all items of a SharePoint List</p>
  "!
  "! @parameter iv_aad_token | <p class="shorttext synchronized" lang="en">Access Token V2.0</p>
  "! @parameter iv_site_id | <p class="shorttext synchronized" lang="en">ID of SharePointSite</p>
  "! @parameter iv_list_id | <p class="shorttext synchronized" lang="en">ID of SharePoint List</p>
  "! @parameter ev_http_status | <p class="shorttext synchronized" lang="en">response http status</p>
  "! @parameter rt_sp_items | <p class="shorttext synchronized" lang="en">Table containing all list items</p>
  METHODS get_SPListItems
    IMPORTING
              VALUE(iv_aad_token)   TYPE string
              VALUE(iv_site_id)     TYPE string
              VALUE(iv_list_id)     TYPE string
    EXPORTING
              VALUE(ev_http_status) TYPE i
    RETURNING VALUE(rt_sp_items)    TYPE sharepointlistItems.

  "! <p class="shorttext synchronized" lang="en">Get all document libraries of a SharePoint Site</p>
  "!
  "! @parameter iv_aad_token | <p class="shorttext synchronized" lang="en">Access Token V2.0</p>
  "! @parameter iv_site_id | <p class="shorttext synchronized" lang="en">ID of SharePoint Site</p>
  "! @parameter ev_http_status | <p class="shorttext synchronized" lang="en">response http status</p>
  "! @parameter rt_drives | <p class="shorttext synchronized" lang="en">Table containing all document libraries aka drives</p>
  METHODS get_SPDrives
    IMPORTING
              VALUE(iv_aad_token)   TYPE string
              VALUE(iv_site_id)     TYPE string
    EXPORTING
              VALUE(ev_http_status) TYPE i
    RETURNING VALUE(rt_drives)    TYPE sharepointdrives.

  "! <p class="shorttext synchronized" lang="en">Upload file to SharePoint</p>
  "!
  "! @parameter iv_aad_token | <p class="shorttext synchronized" lang="en">Access Token V2.0</p>
  "! @parameter iv_site_id | <p class="shorttext synchronized" lang="en">ID of SharePoint Site</p>
  "! @parameter iv_drive_id | <p class="shorttext synchronized" lang="en">ID of drive (document library)</p>
  "! @parameter iv_filename | <p class="shorttext synchronized" lang="en">Filename (with extension) to upload</p>
  "! @parameter iv_binary | <p class="shorttext synchronized" lang="en">Binary data of file</p>
  "! @parameter ev_http_status | <p class="shorttext synchronized" lang="en">response http status</p>
  "! @parameter ev_response | <p class="shorttext synchronized" lang="en">Response message as String</p>
  METHODS upload_file_to_SharePoint
    IMPORTING
              VALUE(iv_aad_token)   TYPE string
              VALUE(iv_site_id)     TYPE string
              VALUE(iv_drive_id)    TYPE string
              VALUE(iv_filename)    TYPE string
              VALUE(iv_binary)      TYPE xstring
    EXPORTING
              VALUE(ev_http_status) TYPE i
    RETURNING VALUE(ev_response)    TYPE string.
ENDINTERFACE.
