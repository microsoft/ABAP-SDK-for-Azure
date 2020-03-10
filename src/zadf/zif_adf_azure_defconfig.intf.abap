INTERFACE zif_adf_azure_defconfig
  PUBLIC .

  CONSTANTS gc_service_eventhub   TYPE zazure_dest VALUE 'EVENTHUB'. "#EC NOTEXT
  CONSTANTS gc_service_blob       TYPE zazure_dest VALUE 'BLOB'. "#EC NOTEXT
  CONSTANTS gc_service_docdb      TYPE zazure_dest VALUE 'DOCUMENTDB'. "#EC NOTEXT
  CONSTANTS gc_service_servicebus TYPE zazure_dest VALUE 'SERVICEBUS'. "#EC NOTEXT
  CONSTANTS gc_service_aad        TYPE zazure_dest VALUE 'AAD'. "#EC NOTEXT
  CONSTANTS gc_service_keyvault   TYPE zazure_dest VALUE 'KV'. "#EC NOTEXT
  CONSTANTS gc_service_cosmosdb   TYPE zazure_dest VALUE 'COSMOSDB'. "#EC NOTEXT

  METHODS get_classname
    IMPORTING i_interface_type   TYPE zazure_dest
    RETURNING VALUE(r_classname) TYPE seoclname
    RAISING   zcx_adf_service.

ENDINTERFACE.
