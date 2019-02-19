Graph Support

Support to create and read calendar events in Graph API

#Prerequisits
* Azure Active Directoy Application with permission to Read and write calendars in all mailboxes

# Demo Program
The demo program ZADF_DEMO_AZURE_GRAPH requires the following intefaces to be setup together with corresponding RFC destinations

## ZADF_CONFIG

| Interface Name | Interface Type |
|----------------|----------------|
| ADB2CGR_GE     | GRAPH          |
| ADB2CGR_PO     | GRAPH          |
| ADB2CTOKEN     | AAD            |

## ZREST_CONFIG
| Interface Name | Interface Type |
|----------------|----------------|
| ADB2CGR_GE     | MS_AZURE_GRAPH          |
| ADB2CGR_PO     | MS_AZURE_GRAPH         |
| ADB2CTOKEN     | MS_AZURE_OAUTH_LOGIN |

## ZADF_REST_MISC
| Interface ID | Method |
|--------------|--------|
| ADB2CGR_GE   | GET    |
| ADB2CGR_PO   | POST   |
| ADB2CTOKEN   | POST   |

