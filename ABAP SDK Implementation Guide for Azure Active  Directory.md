

<p align="center">
<img width="450" height="100" src="MD%20image/1.png"> 
</p>

<H1 align="center">
<font size="16"> 
  <b> Implementation guide for Azure Active Directory </b>
</font> 
</H1>

<p align="center"

<https://github.com/Microsoft/ABAP-SDK-for-Azure>
</p>

<p align="right">
Author: Microsoft SAP Team  
</p>
<p align="right">
Version: 1.0
</p>

<h2 class="title">
    Contents
</h2>

<div id="TOC">
    <ul>
        <li>
            <a href="#What is Azure Active Directory?">1. What is Azure Active Directory? </a>
        </li>
        <li>
            <a href="#Prerequisites">2. Prerequisites </a>
        </li>
        <li>
            <a href="#How to setup Azure Active Directory in Azure?">3. How to setup Azure Active Directory in Azure? </a>
        </li>
        <li>
            <a href="#Generate keys for your application">4. Generate keys for your application </a>
        </li>
        <li>
            <a href="#Steps to use AAD authentication from SAP using ABAP SDK for Azure">5. Steps to use AAD authentication from SAP using ABAP SDK for Azure </a>
        </li>
    </ul>
</div>

* [5.1 Creation of RFC destination to Azure Active Directory](#heading--1-1)
* [5.2 STRUST Setup](#heading--1-2)
* [5.3 Configuration](#heading--1-3)
    * [ZREST_CONFIG ](#heading--1-4)
    * [ZREST_CONF_MISC ](#heading--1-5)
    * [ZADF_CONFIG ](#heading--1-6)
   
   
<div id="TOC">
    <ul>
        <li>
            <a href="#DEMO Program">6. DEMO Program </a>
        </li>
        <li>
        <a href="#ABAP SDK Monitor">7. ABAP SDK Monitor </a>
        </li>
        <li>
            <a href="#Auto re-processing of failed messages">8. Auto re-processing of failed messages </a>
        </li>
    </ul>
</div>

<div id="What is Azure Active Directory?">
    <h2>
        <a href="#TOC">What is Azure Active Directory?</a>
    </h2>
    <p>
    </p>
</div>

Azure Active Directory (Azure AD) provides an easy way for businesses to manage identity and access, both in the cloud and on-premises. Your users can use the same work or school account for single sign-on to any cloud and on- premises web application. Users can use their favorite devices, including iOS, Mac OS X, Android, and Windows. An Organization can protect sensitive data and applications both on-premises and in the cloud with integrated multi- factor authentication ensuring secure local and remote access. Azure AD extends your on-premises directories so that information workers can use a single organizational account to securely and consistently access their corporate resources. Azure AD also offers comprehensive reports, analytics, and self-service capabilities to reduce costs and enhance security. The Azure AD SLA ensures that your business always runs smoothly and can be scaled to enterprise levels.

For more details on Azure Active directory, visit [Microsoft Azure
Active Directory](https://docs.microsoft.com/en-us/previous-versions/azure/azure-services/mt168838(v%3Dazure.100))

<div id="Prerequisites">
    <h2>
        <a href="#TOC">Prerequisites</a>
    </h2>
    <p>
        Make sure you have installed ABAP SDK for Azure in your SAP system. Refer document ‘ABAP SDK for Azure –
        GitHub’ for more details. 
    </p>
</div>

Visit <https://github.com/Microsoft/ABAP-SDK-for-Azure>

<div id="How to setup Azure Active Directory in Azure?">
    <h2>
        <a href="#TOC">How to setup Azure Active Directory in Azure?</a>
    </h2>
</div>

Login to [Microsoft Azure portal](https://ms.portal.azure.com/#home)
> **Note**:If you do not have an account already. please create a new [Azure account](https://azure.microsoft.com/en-us/). You can start free Once you are logged into portal, ,  go to all services and search for Azure Active Directory and Select
“Azure Active Directory” as shown below.

![](MD%20image/32.png)

Create a new tenant for your organization in case it hasn’t been created.
[https://docs.microsoft.com/en-us/azure/active-directory/fundamentals/active-directory-access-create-new-tenant](https://docs.microsoft.com/en-us/azure/active-directory/fundamentals/active-directory-access-create-new-tenant)

Click on ‘App Registrations’ on left side menu as shown below and click the button ‘New application Registration’

![](MD%20image/33.png)

Specify details of your Application and press ‘create’ button.

![](MD%20image/34.png)

Application is created successfully.

![](MD%20image/35.png)

<div id="Generate keys for your application">
    <h2>
        <a href="#TOC">Generate keys for your application</a>
    </h2>
</div>

1.	Once your application is created, go to your application by clicking on it.
Copy the application id which will be required in the implementation of code in ABAP SDK. This application id is client Id.

![](MD%20image/36.png)

Click on ‘Settings’ in the above screen, go to the ‘Required Permissions’ under API Access as shown below.

![](MD%20image/37.png)

2.	Then click on ‘Add’ button as shown in the below screen.

![](MD%20image/38.png)

3.	Under ‘Add API access’ section click on ‘select an API ‘and Search Azure Key Vault and Select the same as shown below.

> **Note**: In this step, we have chosen Azure Key vault as an external application as an example.
In real world scenario, you need to choose your existing API which you want to access with AAD token for authentication.

![](MD%20image/39.png)

4.	Under Add API access section, click on ‘Select permissions’ and enabled the checkbox for ‘Delegated Permissions’ as shown below.

![](MD%20image/40.png)

Then click on ‘Done’ button to complete the Required permissions activity.

![](MD%20image/41.png)

5.	Click on ‘Grant permissions’ and press ‘Yes’ button as shown below.

![](MD%20image/42.png)

6.	Click on ‘Keys’ section and provide key description and expiry Duration in the below screen. Under
‘EXPIRES’ dropdown list, you can select the expiry duration.

![](MD%20image/43.png)

Here key description is provided as client_secret and it expires on 8/2/2019.

![](MD%20image/44.png)

7.	Click on ‘Save’ button to generate secret key. 
**Copy this key and it will be used in ABAP SDK implementation. Please remember you won’t be able to retrieve this key once you leave the screen.**

![](MD%20image/45.png)

<div id="Steps to use AAD authentication from SAP using ABAP SDK for Azure">
    <h2>
        <a href="#TOC">Steps to use AAD authentication from SAP using ABAP SDK for Azure</a>
    </h2>
</div>

<div id="heading--1-1">
    <h3>
        <a href="#TOC">5.1 Creation of RFC destination to Azure Active Directory</a>
    </h3>
    <p>
    Go to transaction SM59 in your SAP system and create new RFC destination of type ‘G’. Maintain your Azure Active directory endpoint in the Target host and Event Hub name in path prefix for authorization token as shown below.
    </p>
</div>

Target host: **login.microsoftonline.com**

Port: 443

Path Prefix: **/InputTenantID/oauth2/token**

For Tenant ID details and creating a new tenant id in Azure Active Directory, please refer this document section
‘How to setup Azure Active Directory in Azure?’

![](MD%20image/46.png)

Now go to ‘Logon & Security’ tab and choose radio button SSL ‘Active’ and select SSL certificate ‘DFAULT SSL Client (Standard)’.

![](MD%20image/47.png)

Do a connection test to make sure it is working. RFC destination is working.

![](MD%20image/48.png)

<div id="heading--1-2">
    <h3>
        <a href="#TOC">5.2 STRUST Setup</a>
    </h3>
    <p>
    We need to import Microsoft’s Certificate and import in STRUST for SSL handshake between SAP system and Azure Active Directory over HTTPS protocol. To download the certificate, in your browser, go to URL with the hostname and path prefix you used for creating RFC destination.
    </p>
</div>

Target host: **login.microsoftonline.com**

Path Prefix: **/InputTenantID/oauth2/token**


![](MD%20image/49.png)

Click on the Lock symbol you find next to refresh button in Chrome browser and select Certificate to view the certificate used for communication

![](MD%20image/50.png)

In the certificate, go to Details tab and Choose button ‘Copy to File’ to download the certificate to your local machine. Repeat the process and download all the certificate until root. In this case, you need to download two certificates.

1.	Microsoft IT TLS CA 4
2.	Baltimore Cyber Trust Root

![](MD%20image/51.png)

when all the certificates are downloaded, Go to STRUST transaction in your SAP system and Import all these certificates in DFAULT PSE.

> **Note:** We are not going through the process of Importing certificates in STRUST in this document. It is straight forward, and your BASIS team can help you to do this activity.

<div id="heading--1-3">
    <h3>
        <a href="#TOC">5.3 Configuration</a>
    </h3>
    <p>
   ABAP SDK has following main configuration tables and they need to be maintained. We will create a new Interface ID to establish connection between SAP system and target Azure Active Directory (AAD). A new Interface ID needs to be created for each AAD namespace.
    </p>
</div>

**ZREST_CONFIG** – Master Table for Interface ID Maintenance. You must define a new Interface name and maintain the RFC destination that was created for target Event hub.

**ZREST_CONF_MISC** – This is an Interface Miscellaneous table which contains information on Alerts and re-processing of failed messages automatically.

**ZADF_CONFIG** – This is an Interface extension table. This stores data that is more specific to Azure Services like SAS keys, AAD secrets and processing Method.

<div id="heading--1-4">
    <h3>
        <a href="#TOC">ZREST_CONFIG</a>
    </h3>
    <p>
   Create a new Interface ID like ‘DEMO_AAD’ and Maintain the RFC destination you created earlier.
    </p>
</div>

![](MD%20image/52.png)

<div id="heading--1-5">
    <h3>
        <a href="#TOC">ZREST_CONF_MISC</a>
    </h3>
    <p>
   Create an entry in table ‘ZREST_CONF_MISC’ for the above interface Id ‘DEMO_AAD’.
    </p>
</div>

Details of configuration:

•	METHOD is ‘POST’.

•	MAX_RETRY is number of retry in case of service failure.

•	EMAIL_ID is the email id for sending alerts.

•	MAIL_BODY_TXT is Text Id to be maintained for the mail content.

•	RETRY_METHOD is type of retrial (Regular ‘0’ or exponential ‘1’)

![](MD%20image/53.png)

<div id="heading--1-6">
    <h3>
        <a href="#TOC">ZADF_CONFIG</a>
    </h3>
    <p>
   Create an entry in table ‘ZADF_CONFIG’ for the above interface Id ‘DEMO_AAD’.
    </p>
</div>

Details of configuration:

•	INTERFACE_TYPE is ‘Azure Active Directory’.

•	SAS_KEY is the shared access key. This is the key which   generated in AAD under section ‘Generate keys for your application’ Step 7 (refer Page 8). You need to change this key in this config table whenever key is changed in Azure.

•	URI is left blank. This may be required for future versions.

•	SERVICE_TYPE can be synchronous(S) or asynchronous(A)

•	IS_TRY is a reprocessing flag, maintain as blank or ‘X’. it can be configured for reprocessing in case of failure of services.

>**Note**:This field can be utilized in our future release to control the reprocessing based on value of X. Presently it's should be enabled as blank.

![](MD%20image/54.png)

<div id="DEMO Program">
    <h3>
        <a href="#TOC">DEMO Program</a>
    </h3>
    <p>
 Please refer to DEMO program ‘ZADF_DEMO_AZURE_AAD’to generate AAD token for AAD based authentication. 
    </p>
</div>

Please note that in the demo program, application id generated in step 1 of **Generate keys for your application** is used as client id. Please check above section to generate keys.

![](MD%20image/55.png)

<div id="ABAP SDK Monitor">
    <h2>
        <a href="#TOC">ABAP SDK Monitor </a>
    </h2>
    <p>
We have provided an Interface Monitor (Transaction ZREST_UTIL), using this monitor you can view history of all the messages that were posted to Azure Services. Incase you have a scheduled a background job to post messages to Azure, you can view the statuses of the messages in this monitor. This Monitor can be used for troubleshooting and re-processing of the message as well.
    </p>
</div>

Go to transaction ZREST_UTIL and provide your Interface ID in the selection screen and execute to view all the messages

![](MD%20image/56.png)

In this monitor, you can view the status of the HTTPs message and its headers, response, payload and so on. In case of errors, you can also re-process the message from this tool.

![](MD%20image/57.png)

<div id="Auto re-processing of failed messages">
    <h2>
        <a href="#TOC">Auto re-processing of failed messages</a>
    </h2>
     <p>
     For auto-processing of messages in case of failures, you must schedule a background job for program ‘ZREST_SCHEDULER’ as a pre-requisite
     <p>
</div>



















