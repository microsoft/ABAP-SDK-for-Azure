
 
<p align="center">
<img width="450" height="100" src="MD%20image/1.png"> 
</p>  

<H1 align="center">
<font size="16"> 
  <b> ABAP SDK Implementation guide for Azure Event hubs </b>
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



### ABAP SDK for Azure
<h2 class="title">
    Contents
</h2>

<div id="TOC">
    <ul>
        <li>
            <a href="#What is Azure Event hub?">1. What is Azure Event hub? </a>
        </li>
        <li>
            <a href="#Prerequisites">2. Prerequisites </a>
        </li>
        <li>
            <a href="#How to setup Event hub in Azure?">3. How to setup Event hub in Azure? </a>
        </li>
        <li>
            <a href="#How to send data from SAP to Azure Event hub?">4. How to send data from SAP to Azure Event hub? </a>
        </li>
    </ul>
</div>

  * [4.1 Creation of RFC destination to Azure Event hub](#heading--1-1)
  * [4.2 STRUST Setup](#heading--1-2)
  * [4.3 Configuration](#heading--1-3)
    * [ZREST_CONFIG ](#heading--1-4)
    * [ZREST_CONF_MISC ](#heading--1-5)
    * [ZADF_CONFIG ](#heading--1-6)
    * [ZADF_EHUB_POLICY ](#heading--1-7)
    * [DEMO Program ](#heading--1-8)
   
<div id="TOC">
    <ul>
        <li>
            <a href="#View sent data in Azure Eventhub">5. View sent data in Azure Eventhub </a>
        </li>
        <li>
        <a href="#ABAP SDK Monitor">6. ABAP SDK Monitor </a>
        </li>
        <li>
            <a href="#Auto re-processing of failed messages">7. Auto re-processing of failed messages </a>
        </li>
    </ul>
</div>



 <div id="What is Azure Event hub?">
    <h2>
        <a href="#TOC">What is Azure Event hub?</a>
    </h2>
    <p>
    </p>
</div>

 **Stream millions of events per second**   

 Azure Event Hubs is a hyper-scale telemetry ingestion service which collects, transforms and stores millions of events. As a distributed streaming platform, it gives you low latency and configurable time retention, which enables you to ingress massive amounts of telemetry into the cloud and read the data from multiple applications using publish-subscribe semantics.  
 For more details on Azure Event hubs, visit [Microsoft Azure Event hub](https://azure.microsoft.com/en-in/services/event-hubs/) 
 
<div id="Prerequisites">
    <h2>
        <a href="#TOC">Prerequisites</a>
    </h2>
    <p>
        Make sure you have installed ABAP SDK for Azure in your SAP system. Refer document ‘ABAP SDK for Azure – Github’ for more details.
    </p>
</div>

Visit <https://github.com/Microsoft/ABAP-SDK-for-Azure>

<div id="How to setup Event hub in Azure?">
    <h2>
        <a href="#TOC">How to setup Event hub in Azure?</a>
    </h2>
</div>
 

 Login to [Microsoft Azure portal](https://ms.portal.azure.com/)
 
 > **Note**:If you do not have an account already. please create a new [Azure account](https://azure.microsoft.com/en-us/). You can start free Once you are logged into portal, chose push button ‘+’ on the left to create a new Azure service.
 
 ![](MD%20image/2.png)
 
 Search for Event hubs and Select 'Event Hubs' with Microsoft as publisher.
 
![](MD%20image/3.png)
 
 
 Now choose the Create push button to create new event hub instance in your subscription.
 
 ![](MD%20image/4.png)
 
 Provide appropriate values in the fields and hit push button Create
 
 ![](MD%20image/5.png)
 
 This should create a new Event Hub. This might take some time, so please wait until it is created successfully. You can check status in **Notifications**.
 
 ![](MD%20image/6.png)
 
 Once it is successfully created, navigate to your Event Hub either by selecting from Navigations pane or from Home screen.
 
 ![](MD%20image/7.png)
 
 You can see multiple options in Azure Event Hubs. We are not discussing each of them in detail in this document.  
 Please visit [Microsoft Azure Website](https://azure.microsoft.com/en-in/services/event-hubs/) for more details.
 
 Once you have entered your Event Hubs Namespace. Now you must create a new **Event Hub** Entity by selecting Event Hub under entities.
 
 ![](MD%20image/8.png)
 
 Create a new Event Hub with appropriate name like ‘SAPEvents’. You can choose any name of your choise and **Create**.
 
 We would post messages from our SAP system to this eventhub.
 
 ![](MD%20image/9.png)
 
 This will now create a new Event Hub in your Event Hubs namespace.
 
 ![](MD%20image/10.png)
 
 Once you are done with creating Event Hub. Now you must create Shared access Policy. By Default, you will have policy **‘RootManageSharedAccessKey’** which will have complete access to Read, Write and Manage. In case you want to restrict users to only Read and Write, create your own policy accordingly.
 
 >**Note** : We will use the SAS key of this policy to access Azure Event Hub from SAP through HTTPS protocol.
 
 ![](MD%20image/11.png)
 
 Once you select the policy, this will display SAS keys that will be used to Authenticate Azure Event Hub. These are secret keys and not to be shared with anyone.
 
 ![](MD%20image/12.png)
 
 With this we are ready with the setup process in Azure portal. We will now configure and code in SAP system to send data from SAP system directly to Azure Event Hub.
 
 <div id="How to send data from SAP to Azure Event hub?">
    <h2>
        <a href="#TOC">How to send data from SAP to Azure Event hub?</a>
    </h2>
</div>
 
<div id="heading--1-1">
    <h3>
        <a href="#TOC">4.1 Creation of RFC destination to Azure Event hub</a>
    </h3>
    <p>
    Go to transaction SM59 in your SAP system and create new RFC destination of type ‘G’. Maintain your Event hubs namespace endpoint in the Target host and Event Hub name in path prefix as shown below.
    </p>
</div>
 
 Target host: <Eventhub Namespace>.servicebus.windows.net

 Port: 443

 Path Prefix: /<Eventhub name>/messages
 
 ![](MD%20image/13.png)
 
 
 Now go to **‘Logon & Security’** tab and choose radio button SSL **‘Active’** and select SSL certificate **‘DFAULT SSL Client (Standard)’**.
 
 ![](MD%20image/14.png)
 
 Do a connection test to make sure it is working. A popup might appear asking for user name and password, select Cancel button. Even if you get 401 Not authorized error, do not worry. you are still good and your RFC destination setup is complete.
 
 ![](MD%20image/15.png)
 
 <div id="heading--1-2">
    <h3>
        <a href="#TOC">4.2 STRUST Setup</a>
    </h3>
    <p>
    We need to import Microsoft’s Certificate and import in STRUST for SSL handshake between SAP system and Azure Eventhub over HTTPS protocol. To download the certificate, in your browser, go to URL with the hostname and path prefix you used for creating RFC destination.
    </p>
</div>

 
 [https://abapsdkkdemo-eh.servicebus.windows.net/sapevents/messages](https://abapsdkkdemo-eh.servicebus.windows.net/sapevents/messages)
 
 ![](MD%20image/16.png)
 
 Click on the Lock symbol you find next to refresh button in Chrome browser and select Certificate to view the certificate used for communication
 
 ![](MD%20image/17.png)
 
 In the certificate, go to Details tab and Choose button **‘Copy to File’** to download the certificate to your local machine. Repeat the process and download all the certificate until root. In this case, you need to download three certificates.
 
 1. Servicebus.windows.net

2.	Microsoft IT TLS CA 5

3.	DigiCert Baltimore Root

![](MD%20image/18.png)

when all the certificates are downloaded, Go to STRUST transaction in your SAP system and Import all these certificates in DFAULT PSE.

>**Note**: We are not going through the process of Importing certificates in STRUST in this document. It is straight forward, and your BASIS team can help you to do this activity.

<div id="heading--1-3">
    <h3>
        <a href="#TOC">4.3 Configuration</a>
    </h3>
    <p>
   ABAP SDK has following main configuration tables and they need to be maintained. We will create a new Interface ID to establish connection between SAP system and target Azure Event Hub. A new Interface ID needs to be created for each Event Hub namespace.
    </p>
</div>


>**Note**: Currently all these Configurations tables must be maintained manually using SM30. We are developing a new Graphical Interface to simplify the configuration steps. This will be released with next versions. Keeping looking for Updates.

**ZREST_CONFIG** – Master Table for Interface ID Maintenance. You must define a new Interface name and maintain the RFC destination that was created for target Event hub.

**ZREST_CONF_MISC** – This is an Interface Miscellaneous table which contains information on Alerts and re-processing of failed messages automatically.

**ZADF_CONFIG** – This is an Interface extension table. This stores data that is more specific to Azure Services like SAS keys, AAD secrets and processing Method.

**ZADF_EHUB_POLICY** – This is Event hub specific table that stores Azure Event hub policy details that shall be used during communication.

<div id="heading--1-4">
    <h3>
        <a href="#TOC">ZREST_CONFIG</a>
    </h3>
    <p>
   Create a new Interface ID like ‘DEMO_EHUB’ and Maintain the RFC destination you created earlier.
    </p>
</div>
 

![](MD%20image/19.png)

<div id="heading--1-5">
    <h3>
        <a href="#TOC">ZREST_CONF_MISC</a>
    </h3>
    <p>
   Create an entry in table ‘ZREST_CONF_MISC’ for the above interface Id ‘DEMO_EHUB’.
    </p>
</div>

Details of configuration:

•	METHOD is ‘POST’.

•	MAX_RETRY is number of retry in case of service failure.

•	EMAIL_ID is the email id for sending alerts.

•	MAIL_BODY_TXT is Text Id to be maintained for the mail content.

•	RETRY_METHOD is type of retrial (Regular ‘0’ or exponential ‘1’)


![](MD%20image/20.png)

<div id="heading--1-6">
    <h3>
        <a href="#TOC">ZADF_CONFIG</a>
    </h3>
    <p>
   Create an entry in table ‘ZADF_CONFIG’ for the above interface Id ‘DEMO_EHUB’.
    </p>
</div>

Details of configuration:

•	INTERFACE_TYPE is ‘EVENTHUB.

•	SAS_KEY is the shared access key. This you can retrieve from      Azure portal. Check page 10 of this document. Primary key is      the SAS key. Every time you generate a new SAS key in portal,     you need to change in this config table.

•	URI is left blank. This may be required for future versions.

•	SERVICE_TYPE can be synchronous(S) or asynchronous(A)

•	IS_TRY is a reprocessing flag, maintain as blank or ‘X’. it can be configured for reprocessing in case of failure of services.


![](MD%20image/21.png)

<div id="heading--1-7">
    <h3>
        <a href="#TOC">ZADF_EHUB_POLICY</a>
    </h3>
    <p>
  You need to maintain the policy name that you have created in Azure portal in this config table. This is required to generate token for authentication with Azure Event hubs. Refer Page 10 of this document to get the policy name. Azure creates default policy ‘RootManageSharedAccessKey’, but if you had created your own policy, then maintain that policy name here.
    </p>
</div>


![](MD%20image/22.png)

<div id="heading--1-8">
    <h3>
        <a href="#TOC">DEMO Program</a>
    </h3>
    <p>
 Please refer to DEMO program ‘ZADF_DEMO_AZURE_EVENTHUB’ to send sample data from your SAP system to Azure Event hub.
    </p>
</div>

<div id="View sent data in Azure Eventhub">
    <h2>
        <a href="#TOC">View sent data in Azure Eventhub</a>
    </h2>
   
</div>

To view data in Azure Event hub, Install [Service bus explorer](https://docs.microsoft.com/en-us/archive/blogs/paolos/service-bus-explorer-2-6-now-available)

You can get more details on How to setup Service Bus explorer in [MSDN Blog](https://docs.microsoft.com/en-us/samples/browse/?redirectedfrom=MSDN-samples)

<div id="ABAP SDK Monitor">
    <h2>
        <a href="#TOC">ABAP SDK Monitor </a>
    </h2>
    <p>
We have provided an Interface Monitor (Transaction ZREST_UTIL), using this monitor you can view history of all the messages that were posted to Azure Services. Incase you have a scheduled a background job to post messages to Azure, you can view the statuses of the messages in this monitor. This Monitor can be used for troubleshooting and re-processing of the message as well.
    </p>
</div>



Go to transaction ZREST_UTIL and provide your Interface ID in the selection screen and execute to view all the messages

![](MD%20image/23.png)

In this monitor, you can view the status of the HTTPs message and its headers, response, payload and so on. In case of errors, you can also re-process the message from this tool.

![](MD%20image/24.png)

<div id="Auto re-processing of failed messages">
    <h2>
        <a href="#TOC">Auto re-processing of failed messages</a>
    </h2>
     <p>
     For auto-processing of messages in case of failures, you must schedule a background job for program ‘ZREST_SCHEDULER’ as a pre-requisite
     <p>
</div>


























[image_ref_ui8et78e]: data:image/jpeg;base64,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
 