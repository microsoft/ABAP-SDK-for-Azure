

<p align="center">
<img width="450" height="100" src="MD%20image/1.png"> 
</p>

<H1 align="center">
<font size="16"> 
  <b> ABAP SDK for Azure </b>
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
            <a href="#What is Azure?">1. What is Azure? </a>
        </li>
        <li>
            <a href="#What is ABAP SDK for Azure?">2. What is ABAP SDK for Azure? </a>
        </li>
    </ul>
</div>


   * [Why do you need ABAP SDK for Azure?](#heading--1-4)
   * [Business Use Cases](#heading--1-5)
   * [Architecture](#heading--1-6)
   * [Installation](#heading--1-7)
   * [Implementation Guides](#heading--1-8)
   * [Dos and Don’ts on usage of ABAP SDK](#heading--1-9)
   * [For more information](#heading--1-2)
   * [Contact and Support](#heading--1-1)
   

 <div id="1.What is Azure?">
    <h2>
        <a href="#TOC">1.What is Azure?</a>
    </h2>
    <p>
    </p>
</div>

Microsoft Azure is an ever-expanding set of cloud services to help your organization meet your business challenges.
It’s the freedom to build, manage, and deploy applications on a massive, global network using your favourite tools
and frameworks.

By hosting your applications in Azure, you can start small and easily scale your application as your customer demand
grows. Azure also offers the reliability that’s needed for high-availability applications, even including failover
between different regions. The [Azure portal](https://ms.portal.azure.com/#home) lets you easily manage all your Azure services. You can also manage
your services programmatically by using service-specific APIs and templates provide by Azure.

For more details on Azure, visit [Microsoft Azure](https://azure.microsoft.com/en-us/)

<div id="What is ABAP SDK for Azure?">
    <h2>
        <a href="#TOC">2. What is ABAP SDK for Azure?</a>
    </h2>
    <p>
    Many large enterprise customers use SAP to run their business. There is growing need to integrate SAP applications to cloud. With Azure gaining popularity with enterprise customers, integrating SAP to Azure natively will be a basic and critical requirement for Azure adoption in enterprises.
    </p>
</div>
Azure prospective customers with SAP installations require in depth knowledge of security and Integration patterns
of the respective Azure services, making it complex for enterprises to integrate SAP with Azure services. To
overcome this challenge,  

**Microsoft has created ABAP SDK for Azure to simplify SAP integration with Azure.**

This SDK is the built using SAP’s proprietary language ABAP and its supporting configuration tools. The framework
enables the programmer to integrate with Azure services by abstracting the complexity like authentication, shared
access tokens, security etc

**ABAP SDK for Azure helps enterprises to accelerate their Azure cloud adoption by making SAP functionality and data
securely available on cloud for partners and other custom applications** 

<div id="heading--1-4">
    <h3>
        <a href="#TOC">Why do you need ABAP SDK for Azure?</a>
    </h3>
    <p>
   SAP plays a pivotal role in all the companies that have implemented SAP to run their business, and with increased
   focus on cloud to reduce infrastructure costs and capital expenditures, companies have a strong requirement to
   integrate their SAP data to cloud products to run their business more efficiently.
    </p>
</div>
  As more and more clouds products are evolving, there is also a need for ETL tools to evolve to support data integration with these cloud products. 

  **ABAP SDK for Azure was designed and developed to help all the SAP customers to communicate or integrate with the latest Azure Products and services directly.**
  It helps customers to run their business more efficiently and at the same time making sure companies are not left behind with outdated technologies which cannot scale to new generation cloud computing technologies.
  
  As of today, there are no products that exist which can integrate SAP to Azure cloud services except for SQL storage
  and Blob. In the absence of native integration, customers are using middleware systems for this purpose. These middleware systems always come with extra development effort, more coordination, additional costs, reduced performance, high latency round trips and so on. 
  
  ABAP SDK for Azure provides a platform for all SAP developers to easily integrate to Azure cloud services directly without above mentioned pain points.
  
  The ABAP SDK for Azure libraries are built in SAP’s programming language ABAP, and these libraries can easily be consumed in any ABAP based tools and products.
   **The SDK also follows the industry standards to ensure the safety of secrets keys that are provided by Azure cloud services.** These secrets are either stored in SAP’s secure store or encrypted before storing into database.
   
   SDK also provides detailed execution logs and has capabilities like,
   
  *  Automatic reprocessing of payloads
  * Dashboard for monitoring and troubleshooting
  * Email notifications in case of errors
  * Throttling and Queuing for Transactional payloads
  
  In current version, ABAP SDK for Azure provides ABAP libraries for the below mentioned Azure Services. Each of these Azure services can be used to address various IT and business requirements in your landscape. Please go through individual Azure service documentation to understand it’s purpose and use cases in detail.
  
 
 * [SQL DB](https://azure.microsoft.com/en-in/services/sql-database/)
 * [Blob Storage](https://azure.microsoft.com/en-in/services/storage/blobs/)
 * [Event Hubs](https://azure.microsoft.com/en-in/services/event-hubs/)
 * [Service Bus](https://azure.microsoft.com/en-in/services/service-bus/)
 * [Active Directory](https://azure.microsoft.com/en-in/services/active-directory/)
 * [Key Vault](https://azure.microsoft.com/en-in/services/key-vault/)
<div id="heading--1-5">
    <h3>
        <a href="#TOC">Business Use Cases?</a>
    </h3>
    <p>
    </p>
</div>

#### Real time notifications to integrated systems

All the systems that are integrated with SAP do not have any visibility or status on the transactions that were being
processed by SAP. With Azure SDK for ABAP, near real time notifications can be sent to Azure Cloud product like
[Eventhub](https://docs.microsoft.com/en-us/azure/event-hubs/event-hubs-about) where all the interested parties can subscribe to receive the status updates of SAP system. This provides
more transparency to the business and all the involved parties
within IT

#### Avoid the Middleware systems for Data Integration
There are no tools that can integrate SAP to Azure services directly, so customers started had to use middleware applications like .NET Web API’s, Web services, etc. to integrate SAP to Azure Cloud. Now with Azure SDK for ABAP, Developers can directly integrate SAP to Azure products. 

#### Easy Integration with ETL tools for data migration 
SAP has released a product named SLT for real time data replication from SAP to other databases. But, this product
is supported only for SQL databases and does not support Azure cloud products. Azure SDK for ABAP can be integrated with SLT product for supporting the real-time data replication to Azure cloud products.

#### Delta extractions from SAP to Azure Services in near real time 
SAP transactions raise many events during the end to end business flow, these events can be listened for real time status on the business process flow. Azure SDK for ABAP can easily be integrated into these event listeners and transfer the real time business process data to Azure cloud services for more Business and Application Insights. 
>**Note**: These are few sample business use cases given as example. There could be many more use cases depending on
your business needs.

<div id="heading--1-6">
    <h3>
        <a href="#TOC">Architecture </a>
    </h3>
    <p>
   All the libraries in Azure SDK for ABAP are built using SAP’s proprietary language ABAP, so this makes it very easy for all the SAP customers and developers to easily consume the libraries in any ABAP based programs or objects in SAP ABAP NetWeaver stack. 
    </p>
</div>
SAP has many objects like transactions, programs, Function modules, classes, events, Business object repositories,
Enhancements and so on, ABAP SDK for Azure can be integrated into all these objects to transfer data to Azure in either real time or batch bases on the processing mode of the respective objects.

ABAP SDK for Azure primarily uses HTTPs protocol and ODBC connections to transfer the data to Azure Cloud
products and Services.

![](MD%20image/25.png)

<p align="center">
Figure 1: High-level Architecture diagram of ABAP SDK for Azure 
</p>

<div id="heading--1-7">
    <h3>
        <a href="#TOC">Installation</a>
    </h3>
    <p>
    </p>
</div>
You can easily install ABAP SDK for Azure in your landscape through

[abapGit](https://github.com/larshp/abapGit)

>**Note**: : abapGit is mandatory to install ABAP SDK for Azure. If you do not already have it in your SAP system, please install it. For more details on abapGit installation, visit https://github.com/larshp/abapGit

In SE80, create a new package **‘Z_AZURE_ABAPSDK’**

![](MD%20image/26.png)

![](MD%20image/27.png)

After creating package, in se38 execute program ‘ZABAPGIT’ and select ‘+Online’

![](MD%20image/28.PNG)

Maintain the data in the fields as mentioned below and Select push button ‘OK’.

Git clone URL: https://github.com/Microsoft/ABAP-SDK-for-Azure

Target Package: Z_AZURE_ABAPSDK
![](MD%20image/29.PNG)

Save and activate all the objects into a transport request.

Now abapGit will show all the objects of ABAP SDK for Azure on the screen as shown in below screenshot

![](MD%20image/30.png)

Once installed successfully, you should see all the objects of ABAP SDK for Azure imported into package
‘Z_AZURE_ABAPSDK’.

![](MD%20image/31.png)

<div id="heading--1-8">
    <h3>
        <a href="#TOC">Implementation Guides</a>
    </h3>
    <p>
 We have used Abstract factory pattern to build these libraries. So, implementation of these libraries is almost similar with few minor differences in configuration. Please find the detailed implementation guide for Azure Event hubs.We will be provide Implementation guides for other services in next few weeks.
    </p>
</div>

•	[ABAP SDK Implementation Guide for Event Hubs](https://github.com/Microsoft/ABAP-SDK-for-Azure/blob/master/ABAP%20SDK%20Implementation%20Guide%20for%20Azure%20Event%20hubs.pdf)

<div id="heading--1-9">
    <h3>
        <a href="#TOC">Dos and Don’ts on usage of ABAP SDK</a>
    </h3>
    <p>
 ABAP SDK currently supports only HTTPS protocol, so be very cautious about performance issues when sending high volumes of messages from SAP to Azure services. You need to consider parallel processing techniques to avoid performance bottlenecks in SAP system.
    </p>
</div>

<div id="heading--1-2">
    <h3>
        <a href="#TOC">For more information</a>
    </h3>
    <p>
    </p>
</div>
You can find more information on ABAP SDK in 

 [Microsoft IT Showcase](https://www.microsoft.com/en-us/itshowcase/streamlining-business-processes-with-sap-connectors-and-azure-services)
<div id="heading--1-1">
    <h3>
        <a href="#TOC">Contact and Support</a>
    </h3>
    <p>
    </p>
</div>
For any queries and support, please mail us at

[ABAP SDK for Azure Support](mailto:abapsdk4azure@microsoft.com)







