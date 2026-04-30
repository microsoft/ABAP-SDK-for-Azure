

<H1 align="center">
<font size="16"> 
  <b> Walkthrough for MS Graph using AAD Token V2.0 to work with SharePoint </b>
</font> 
</H1>

<p align="center"

<https://github.com/Microsoft/ABAP-SDK-for-Azure>
</p>

<p align="right">
Author: Patrick Weber, BOMAG  
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
            <a href="#Purpose">1. Purpose </a>
        </li>
        <li>
            <a href="#Prerequisites">2. Prerequisites </a>
        </li>
    </ul>
</div>

* [2.1 Define RFC Destination for AAD Token V2](#heading--1-1)
* [2.2 Define RFC Destination for Graph](#heading--1-2)
* [2.3 Configuration](#heading--1-3)
   
   
<div id="TOC">
    <ul>
        <li>
            <a href="#DEMO Program">3. DEMO Program</a>
        </li>
    </ul>
</div>

<div id="Purpose">
    <h2>
        <a href="#TOC">Purpose</a>
    </h2>
    <p>
    </p>
</div>

If you want to connect with SharePoint Online i.e. to upload files, the available O365 classes of this SDK are not able to do this. I was mentioned to use MS Graph instead. 

With MS Graph, you get a variety of rest services to connect with many Microsoft products running on Azure or Office 365 cloud. In this SDK, a first implementation
of MS Graph api is present and described. With this example, one can fetch user data and calendar items. In total, only three services of Graph has been implemented so far. To get my task done, I started to enhance ZCL_ADF_SERVICE_GRAPH class and added some methods to act with SharePoint Online. While doing this, I noted that the 
access token provided by login.microsoft.com/oauth2/token was not sufficient to access Graph.

For anyone interested in interacting with SharePoint Online using MS Graph, necessary configuration of RFC destinations and ADF can be found here. Also a small demo program ZADF_DEMO_AZURE_GRAPH_V2 is available.

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

<div id="heading--1-1">
    <h3>
        <a href="#TOC">2.1 Define RFC Destination for AAD Token V2</a>
    </h3>
    <p>
    Go to transaction SM59 in your SAP system and create new RFC destination of type ‘G’ like this
    </p>
</div>

Target host: **login.microsoftonline.com**

Port: 443

Path Prefix: **/InputTenantID/oauth2/v2.0/token**

![](MD%20image/58.png)

<div id="heading--1-2">
    <h3>
        <a href="#TOC">2.2 Define RFC Destination for Graph</a>
    </h3>
    <p>
    Go to transaction SM59 in your SAP system and create new RFC destination of type ‘G’ like this
    </p>
</div>

Target host: **graph.microsoft.com**

Port: 443

Path Prefix: **/v1.0**

![](MD%20image/59.png)
<div id="heading--1-3">
    <h3>
        <a href="#TOC">2.3 Configuration</a>
    </h3>
    <p>
   Go to transaction ZADF_CONFIG and define the necessary interface IDs. You will need three in total. Please note, that GRAPH_GET and GRAPH_PUT uses the same RFC 
   destination. We need two interfaces pointing to the same URI but we want to configure http header differently.
    </p>
</div>

![](MD%20image/60.png)

Next go to SAP Azure Data Framework Configurations and create these three entries. Enter Client Secret into field Key/AAD for interface TOKEN_V2. Set call type to synchronuous for all three

![](MD%20image/62.png)

Next go to SAP Azure REST Call Header values and create these entries. 
![](MD%20image/61.png)

<div id="DEMO Program">
    <h3>
        <a href="#TOC">3. DEMO Program</a>
    </h3>
    
   Report ZADF_DEMO_AZURE_GRAPH_V2 of package ZADF_DEMO shows different usage examples. It shows you how to
    <ul>
    <li>fetch ID of SharePoint Site Collection</li>
    <li>get a list of all SharePoint lists of a site</li>
    <li>get all list items of a list</li>
    <li>get all drives (aka document libraries) </li>
    <li>upload files to a drive</li>
    </ul>
</div>




















