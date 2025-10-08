<% response.contentType = "text/xml" %> 
<% response.characterEncoding = "UTF-8" %>
<metadata
 xmlns="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0 http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.xsd"
 metadataType="THREDDS" inherited="true">

<documentation type="summary">${it.summary?.encodeAsCdp()}</documentation>

<geospatialCoverage>
 <northsouth>
  <start>${it.minimumLatitude}</start>
  <size>${it.maximumLatitude - it.minimumLatitude}</size>
  <units>Degrees</units>
 </northsouth>
 <eastwest>
  <start>${it.minimumLongitude}</start>
  <size>${it.maximumLongitude - it.minimumLongitude}</size>
  <units>Degrees</units>
 </eastwest>
 <name>${it.locationKeyword}</name>
</geospatialCoverage>

<timeCoverage>
 <start><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.beginDate}" /></start>
 <end><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.endDate}" /></end>
</timeCoverage>

<publisher>
 <name>NCAR/EOL</name>
 <contact url="http://data.eol.ucar.edu/" email="cadis@eol.ucar.edu"/>
</publisher>

<g:if test="${it.project?.piContact}">
<creator>
<name>${it.project?.piContact.toCdpName()}</name>
<contact email="${it.project?.piContact.email}" />
</creator>
</g:if>

<property name="awardNumber" value="${it.project?.nsfAwardNumber}"/>

<contributor role="pointOfContact">
${it.metadataContact?.toCdpString()}
</contributor>

<contributor role="custodian">
${it.datacenterContact?.toCdpString()}
</contributor>

<keyword vocabulary="DIF">${it.scienceKeyword}</keyword>
<documentation type="platform">${it.platformKeyword}</documentation>
<documentation type="instruments">${it.instrumentKeyword}</documentation>

<documentation type="processing_level">${it.progress}</documentation>

<keyword vocabulary="searchDiscipline">${it.project?.discipline}</keyword>

<documentation type="ISO_Topic">${it.topic}</documentation>
<documentation type="rights">${it.accessRestrictions.encodeAsCdp()}</documentation>
<documentation type="rights">${it.useConstraints.encodeAsCdp()}</documentation>

<dataFormat>${it.distributionFormat}</dataFormat>

<documentation type="language">${it.datasetLanguage.encodeAsCdp()}</documentation>
<documentation type="metadataName">${it.metadataName}</documentation>
<documentation type="metadataVersion">${it.metadataVersion}</documentation>

<date type="metadataCreated" format="yyyy-MM-DD'T'HH:mm:ss'Z'"><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.dateCreated}" /></date>
<date type="modified" format="yyyy-MM-DD'T'HH:mm:ss'Z'"><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.lastUpdated}" /></date>

<documentation type="IPY">${it.project?.title.encodeAsCdp()}</documentation>

<g:if test="${it.dataAccess}">
<documentation xlink:href="${it.dataAccess.encodeAsCdpURL()}" xlink:title="Data Access" type="extURL" />
</g:if>
<g:if test="${it.xlink1}">
<documentation xlink:href="${it.xlink1.encodeAsCdpURL()}" xlink:title="${it.xlink1Purpose}" type="extURL" />
</g:if>
<g:if test="${it.xlink2}">
<documentation xlink:href="${it.xlink2.encodeAsCdpURL()}" xlink:title="${it.xlink2Purpose}" type="extURL" />
</g:if>

<dataType>${it.spatialType}</dataType>

<documentation type="frequency">${it.frequency}</documentation>
<documentation type="resolution">${it.resolution}</documentation>

<property name="dataTag" value="${it.dataTags?.encodeAsCdp()}" />

</metadata>
