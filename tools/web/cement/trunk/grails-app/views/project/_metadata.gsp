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
</geospatialCoverage>

<timeCoverage>
 <start><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.beginDate}" /></start>
 <end><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.endDate}" /></end>
</timeCoverage>

<publisher>
 <name>NCAR/EOL</name>
 <contact url="http://data.eol.ucar.edu/" email="cadis@eol.ucar.edu"/>
</publisher>

<creator>
<name>${it.piContact.toCdpName()}</name>
<contact email="${it.piContact.email}" />
</creator>

<g:if test="${!it.nsfAwardNumber.startsWith('0999')}">
	<property name="awardNumber" value="${it.nsfAwardNumber}"/>
	<documentation xlink:href="http://www.nsf.gov/awardsearch/showAward.do?AwardNumber=${it.nsfAwardNumber.encodeAsURL()}" xlink:title="NSF award" type="extURL" />

	<property name="arlssLink" value="${it.nsfAwardNumber}"/>
	<documentation xlink:href="http://www.polar.ch2m.com/arlss_reports/arlss_projectsdetail.aspx?cbpropnum=${it.nsfAwardNumber.encodeAsURL()}" xlink:title="ARLSS Progress Report" type="extURL" />
</g:if>

<keyword>${it.discipline}</keyword>

<date type="metadataCreated" format="yyyy-MM-DD'T'HH:mm:ss'Z'"><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.dateCreated}" /></date>
<date type="modified" format="yyyy-MM-DD'T'HH:mm:ss'Z'"><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.lastUpdated}" /></date>

<documentation type="IPY">${it.title.encodeAsCdp()}</documentation>

<%-- xlinks --%>

</metadata>
