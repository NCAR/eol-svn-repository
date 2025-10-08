<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%
   response.addHeader("Pragma","no-cache");
   response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
   response.addHeader("Cache-Control","pre-check=0, post-check=0");
   response.setDateHeader("Expires",0);
%>

<c:choose>
   <c:when test="${param.project == 'ACE-ASIA'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/ace-asia/index.html" />
   </c:when>
   <c:when test="${param.project == 'AMMA'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/amma/index.html" />
   </c:when>
   <c:when test="${param.project == 'BAMEX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/bamex/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/EOP-1'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/ceop/eop1/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/EOP-3/4'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/ceop/eop3and4/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/HYDRO'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/ceop/hydro/index.html" />
   </c:when>
   <c:when test="${param.project == 'DYCOMS'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/dycoms/index.html" />
   </c:when>
   <c:when test="${param.project == 'EPIC'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/epic/index.html" />
   </c:when>
   <c:when test="${param.project == 'GAPP'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/gapp/index.html" />
   </c:when>
   <c:when test="${param.project == 'GCIP'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/gcip/index.html" />
   </c:when>
   <c:when test="${param.project == 'IHOP_2002'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/ihop/index.html" />
   </c:when>
   <c:when test="${param.project == 'INDOEX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/indoex/index.html" />
   </c:when>
   <c:when test="${param.project == 'LPB'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/lpb/index.html" />
   </c:when>
   <c:when test="${param.project == 'NAME'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/name/index.html" />
   </c:when>
   <c:when test="${param.project == 'PACS'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/pacs/index.html" />
   </c:when>
   <c:when test="${param.project == 'PLATIN'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/lpb/index.html" />
   </c:when>
   <c:when test="${param.project == 'RAINEX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/rainex/index.html" />
   </c:when>
   <c:when test="${param.project == 'RICO'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/rico/index.html" />
   </c:when>
   <c:when test="${param.project == 'SALLJEX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/salljex/index.html" />
   </c:when>
   <c:when test="${param.project == 'SMEX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/smex/index.html" />
   </c:when>
   <c:when test="${param.project == 'T-REX'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/trex/index.html" />
   </c:when>
   <c:when test="${param.project == 'VOCALS'}">
      <c:import url="http://data.eol.ucar.edu/master_list/static/vocals/index.html" />
   </c:when>
   <c:otherwise>
<html>
<head>
   <title>Master List</title>
</head>

<body>
<h1>Master List</h1>
<c:if test="${!empty param.project && param.project != ''}">
   <p>The Master List for ${param.project} could not be found.</p>
</c:if>

<ul>
   <li><a href="/master_list/?project=ACE-ASIA">ACE-ASIA</a></li>
   <li><a href="/master_list/?project=AMMA">AMMA</a></li>
   <li><a href="/master_list/?project=BAMEX">BAMEX</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-1">CEOP/EOP-1</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-3/4">CEOP/EOP-3/4</a></li>
   <li><a href="/master_list/?project=CEOP/HYDRO">CEOP/HYDRO</a></li>
   <li><a href="/master_list/?project=DYCOMS">DYCOMS</a></li>
   <li><a href="/master_list/?project=EPIC">EPIC</a></li>
   <li><a href="/master_list/?project=GAPP">GAPP</a></li>
   <li><a href="/master_list/?project=GCIP">GCIP</a></li>
   <li><a href="/master_list/?project=IHOP_2002">IHOP_2002</a></li>
   <li><a href="/master_list/?project=INDOEX">INDOEX</a></li>
   <li><a href="/master_list/?project=LPB">LPB</a></li>
   <li><a href="/master_list/?project=NAME">NAME</a></li>
   <li><a href="/master_list/?project=PACS">PACS</a></li>
   <li><a href="/master_list/?project=RAINEX">RAINEX</a></li>
   <li><a href="/master_list/?project=RICO">RICO</a></li>
   <li><a href="/master_list/?project=SALLJEX">SALLJEX</a></li>
   <li><a href="/master_list/?project=SMEX">SMEX</a></li>
   <li><a href="/master_list/?project=T-REX">T-REX</a></li>
   <li><a href="/master_list/?project=VOCALS">VOCALS</a></li>
</ul>

</body>
</html>
   </c:otherwise>
</c:choose>
