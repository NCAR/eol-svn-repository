<%@ page import="meta.Dataset" %>
<%@ page import="java.util.TimeZone" %>

<script type="text/javascript">
	var required = [${required}];
	var badFields = [];
	
	function verifyRequired() {
		var verified = true;
		badFields = [];
		
		for (var i = 0; i < required.length; i++) {
			var field = $("label[for="+required[i]+"]").next();
			if ( field.val() == null || field.val() == "" ) {
				verified = false;
				badFields.push(required[i]); // Add the unverified field
			}
		}
		
		return verified;
	}

	function addRequireds() {
		for (var i = 0; i < required.length; i++) {
			var field = $("label[for="+required[i]+"]")
			
			if (  field.children(".required-indicator").size() == 0 ) {
				if ( field.children(".helptip").size() > 0 ) {
					field.children(".helptip").before("<span class=\"required-indicator\">*</span>");
				} else {
					field.append("<span class=\"required-indicator\">*</span>");
				}
			}
			
			field.next().attr("required", "");
		}
	}

	$(document).on("ready", addRequireds);
</script>

<div class="title fieldcontain ${hasErrors(bean: datasetInstance, field: 'project', 'error')} required">
	<g:hiddenField name="project.id" value="${datasetInstance?.project?.id}" /> 
</div>

<div class="title fieldcontain ${hasErrors(bean: datasetInstance, field: 'title', 'error')} required">
	<label for="title">
		<g:message code="dataset.title.label" default="Title" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="title" />
	</label>
	<g:textField name="title" maxlength="255" required="" value="${datasetInstance?.title}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'summary', 'error')} ">
	<label for="summary">
		<g:message code="dataset.summary.label" default="Summary" />
		<meta:tooltip domain="dataset" prop="summary" />
	</label>
<%--	<g:textArea name="summary" cols="40" rows="5" maxlength="65535" value="${datasetInstance?.summary}"/>--%>
	<ckeditor:editor name="summary" height="400px" width="100%">
	${datasetInstance?.summary}
	</ckeditor:editor>
</div>

<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'datasetVersion', 'error')} required">
	<label for="datasetVersion">
		<g:message code="dataset.datasetVersion.label" default="Dataset Version" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="datasetVersion" />
	</label>
	<g:field type="number" name="datasetVersion" min="0" required="" value="${fieldValue(bean: datasetInstance, field: 'datasetVersion')}"/>
</div>
</sec:ifAnyGranted>

<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'progress', 'error')} required">
	<label for="progress">
		<g:message code="dataset.progress.label" default="Progress" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="progress" />
	</label>
	<g:select name="progress" from="${datasetInstance.constraints.progress.inList}" required="" value="${datasetInstance?.progress}" valueMessagePrefix="dataset.progress"/>
</div>

<div style="clear:both;"></div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'owner', 'error')} required">
	<g:if test="${datasetInstance?.owner?.id}">
		<g:hiddenField name="owner.id" value="${datasetInstance?.owner?.id}" />
	</g:if>
	<g:else>
		<g:set var="username"><sec:loggedInUserInfo field="username" /></g:set>
		<g:hiddenField name="owner.id" value="${meta.User.findByUsername(username).id}" />
	</g:else>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'pointOfContact', 'error')} required">
	<label for="pointOfContact">
		<g:message code="dataset.pointOfContact.label" default="Point of Contact" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="pointOfContact" />
	</label>
	<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
		<g:set var="pointOfContactList" value="${meta.auth.User.list()}" />
	</sec:ifAnyGranted>
	<sec:ifNotGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
		<g:set var="dsProject" value="${datasetInstance?.project}" />
		<g:if test="${dsProject?.members() != null}">
			<g:set var="pointOfContactList" value="${dsProject?.members()}" />
		</g:if>
		<g:else>
			<g:set var="pointOfContactList" value="${datasetInstance?.owner?.id}" />
		</g:else>
	</sec:ifNotGranted>
	<g:select id="pointOfContact" name="pointOfContact.id" from="${pointOfContactList}" optionKey="id" required="" value="${datasetInstance?.pointOfContact?.id}" class="many-to-one"/>
</div>


<g:render template="/author/authors" model="['datasetInstance':datasetInstance]" />


<br /><br />
<label class="section-label">Temporal Coverage</label>
<div class="dateRange fieldcontain ${hasErrors(bean: datasetInstance, field: 'beginDate', 'error')} ">
	<label for="beginDate">
		<g:message code="dataset.beginDate.label" default="Begin Date" />
		<meta:tooltip domain="dataset" prop="beginDate" />
	</label>
	<meta:dateTimeInputter name="beginDate" domain="ds" precision="minute" value="${datasetInstance?.beginDate}"/>
<%--	<g:datePicker name="beginDate" precision="minute" value="${datasetInstance?.beginDate}" default="none" noSelection="['': '']" />--%>
<%--	<meta:timeZoneInputter name="beginDateTZ" />--%>
</div>

<div class="dateRange fieldcontain ${hasErrors(bean: datasetInstance, field: 'endDate', 'error')} ">
	<label for="endDate">
		<g:message code="dataset.endDate.label" default="End Date" />
		<meta:tooltip domain="dataset" prop="endDate" />
	</label>
	<meta:dateTimeInputter name="endDate" domain="ds" precision="minute" value="${datasetInstance?.endDate}"/>
<%--	<g:datePicker name="endDate" precision="minute" value="${datasetInstance?.endDate}" default="none" noSelection="['': '']" />--%>
<%--	<meta:timeZoneInputter name="endDateTZ" />--%>
</div>
<g:render template="/layouts/anytime_script"/>

<%--<div id="timeZone" class="dateRange fieldcontain">--%>
<%--	<label for="theTimeZone" title=" Time Zone must be the same for both Begin and End Dates ">Time Zone</label>--%>
<%--	<g:radio name="timeZoneType" value="UTC" checked="true" onclick="document.getElementById('theTimeZone').style.display='none'" /> <label for="UTCtime">UTC</label>--%>
<%--	<g:radio name="timeZoneType" value="localtime" onclick="document.getElementById('theTimeZone').style.display='block'" /> <label for="localtime">Local Time</label>--%>
<%--	<g:hiddenField id="theTimeZoneField" name='theTimeZone' value=""/>--%>
<%--	<g:timeZoneSelect id="theTimeZone" name="theTimeZoneSelect" style="display: none" onchange="document.getElementById('theTimeZoneField').value = this.options[this.selectedIndex].text" />--%>
<%--</div>--%>

<div style="clear:both;"></div>



<br /><br />
<label class="section-label">Spatial Coverage</label>
<div class="coords fieldcontain ${hasErrors(bean: datasetInstance, field: 'minLat', 'error')} ">
	<label for="minLat">
		<g:message code="dataset.minLat.label" default="Minimum (South) Latitude" />
		<meta:tooltip domain="dataset" prop="minLat" />
	</label>
	<g:field type="number" name="minLat" min="-90" max="90" step="any" value="${fieldValue(bean: datasetInstance, field: 'minLat')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: datasetInstance, field: 'maxLat', 'error')} ">
	<label for="maxLat">
		<g:message code="dataset.maxLat.label" default="Maximum (North) Latitude" />
		<meta:tooltip domain="dataset" prop="maxLat" />
	</label>
	<g:field type="number" name="maxLat" min="-90" max="90" step="any" value="${fieldValue(bean: datasetInstance, field: 'maxLat')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: datasetInstance, field: 'minLon', 'error')} ">
	<label for="minLon">
		<g:message code="dataset.minLon.label" default="Minimum (West) Longitude" />
		<meta:tooltip domain="dataset" prop="minLon" />
	</label>
	<g:field type="number" name="minLon" min="-180" max="180" step="any" value="${fieldValue(bean: datasetInstance, field: 'minLon')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: datasetInstance, field: 'maxLon', 'error')} ">
	<label for="maxLon">
		<g:message code="dataset.maxLon.label" default="Maximum (East) Longitude" />
		<meta:tooltip domain="dataset" prop="maxLon" />
	</label>
	<g:field type="number" name="maxLon" min="-180" max="180" step="any" value="${fieldValue(bean: datasetInstance, field: 'maxLon')}"/>
</div>

<div style="clear:both;"></div>



<br /><br />
<label class="section-label">Funding Information</label>
<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'fundingAgency', 'error')} required">
	<label for="fundingAgency">
		<g:message code="dataset.fundingAgency.label" default="Funding Agency" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="fundingAgency" />
	</label>
	<g:select id="fundingAgency" name="fundingAgency.id" from="${meta.Agency.list()}" optionKey="id" required="" value="${datasetInstance?.fundingAgency?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'awardNumber', 'error')}">
	<label for="awardNumber">
		<g:message code="dataset.awardNumber.label" default="Award Number" />
	</label>
	<g:textField name="awardNumber" value="${datasetInstance?.awardNumber}"/>
</div>

<%--<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'awardAmount', 'error')} ">--%>
<%--	<label for="awardAmount">--%>
<%--		<g:message code="dataset.awardAmount.label" default="Award Amount" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:field type="number" name="awardAmount" value="${fieldValue(bean: datasetInstance, field: 'awardAmount')}"/>--%>
<%--</div>--%>




<br /><br />
<label class="section-label">Categorization &amp; Instrumentation</label>
<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'datasetCategories', 'error')} ">
	<label for="datasetCategories">
		<g:message code="dataset.datasetCategories.label" default="Categories" />
		<meta:tooltip domain="dataset" prop="datasetCategories" />
	</label>
<%--	<g:hiddenField name="datasetCategories" value="${datasetInstance?.categories()}" />--%>
	<g:select id="categoryList" name="categoryId" from="${meta.Category.list().sort{it.name.toLowerCase()}}" optionKey="id" multiple="true" value="${datasetInstance?.categories()}" class="many-to-many"/>
	<span class="tooltip-ui" style="display: inline-block;">Don't see the category you're looking for? <br /><a href="mailto:metaarch@eol.ucar.edu?subject=[MetaArch] New Category Suggestion">Suggest a new category.</a></span>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'datasetPlatforms', 'error')} ">
	<label for="datasetPlatforms">
		<g:message code="dataset.datasetPlatforms.label" default="Platforms" />
		<meta:tooltip domain="dataset" prop="datasetPlatforms" />
	</label>
<%--	<g:hiddenField name="datasetPlatforms" value="${datasetInstance?.platforms()}" />--%>
	<g:select id="platformList" name="platformId" from="${meta.Platform.list().sort{it.name.toLowerCase()}}" optionKey="id" multiple="true" value="${datasetInstance?.platforms()}" class="many-to-many"/>
	<span class="tooltip-ui" style="display: inline-block;">Don't see the platform you're looking for? <br /><a href="mailto:metaarch@eol.ucar.edu?subject=[MetaArch] New Platform Suggestion">Suggest a new platform.</a></span>
</div>



<br /><br />
<label class="section-label">Resolution</label>
<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'horizontalResolution', 'error')} ">
	<label for="horizontalResolution" style="width:36% !important;">
		<g:message code="dataset.horizontalResolution.label" default="Horizontal Resolution" />
		<meta:tooltip domain="dataset" prop="horizontalResolution" />
	</label>
	<g:select id="horizontalResolution" name="horizontalResolution.id" from="${meta.HorizontalResolution.list()}" optionKey="id" value="${datasetInstance?.horizontalResolution?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'verticalResolution', 'error')} ">
	<label for="verticalResolution">
		<g:message code="dataset.verticalResolution.label" default="Vertical Resolution" />
		<meta:tooltip domain="dataset" prop="verticalResolution" />
	</label>
	<g:select id="verticalResolution" name="verticalResolution.id" from="${meta.VerticalResolution.list()}" optionKey="id" value="${datasetInstance?.verticalResolution?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>
<div style="clear: both;"></div>



<br /><br />
<label class="section-label">GCMD Keywords</label>
<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'scienceKeyword', 'error')} ">
	<label for="scienceKeyword">
		<g:message code="dataset.scienceKeyword.label" default="Science Keyword" />
		<meta:tooltip domain="dataset" prop="scienceKeyword" />
	</label>
	<g:select id="scienceKeyword" name="scienceKeyword.id" from="${meta.GcmdScience.list()}" optionKey="id" value="${datasetInstance?.scienceKeyword?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'locationKeyword', 'error')} ">
	<label for="locationKeyword">
		<g:message code="dataset.locationKeyword.label" default="Location Keyword" />
		<meta:tooltip domain="dataset" prop="locationKeyword" />
	</label>
	<g:select id="locationKeyword" name="locationKeyword.id" from="${meta.GcmdLocation.list()}" optionKey="id" value="${datasetInstance?.locationKeyword?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'platformKeyword', 'error')} ">
	<label for="platformKeyword">
		<g:message code="dataset.platformKeyword.label" default="Platform Keyword" />
		<meta:tooltip domain="dataset" prop="platformKeyword" />
	</label>
	<g:select id="platformKeyword" name="platformKeyword.id" from="${meta.GcmdPlatform.list()}" optionKey="id" value="${datasetInstance?.platformKeyword?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'instrumentKeyword', 'error')} ">
	<label for="instrumentKeyword">
		<g:message code="dataset.instrumentKeyword.label" default="Instrument Keyword" />
		<meta:tooltip domain="dataset" prop="instrumentKeyword" />
	</label>
	<g:select id="instrumentKeyword" name="instrumentKeyword.id" from="${meta.GcmdInstrument.list()}" optionKey="id" value="${datasetInstance?.instrumentKeyword?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'topic', 'error')} ">
	<label for="topic">
		<g:message code="dataset.topic.label" default="ISO Topic" />
		<meta:tooltip domain="dataset" prop="topic" />
	</label>
	<g:select id="topic" name="topic.id" from="${meta.IsoTopic.list()}" optionKey="id" value="${datasetInstance?.topic?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>



<br /><br />
<%--<label class="section-label">Data Access Details</label>--%>
<label class="section-label">Additional Information</label>
<%--<g:render template="/file/files" model="['datasetInstance':datasetInstance]" />--%>
<g:render template="/xlink/xlinks" model="['datasetInstance':datasetInstance]" />


<%--<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'distributionFormat', 'error')} ">--%>
<%--	<label for="distributionFormat">--%>
<%--		<g:message code="dataset.distributionFormat.label" default="Distribution Format" />--%>
<%--		--%>
<%--	</label>--%>
<%--	<g:select id="distributionFormat" name="distributionFormat.id" from="${meta.Format.list()}" optionKey="id" value="${datasetInstance?.distributionFormat?.id}" class="many-to-one" noSelection="['null': '']"/>--%>
<%--</div>--%>


<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'spatialType', 'error')} required">
	<label for="spatialType">
		<g:message code="dataset.spatialType.label" default="Spatial Type" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="spatialType" />
	</label>
	<g:select name="spatialType" from="${datasetInstance.constraints.spatialType.inList}" required="" value="${datasetInstance?.spatialType}" valueMessagePrefix="dataset.spatialType"/>
</div>


<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'frequency', 'error')} ">
	<label for="frequency">
		<g:message code="dataset.frequency.label" default="Frequency" />
		<meta:tooltip domain="dataset" prop="frequency" />
	</label>
	<g:select id="frequency" name="frequencyId" from="${meta.Frequency.list()}" optionKey="id" value="${meta.Frequency.get(datasetInstance?.frequency)}" />
</div>
<div style="clear: both;"></div>


<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'accessRestrictions', 'error')} required">
	<label for="accessRestrictions">
		<g:message code="dataset.accessRestrictions.label" default="Access Restrictions" />
		<span class="required-indicator">*</span>
		<meta:tooltip domain="dataset" prop="accessRestrictions" />
	</label>
	<g:select name="accessRestrictions" from="${datasetInstance.constraints.accessRestrictions.inList}" required="" value="${datasetInstance?.accessRestrictions}" valueMessagePrefix="dataset.accessRestrictions"/>
</div>

<div class="side-by-side fieldcontain ${hasErrors(bean: datasetInstance, field: 'dataTags', 'error')} ">
	<label for="dataTags">
		<g:message code="dataset.dataTags.label" default="Data Tags" />
		<meta:tooltip domain="dataset" prop="dataTags" />
	</label>
	<g:textField name="dataTags" maxlength="200" value="${datasetInstance?.dataTags}"/>
</div>
<div style="clear: both;"></div>


<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'language', 'error')} ">
	<label for="language">
		<g:message code="dataset.language.label" default="Language" />
		<meta:tooltip domain="dataset" prop="language" />
	</label>
	<g:textField name="language" value="${datasetInstance?.language}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'comments', 'error')} ">
	<label for="comments">
		<g:message code="dataset.comments.label" default="Comments" />
		<meta:tooltip domain="dataset" prop="comments" />
	</label>
	<g:textArea name="comments" cols="40" rows="5" value="${datasetInstance?.comments}"/>
</div>

