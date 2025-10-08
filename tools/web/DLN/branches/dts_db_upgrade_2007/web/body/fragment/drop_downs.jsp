<%--
 * The drop_downs.jsp is a fragment for the data set list.  It is a header
 * bar that is displayed at the top and bottom of the page with the drop
 * down menus for the data set list filter and add new button.  It is in a 
 * fragment file so updates will be applied to both the top and bottom header.
--%>
<%@ page import="dln.display.*" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:useBean id="display" class="dln.display.DatasetDisplayBean" scope="session"/>	

<div class="dropdownBar">
    <div class="dropdown">
        <!-- Display the drop down box to show hidden data sets.  
             This must be done first because it floats right. -->
        <select name="show" onChange="javascript: changeVisibility( this, 'true' );">
            <option value="prompt" selected>------Show------</option>
            <c:if test="${!display.showLoaded}"><option value="loadFlag">Loaded</option></c:if>
            <c:if test="${!display.showChecked}"><option value="checkFlag">Checked</option></c:if>
            <c:if test="${!display.showDocumented}"><option value="docFlag">Documented</option></c:if>
            <%
            if (display.isDisplayProjectList() && !display.getShowMaster()) {
                out.println("<option value=\"masterFlag\">In Master List</option>");
            }
            %>
            <option value="allFlags">All</option>
        </select>
    </div>

    <div class="dropdown">
        <select name="hide" onChange="javascript: changeVisibility( this, 'false' );">
            <option value="prompt" selected>------Hide------</option>
            <c:if test="${display.showLoaded}"><option value="loadFlag">Loaded</option></c:if>
            <c:if test="${display.showChecked}"><option value="checkFlag">Checked</option></c:if>
            <c:if test="${display.showDocumented}"><option value="docFlag">Documented</option></c:if>
            <%
            if (display.isDisplayProjectList() && display.getShowMaster()) {
            	out.println("<option value=\"masterFlag\">In Master List</option>");
            }
            %>
        </select>
    </div>

    <div class="button">
        <button onclick="javascript: editDataset( '${pageContext.request.contextPath}/edit/edit_dataset.jsp?mode=add' );">Add New</button>
    </div>

    <c:if test="${!display.showLoaded}">    
	    <div class="status"><img src="${pageContext.request.contextPath}/images/loaded.gif" /> Loaded data sets are not displayed.</div>
    </c:if>
    <c:if test="${!display.showChecked}">
        <div class="status"><img src="${pageContext.request.contextPath}/images/checked.gif" /> Checked data sets are not displayed.</div>
    </c:if>
    <c:if test="${!display.showDocumented}">
        <div class="status"><img src="${pageContext.request.contextPath}/images/doc.gif" /> Documented data sets are not displayed.</div>
    </c:if>
    <% if (display.isDisplayProjectList() && !display.getShowMaster()) { %>
        <div class="status"><img src="${pageContext.request.contextPath}/images/ml.gif" /> Data sets in the Master List are not displayed.</div>
    <% } %>
</div>