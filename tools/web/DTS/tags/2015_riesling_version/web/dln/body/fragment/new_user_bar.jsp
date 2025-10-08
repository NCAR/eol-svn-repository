<%--
 * The new_user_bar.jsp is a fragment for the user list.  It is a header
 * bar that is displayed at the top and bottom of the page containing the add new
 * button.  It is in a fragment file so updates will be applied to both the top 
 * and bottom header.
--%>
<div class="newEntryBar">
    <div class="button">
        <button onClick="javascript: editProject( '${pageContext.request.contextPath}/dln/edit/edit_user.jsp?mode=add' );">Add New</button>
    </div>
</div>