<script type="text/javascript">
	/* Serialize Object to JSON */
	$.fn.serializeObject = function()
	{
		var o = {};
		var a = this.serializeArray();
		$.each(a, function() {
			if (o[this.name] !== undefined) {
				if (!o[this.name].push) {
					o[this.name] = [o[this.name]];
				}
				o[this.name].push(this.value || '');
			} else {
				o[this.name] = this.value || '';
			}
		});
		return o;
	}; /* Serialize Object to JSON */

	function addReqChild(id, field) {
		var reqChild = $(".req-div[is-clone=clone]").clone();
			reqChild.attr("is-clone", "nonclone");
			reqChild.find(".del-req").attr("id", "req-"+id);
			reqChild.find(".req-field").html(field);
		
		$("#reqList").append(reqChild);
		reqChild.show();
	}
	
	$(function() {
		var prj = $("input#reqSearchId").val();
		$("input#reqSearchInput").autocomplete({
				minLength: 0,
				position: { my: "left top", at: "left bottom"},
				source: "${createLink(controller:'requirement', action:'afields', absolute:'true')}?project="+prj,
				select: function(event, ui) {
					// On select, add requirement to list!
					$("#reqSearchInput").val(ui.item.label);
					},
				open: function(event, ui) { 
					var z = parseFloat($(".project-permitted").css("z-index")) + 1;
					$(".ui-autocomplete").css("z-index", z);
					$(".ui-autocomplete").css("overflow-y", "auto");
					$(".ui-autocomplete").css("font-size", "12px");					
					}
		});
		
		$(document).on('click', 'input#addReq', function() {
			var project = $( "input#reqSearchId" );
			var field = $( "input#reqSearchInput" );
			var allFields = $( [] ).add(project).add(field);
			
			// Convert the fields into JSON
			var reqData = allFields.serializeObject();
			
			// Trigger the author create AJAX action
			$.ajax({
				url:"${g.createLink(controller:'requirement',action:'acreate', absolute:'true')}",
				data: reqData,
				success: function(data) {
					// Be sure to add the requirement to the list!
					addReqChild(data[0], field.val());
					
					// Once the AJAX returns successful, clear the form.
					field.val("");
				},
				error: function(request, status, error) {
				}
			});
		});

		$(document).on('click', '.del-req', function() {
			// find the parent div
			var parent = $(this).parents(".req-div");
			var reqId = ($(this).attr('id')).replace('req-', '');

			// Trigger the author delete AJAX action
			$.ajax({
				url:"${g.createLink(controller:'requirement',action:'adelete', absolute:'true')}/"+reqId,
				success: function(data) {
					// remove from the DOM!
					parent.remove();
				},
				error: function(request, status, error) {
				}
			});
		});	
	});
</script>

<g:form>
	<fieldset>
		<legend>Require:</legend>
		<g:hiddenField id="reqSearchId" name="id" value="${projectInstance?.id}" />
		<g:textField id="reqSearchInput" name="field" onchange="" />
		<input type="button" id="addReq" value="Add" />
	</fieldset>
</g:form>