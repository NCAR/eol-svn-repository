<script type="text/javascript">
	$(function() {	
		$("input#memberSearchInput").autocomplete({
				minLength: 1,
				position: { my: "left top", at: "left bottom"},
				source: "${createLink(controller:'user', action:'asearch')}",
				select: function(event, ui) {
					// On select, add member to list!
					$("#memberSearchInput").val(ui.item.label);
					},
				open: function(event, ui) { 
					var z = parseFloat($(".project-permitted").css("z-index")) + 1;
					$(".ui-autocomplete").css("z-index", z);
					$(".ui-autocomplete").css("overflow-y", "auto");
					$(".ui-autocomplete").css("font-size", "12px");					
					}
		});
	});
</script>