<script type="text/javascript">
	downloadFile = function() {
		var url = "${g.createLink(absolute:true,controller:'tempFile',action:'adownload')}/${tFileId}";
		var downloadId = "hiddenDownloadFrame";
		var iframe = $("#"+downloadId);
	
		if (iframe.size() == 0) {
			iframe = $("<iframe></iframe>")
				.attr("id", downloadId)
				.css("display", "none")
				.css("visibility", "hidden");
			
			$("body").append(iframe);
		}
		
		iframe.attr("src", url);
	}
</script>