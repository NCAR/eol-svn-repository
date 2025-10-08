if (typeof jQuery !== 'undefined') {
	(function($) {
		$('#spinner').ajaxStart(function() {
			$(this).fadeIn();
		}).ajaxStop(function() {
			$(this).fadeOut();
		});
	})(jQuery);
}

function initGlobalVar(varName, initValue) {
	if (window[varName] == undefined) {
		window[varName] = initValue;
	} else {
		return;
	}
}

function addClone(cloneId, cloneVar, cloneMax) {
	if (window[cloneVar] >= cloneMax) {
		alert('You can only duplicate this field up to ' + cloneMax + ' times!');
		return;
	}

	var clone = $('#' + cloneId).clone()
		.insertAfter('#' + cloneId)
		.attr("id", cloneId + window[cloneVar])
		.find("*")
		.each(function() {
			var id = this.id || "";
			var name = this.name || "";
			var forLabel = this.getAttribute('for') || "";
			var domainProperty = this.getAttribute('domain-property') || "";

			if (id != "") {
				this.id = id + window[cloneVar];
			}

			if (name != "") {
				this.name = name + window[cloneVar];
			}

			if (forLabel != "") {
				this.setAttribute('for', forLabel + window[cloneVar]);
			}

			if (domainProperty != "") {
				this.setAttribute('domain-property', domainProperty + window[cloneVar]);
			}
		});

	++window[cloneVar];
}

function removeClone(cloneId, cloneVar, cloneMin) {
	if (window[cloneVar] <= cloneMin) {
		alert("You can't remove the last remaining element.");
		return;
	}

	--window[cloneVar];
	$('#' + cloneId + window[cloneVar]).remove();
}
