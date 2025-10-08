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
		
			$(function() {
				var fName = $( "input[name=firstName]" ),
					mName = $( "input[name=middleName]" ),
					lName = $( "input[name=lastName]" ),
					org = $( "input[name=organization]" ),
					pos = $( "input[name=position]" ),
					email = $( "input[name=email]" ),
					phone = $( "input[name=phone]" ),
					fax = $( "input[name=fax]" ),
					addr = $( "textarea[name=address]" ),
					city = $( "input[name=city]" ),
					state = $( "input[name=state]" ),
					pCode = $( "input[name=postalCode]" ),
					country = $( "input[name=country]" ),
					page = $( "input[name=homepage]" ),
					allFields = $( [] ).add(fName).add(mName).add(lName)
						.add(org).add(pos).add(email).add(phone).add(fax)
						.add(addr).add(city).add(state).add(pCode)
						.add(country).add(page),
					tips = $( ".validateTips" );
	
				function updateTips( t ) {
					tips.text( t ).addClass( "ui-state-highlight" );
					setTimeout(function() {
						tips.removeClass( "ui-state-highlight", 1500 );
					}, 500 );
				}
	
				function checkLength( o, n, min, max ) {
					if ( o.val().length > max || o.val().length < min ) {
						o.addClass( "ui-state-error" );
						updateTips( "Length of " + n + " must be between " +
							min + " and " + max + "." );
						return false;
					} else {
						return true;
					}
				}
	
				function checkRegexp( o, regexp, n ) {
					if ( !( regexp.test( o.val() ) ) ) {
						o.addClass( "ui-state-error" );
						updateTips( n );
						return false;
					} else {
						return true;
					}
				}
	
				$( "#dialog-form" ).dialog({
					autoOpen: false,
					height: 450,
					width: 530,
					modal: true,
					buttons: {
						"Create New Author": function() {
							var bValid = true;
							allFields.removeClass( "ui-state-error" );
							
							// Call all the validation functions
							bValid = bValid && checkLength( phone, "phone", 0, 63 );
							bValid = bValid && checkLength( fax, "fax", 0, 63 );
							bValid = bValid && checkLength( addr, "address", 0, 65535 );
							bValid = bValid && checkLength( pCode, "postal code", 0, 31 );
							bValid = bValid && checkLength( country, "country", 0, 63 );
							
							if ( bValid ) {
								// Convert the fields into JSON
								var authorData = allFields.serializeObject();
								//console.log(authorData);
								
								// Trigger the author create AJAX action
								$.ajax({
									url:"${g.createLink(controller:'author',action:'acreate')}",
									data: authorData,
									success: function(data) {
										// Once the AJAX returns successful, clear the form.
										allFields.each(function() {
											$(this).val("");
										});

										// Be sure to add the author to the list!
										$("#authSearchId").attr("value", data[0]);
										$("#authSearchName").attr("value", data[1]);
										addAuthorChild();

										// Close this dialog.
										$("#dialog-form").dialog("close");
									},
									error: function(request, status, error) {
									}
								});
							}
						},
						Cancel: function() {
							$( this ).dialog( "close" );
						}
					},
					close: function() {
						allFields.val( "" ).removeClass( "ui-state-error" );
					}
				});
	
				/*
				$( "#create-readme" ).button().click(function() {
					$( "#dialog-form" ).dialog( "open" );
				});
				*/
	
				$(function() {
					$( "#filetypes" ).buttonset();
					$( "#doptions" ).buttonset();
				});
			});
	
			function triggerDialog() {
				return $( "#dialog-form" ).dialog( "open" );
			}
		</script>
		
		<form name="new_author">
			<fieldset class="dialog">
				<div id="dialog-form" title="Create a New Author:">
					<p class="validateTips">All required fields are marked.</p>
					<table style="width: 500px">
				 		<tr>
				 			<td width="33%">
							 	<label for="firstName">First</label>
								<input type="text" id="firstName" name="firstName" class="reqd text ui-widget-content ui-corner-all" />
							</td>
							<td width="33%">
								<label for="middleName">Middle</label>
								<input type="text" id="middleName" name="middleName" class="text ui-widget-content ui-corner-all" />
							</td>
							<td width="33%">
								<label for="lastName">Last</label>
								<input type="text" id="lastName" name="lastName" class="reqd text ui-widget-content ui-corner-all" />
							</td>
						</tr>
						
						<tr>
							<td colspan="2">
								<label for="organization">Organization</label>
								<input type="text" id="organization" name="organization" class="reqd text ui-widget-content ui-corner-all" style="width:90%" />
							</td>
							<td colspan="1">
								<label for="position">Position</label>
								<input type="text" id="position" name="position" class="text ui-widget-content ui-corner-all" />
							</td>
						</tr>
						
						<tr>
							<td>
								<label for="email">E-Mail</label>
								<input type="text" id="email" name="email" class="text ui-widget-content ui-corner-all" />
							</td>
							<td>
								<label for="phone">Phone</label>
								<input type="text" id="phone" name="phone" class="text ui-widget-content ui-corner-all" />
							</td>
							<td>
								<label for="fax">Fax</label>
								<input type="text" id="fax" name="fax" class="text ui-widget-content ui-corner-all" />
							</td>
						</tr>
					
						<!-- Address Pieces Here -->
						<tr>
							<td colspan="2">
								<label for="address">Address</label>
								<textarea id="address" name="address" class="text ui-widget-content ui-corner-all" style="width: 90%; height: 100px;"></textarea>
							</td>
							<td colspan="1">
								<label for="city">City</label>
								<input type="text" id="city" name="city" class="text ui-widget-content ui-corner-all" /><br />
								<label for="state">State</label>
								<input type="text" id="state" name="state" class="text ui-widget-content ui-corner-all" /><br />
								<label for="postalCode">Postal Code</label>
								<input type="text" id="postalCode" name="postalCode" class="text ui-widget-content ui-corner-all" /><br />
								<label for="country">Country</label>
								<input type="text" id="country" name="country" class="text ui-widget-content ui-corner-all" />
							</td>
						</tr>
						<tr>
							<td colspan="3">
								<label for="homepage">Homepage</label>
								<input type="text" id="homepage" name="homepage" class="text ui-widget-content ui-corner-all" style="width: 230px" />
				  			</td>
				  		</tr>				
					</table>
				</div>
			</fieldset>
		</form>