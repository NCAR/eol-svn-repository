		<script type="text/javascript">
			/*
			function datesToUTC() {
				var bDate = document.getElementById('beginDate-input');
				var eDate = document.getElementById('endDate-input');
				var bd, ed;
				var str;
				
				// yyyy-MM-dd HH:mm Z
				bd = new Date(bDate.value);
				ed = new Date(eDate.value);

				str = bd.getUTCFullYear() + '-' + 
						(bd.getUTCMonth()+1 < 10? '0'+(bd.getUTCMonth()+1) : (bd.getUTCMonth()+1)) + '-' + 
						(bd.getUTCDate() < 10? '0'+bd.getUTCDate() : bd.getUTCDate()) + ' ' + 
						(bd.getUTCHours() < 10? '0'+bd.getUTCHours() : bd.getUTCHours()) + ':' + 
						(bd.getUTCMinutes() < 10? '0'+bd.getUTCMinutes() : bd.getUTCMinutes()) + 
						' +0000';
				//console.log("UTC: "+str);

				bDate.value = str;
				
				str = ed.getUTCFullYear() + '-' + 
						(ed.getUTCMonth()+1 < 10? '0'+(ed.getUTCMonth()+1) : (ed.getUTCMonth()+1)) + '-' + 
						(ed.getUTCDate() < 10? '0'+ed.getUTCDate() : ed.getUTCDate()) + ' ' + 
						(ed.getUTCHours() < 10? '0'+ed.getUTCHours() : ed.getUTCHours()) + ':' + 
						(ed.getUTCMinutes() < 10? '0'+ed.getUTCMinutes() : ed.getUTCMinutes()) + 
						' +0000';
				//console.log("UTC: "+str);

				eDate.value = str;

				//alert('bDate: '+bDate.value+'    eDate: '+eDate.value);
			}

			function formatATCinput(theVal, objId) {
				theVal = theVal.replace(/[()]+/g, '');
				var valArr = theVal.split(' ');
				var outVal = '';
				var m = { Jan: ['01'], Feb: ['02'], Mar: ['03'], Apr: ['04'], May: ['05'], Jun: ['06'], Jul: ['07'], Aug: ['08'], Sep: ['09'], Oct: ['10'], Nov: ['11'], Dec: ['12'] };
				var mstr = valArr[1];
				var mon = m[mstr];
				
				outVal = valArr[2]+'-'+mon+'-'+valArr[0]+' '+valArr[3]+' '+valArr[4];

				document.getElementById(objId).value = outVal;
			}
			*/


			function updateInputter(obj) {
				var id = (obj.id).replace(/^(prj|ds)\-/, '');
				var val = obj.value;

				var valArr = val.split(' ');
				var m = { Jan: [1], Feb: [2], Mar: [3], Apr: [4], May: [5], Jun: [6], Jul: [7], Aug: [8], Sep: [9], Oct: [10], Nov: [11], Dec: [12] };
				var mstr = valArr[1];
				var mon = m[mstr];
				var time = (valArr[3]).split(':');
<%--				var offset = (valArr[4]).replace(/[()]/g, '');--%>
<%--				var hoff = offset.substring(offset.length-4,offset.length-2);--%>
<%--				var moff = offset.substring(offset.length-2,offset.length);--%>
<%--				hoff = parseInt(hoff, 10);--%>
<%--				moff = parseInt(moff, 10);--%>
<%----%>
<%--				--%>
<%--				if (offset.substring(0,1) == '+') {--%>
<%--					console.log(moff+parseInt(time[1], 10) > 59);--%>
<%--					console.log(hoff+parseInt(time[0]) > 23);--%>
<%--					--%>
<%--					if (moff+parseInt(time[1], 10) > 59) {--%>
<%--					}--%>
<%--					if (hoff+parseInt(time[0]) > 23) {--%>
<%--					}--%>
<%--				} else if (offset.substring(0,1) == '-') {--%>
<%--					console.log(parseInt(time[1], 10)-moff < 0);--%>
<%--					console.log(parseInt(time[0])-hoff < 0);--%>
<%--					--%>
<%--					if (parseInt(time[1], 10)-moff < 0) {--%>
<%--					}--%>
<%--					if (parseInt(time[0])-hoff < 0) {--%>
<%--					}--%>
<%--				}--%>
				

				$('#'+id+'_year').attr('value', valArr[2]);
				$('#'+id+'_month').attr('value', mon);
				$('#'+id+'_day').attr('value', valArr[0]);
				$('#'+id+'_hour').attr('value', time[0]);
				$('#'+id+'_minute').attr('value', time[1]);

				//console.log('#'+id+'_month');
			}
			
			

			if (document.getElementById('prj-beginDate') != null) {

				AnyTime.picker( "prj-beginDate",
					{
						format: "%d %b %Y %H:%i",
						firstDOW: 0,
						labelTitle: "Beginning Date/Time"
					} 
			    );
				AnyTime.picker( "prj-endDate",
					{
						format: "%d %b %Y %H:%i",
						firstDOW: 0,
						labelTitle: "Ending Date/Time"
					} 
			    );
			} else if (document.getElementById('ds-beginDate') != null) {
				
				/* All TimeZone-related things */
				AnyTime.picker( "ds-beginDate",
					{
						format: "%d %b %Y %H:%i (%+)",
						formatUtcOffset: "%+ (%@)",
						//format: "%d %b %Y %H:%i",
						firstDOW: 0,
						labelTitle: "Beginning Date/Time"
					} 
			    );
				AnyTime.picker( "ds-endDate",
					{
						format: "%d %b %Y %H:%i (%+)",
						formatUtcOffset: "%+ (%@)",
						//format: "%d %b %Y %H:%i",
						firstDOW: 0,
						labelTitle: "Ending Date/Time"
					} 
			    );
			}
		</script>