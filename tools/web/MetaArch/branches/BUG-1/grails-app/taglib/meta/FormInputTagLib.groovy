package meta

class FormInputTagLib {

	static namespace = "meta"
	
	def dateTimeInputter = { attrs ->
		/* modified datePicker code snippets to behave better for user interface */
		def sb = new StringBuilder()
		
		def value = attrs.value
		if (value.toString() == 'none') {
			value = null
		}
		
		def name = attrs.name
		def id = attrs.id ?: name
		
		final PRECISION_RANKINGS = ["year": 0, "month": 10, "day": 20, "hour": 30, "minute": 40, "second": 50]
		def precision = (attrs.precision ? PRECISION_RANKINGS[attrs.precision] : PRECISION_RANKINGS["minute"])
		
		//final MONTHS = [ 'Jan': '01', 'Feb': '02', 'Mar': '03', 'Apr': '04', 'May': '05', 'Jun': '06', 'Jul': '07', 'Aug': '08', 'Sep': '09', 'Oct': '10', 'Nov': '11', 'Dec': '12' ]
		final MONTHS = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
		
		def day
		def month
		def year
		def hour
		def minute
		
		def c = null
		if (value instanceof Calendar) {
			c = value
		} else if (value != null) {
			c = new GregorianCalendar()
			c.setTime(value)
		}

		if (c != null) {
			day = c.get(GregorianCalendar.DAY_OF_MONTH)
			month = c.get(GregorianCalendar.MONTH)
			year = c.get(GregorianCalendar.YEAR)
			hour = c.get(GregorianCalendar.HOUR_OF_DAY)
			minute = c.get(GregorianCalendar.MINUTE)
		}
		
		sb << "<input type=\"hidden\" name=\"${name}\" value=\"date.struct\" />"
		if (precision >= PRECISION_RANKINGS["year"]) {
			sb << "<input type=\"hidden\" name=\"${name}_year\" id=\"${name}_year\" title=\"year\" value=\"${year}\" size=\"4\" maxlength=\"4\" />"
		}
		if (precision >= PRECISION_RANKINGS["month"]) {
			sb << "<input type=\"hidden\" name=\"${name}_month\" id=\"${name}_month\" title=\"month\" value=\"${month+1}\" size=\"2\" maxlength=\"2\" />"
		}
		if (precision >= PRECISION_RANKINGS["day"]) {
			sb << "<input type=\"hidden\" name=\"${name}_day\" id=\"${name}_day\" title=\"day\" value=\"${day}\" size=\"2\" maxlength=\"2\" />"
		}
		if (precision >= PRECISION_RANKINGS["hour"]) {
			sb << "<input type=\"hidden\" name=\"${name}_hour\" id=\"${name}_hour\" title=\"hour\" value=\"${hour}\" size=\"2\" maxlength=\"2\" />"
		}
		if (precision >= PRECISION_RANKINGS["minute"]) {
			sb << "<input type=\"hidden\" name=\"${name}_minute\" id=\"${name}_minute\" title=\"minute\" value=\"${minute}\" size=\"2\" maxlength=\"2\" />"
		}
		if (precision >= PRECISION_RANKINGS["second"]) {
			sb << "<input type=\"hidden\" name=\"${name}_second\" id=\"${name}_second\" title=\"second\" value=\"${second}\" size=\"2\" maxlength=\"2\" />"
		}
		
		
		day = (day < 10 ? '0'+day : day)
		hour = (hour < 10 ? '0'+hour : hour)
		minute = (minute < 10 ? '0'+minute : minute)
		month = MONTHS[month]
		
		sb << "<input type=\"text\" id=\"${attrs.domain}-${name}\" value=\"${day} ${month} ${year} ${hour}:${minute}"
		if (attrs.domain != null && attrs.domain == 'ds') { sb << " (+0000)" }
		sb << "\" onblur=\"updateInputter(this);\" />"
		
		
		/*
			<input type="text" id="prjBeginDate" value="" onblur="document.getElementById('beginDate-input').value = new Date(this.value+' UTC');" />	
			<input type="hidden" id="beginDate-input" value="${g.formatDate(format: "yyyy-MM-dd HH:mm Z", date: projectInstance?.beginDate)}" />
			<input type="hidden" name="beginDate" value="struct">
			<input type="hidden" name="beginDate_day" id="beginDate_day" value="">
			<input type="hidden" name="beginDate_month" id="beginDate_month" value="">
			<input type="hidden" name="beginDate_year" id="beginDate_year" value="">
			<input type="hidden" name="beginDate_hour" id="beginDate_hour" value="">
			<input type="hidden" name="beginDate_minute" id="beginDate_minute" value="">
		 */
		
		out << sb as String
	}
	
	
	def timeZoneInputter = { attrs ->
		def sb = new StringBuilder()
		
		sb = """
				<select name="${attrs.name}" id="${attrs.name}">
					<option value="-1200">Baker Island Time  -- BIT</option>
					<option value="-1130">Niue Time  -- NUT</option>
					<option value="-1100">Samoa Standard Time  -- SST</option>
					<option value="-1000">Cook Island Time  -- CKT</option>
					<option value="-1000">Hawaii-Aleutian Standard Time  -- HAST</option>
					<option value="-1000">Hawaii Standard Time  -- HST</option>
					<option value="-1000">Tahiti Time  -- TAHT</option>
					<option value="-0930">Marquesas Islands Time  -- MART</option>
					<option value="-0930">Marquesas Islands Time  -- MIT</option>
					<option value="-0900">Alaska Standard Time  -- AKST</option>
					<option value="-0900">Gambier Islands  -- GAMT</option>
					<option value="-0900">Gambier Island Time  -- GIT</option>
					<option value="-0900">Hawaii-Aleutian Daylight Time  -- HADT</option>
					<option value="-0800">Alaska Daylight Time  -- AKDT</option>
					<option value="-0800">Choibalsan  -- CHOT</option>
					<option value="-0800">Clipperton Island Standard Time  -- CIST</option>
					<option value="-0800">Pacific Standard Time (North America)  -- PST</option>
					<option value="-0700">Mountain Standard Time (North America)  -- MST</option>
					<option value="-0700">Pacific Daylight Time (North America)  -- PDT</option>
					<option value="-0600">Central Standard Time (North America)  -- CST</option>
					<option value="-0600">Easter Island Standard Time  -- EAST</option>
					<option value="-0600">Galapagos Time  -- GALT</option>
					<option value="-0600">Mountain Daylight Time (North America)  -- MDT</option>
					<option value="-0500">Central Daylight Time (North America)  -- CDT</option>
					<option value="-0500">Colombia Time  -- COT</option>
					<option value="-0500">Cuba Standard Time  -- CST</option>
					<option value="-0500">Easter Island Standard Summer Time  -- EASST</option>
					<option value="-0500">Ecuador Time  -- ECT</option>
					<option value="-0500">Eastern Standard Time (North America)  -- EST</option>
					<option value="-0500">Peru Time  -- PET</option>
					<option value="-0430">Venezuelan Standard Time  -- VET</option>
					<option value="-0400">Atlantic Standard Time  -- AST</option>
					<option value="-0400">Bolivia Time  -- BOT</option>
					<option value="-0400">Cuba Daylight Time  -- CDT</option>
					<option value="-0400">Chile Standard Time  -- CLT</option>
					<option value="-0400">Colombia Summer Time  -- COST</option>
					<option value="-0400">Eastern Caribbean Time  -- ECT</option>
					<option value="-0400">Eastern Daylight Time (North America)  -- EDT</option>
					<option value="-0400">Falkland Islands Time  -- FKT</option>
					<option value="-0400">Guyana Time  -- GYT</option>
					<option value="-0330">Newfoundland Standard Time  -- NST</option>
					<option value="-0330">Newfoundland Time  -- NT</option>
					<option value="-0300">Atlantic Daylight Time  -- ADT</option>
					<option value="-0300">Argentina Time  -- ART</option>
					<option value="-0300">Brasilia Time  -- BRT</option>
					<option value="-0300">Chile Summer Time  -- CLST</option>
					<option value="-0300">Falkland Islands Summer Time  -- FKST</option>
					<option value="-0300">French Guiana Time  -- GFT</option>
					<option value="-0300">Saint Pierre and Miquelon Standard Time  -- PMST</option>
					<option value="-0300">Rothera Research Station Time  -- ROTT</option>
					<option value="-0300">Suriname Time  -- SRT</option>
					<option value="-0300">Uruguay Standard Time  -- UYT</option>
					<option value="-0230">Newfoundland Daylight Time  -- NDT</option>
					<option value="-0200">Fernando de Noronha Time  -- FNT</option>
					<option value="-0200">South Georgia and the South Sandwich Islands  -- GST</option>
					<option value="-0200">Saint Pierre and Miquelon Daylight time  -- PMDT</option>
					<option value="-0200">Uruguay Summer Time  -- UYST</option>
					<option value="-0100">Azores Standard Time  -- AZOST</option>
					<option value="-0100">Cape Verde Time  -- CVT</option>
					<option value="-0100">Eastern Greenland Time  -- EGT</option>
					<option value="+0000">Greenwich Mean Time  -- GMT</option>
					<option value="+0000">Coordinated Universal Time  -- UTC</option>
					<option value="+0000">Western European Time  -- WET</option>
					<option value="+0000">Eastern Greenland Summer Time  -- EGST</option>
					<option value="+0100">British Summer Time  -- BST</option>
					<option value="+0100">Central European Time  -- CET</option>
					<option value="+0100">AIX specific equivalent of Central European Time  -- DFT</option>
					<option value="+0100">Irish Standard Time  -- IST</option>
					<option value="+0100">Middle European Time  -- MET</option>
					<option value="+0100">West Africa Time  -- WAT</option>
					<option value="+0100">Western European Daylight Time  -- WEDT</option>
					<option value="+0100">Western European Summer Time  -- WEST</option>
					<option value="+0200">Central Africa Time  -- CAT</option>
					<option value="+0200">Central European Daylight Time  -- CEDT</option>
					<option value="+0200">Central European Summer Time  -- CEST</option>
					<option value="+0200">Eastern European Time  -- EET</option>
					<option value="+0200">Heure Avancée d'Europe Centrale (CEST)  -- HAEC</option>
					<option value="+0200">Israel Standard Time  -- IST</option>
					<option value="+0200">Middle European Saving Time  -- MEST</option>
					<option value="+0200">South African Standard Time  -- SAST</option>
					<option value="+0200">West Africa Summer Time  -- WAST</option>
					<option value="+0300">Arabia Standard Time  -- AST</option>
					<option value="+0300">East Africa Time  -- EAT</option>
					<option value="+0300">Eastern European Daylight Time  -- EEDT</option>
					<option value="+0300">Eastern European Summer Time  -- EEST</option>
					<option value="+0300">Further-eastern European Time  -- FET</option>
					<option value="+0300">Israel Daylight Time  -- IDT</option>
					<option value="+0300">Indian Ocean Time  -- IOT</option>
					<option value="+0300">Showa Station Time  -- SYOT</option>
					<option value="+0330">Iran Standard Time  -- IRST</option>
					<option value="+0400">Armenia Time  -- AMT</option>
					<option value="+0400">Azerbaijan Time  -- AZT</option>
					<option value="+0400">Georgia Standard Time  -- GET</option>
					<option value="+0400">Gulf Standard Time  -- GST</option>
					<option value="+0400">Moscow Time  -- MSK</option>
					<option value="+0400">Mauritius Time  -- MUT</option>
					<option value="+0400">Réunion Time  -- RET</option>
					<option value="+0400">Samara Time  -- SAMT</option>
					<option value="+0400">Seychelles Time  -- SCT</option>
					<option value="+0400">Volgograd Time  -- VOLT</option>
					<option value="+0430">Afghanistan Time  -- AFT</option>
					<option value="+0500">Armenia Summer Time  -- AMST</option>
					<option value="+0500">Heard and McDonald Islands Time  -- HMT</option>
					<option value="+0500">Mawson Station Time  -- MAWT</option>
					<option value="+0500">Maldives Time  -- MVT</option>
					<option value="+0500">Oral Time  -- ORAT</option>
					<option value="+0500">Pakistan Standard Time  -- PKT</option>
					<option value="+0500">Indian/Kerguelen  -- TFT</option>
					<option value="+0500">Tajikistan Time  -- TJT</option>
					<option value="+0500">Turkmenistan Time  -- TMT</option>
					<option value="+0500">Uzbekistan Time  -- UZT</option>
					<option value="+0500">Yekaterinburg Time  -- YEKT</option>
					<option value="+0530">Indian Standard Time  -- IST</option>
					<option value="+0530">Sri Lanka Time  -- SLT</option>
					<option value="+05:4500">Nepal Time  -- NPT</option>
					<option value="+0600">British Indian Ocean Time  -- BIOT</option>
					<option value="+0600">Bangladesh Standard Time  -- BST</option>
					<option value="+0600">Bhutan Time  -- BTT</option>
					<option value="+0600">Kyrgyzstan time  -- KGT</option>
					<option value="+0600">Omsk Time  -- OMST</option>
					<option value="+0600">Vostok Station Time  -- VOST</option>
					<option value="+0630">Cocos Islands Time  -- CCT</option>
					<option value="+0630">Myanmar Time  -- MMT</option>
					<option value="+0630">Myanmar Standard Time  -- MST</option>
					<option value="+0700">Christmas Island Time  -- CXT</option>
					<option value="+0700">Davis Time  -- DAVT</option>
					<option value="+0700">Khovd Time  -- HOVT</option>
					<option value="+0700">Indochina Time  -- ICT</option>
					<option value="+0700">Krasnoyarsk Time  -- KRAT</option>
					<option value="+0700">Thailand Standard Time  -- THA</option>
					<option value="+0800">ASEAN Common Time  -- ACT</option>
					<option value="+0800">Australian Western Standard Time  -- AWST</option>
					<option value="+0800">Brunei Time  -- BDT</option>
					<option value="+0800">Central Indonesia Time  -- CIT</option>
					<option value="+0800">China Standard Time  -- CST</option>
					<option value="+0800">China time  -- CT</option>
					<option value="+0800">Hong Kong Time  -- HKT</option>
					<option value="+0800">Iran Daylight Time  -- IRDT</option>
					<option value="+0800">Malaysia Standard Time  -- MST</option>
					<option value="+0800">Malaysia Time  -- MYT</option>
					<option value="+0800">Philippine Time  -- PHT</option>
					<option value="+0800">Singapore Time  -- SGT</option>
					<option value="+0800">Singapore Standard Time  -- SST</option>
					<option value="+0800">Ulaanbaatar Time  -- ULAT</option>
					<option value="+0800">Western Standard Time  -- WST</option>
					<option value="+08:4500">Central Western Standard Time (Australia)  -- CWST</option>
					<option value="+0900">Australian Western Daylight Time  -- AWDT</option>
					<option value="+0900">Eastern Indonesian Time  -- EIT</option>
					<option value="+0900">Irkutsk Time  -- IRKT</option>
					<option value="+0900">Japan Standard Time  -- JST</option>
					<option value="+0900">Korea Standard Time  -- KST</option>
					<option value="+0900">Timor Leste Time  -- TLT</option>
					<option value="+0900">Yakutsk Time  -- YAKT</option>
					<option value="+0930">Australian Central Standard Time  -- ACST</option>
					<option value="+0930">Central Standard Time (Australia)  -- CST</option>
					<option value="+1000">Australian Eastern Standard Time  -- AEST</option>
					<option value="+1000">Chamorro Standard Time  -- ChST</option>
					<option value="+1000">Chuuk Time  -- CHUT</option>
					<option value="+1000">Dumont d'Urville Time  -- DDUT</option>
					<option value="+1000">Eastern Standard Time (Australia)  -- EST</option>
					<option value="+1000">Papua New Guinea Time  -- PGT</option>
					<option value="+1000">Vladivostok Time  -- VLAT</option>
					<option value="+1030">Australian Central Daylight Time  -- ACDT</option>
					<option value="+1030">Central Summer Time (Australia)  -- CST</option>
					<option value="+1030">Lord Howe Standard Time  -- LHST</option>
					<option value="+1100">Australian Eastern Daylight Time  -- AEDT</option>
					<option value="+1100">Kosrae Time  -- KOST</option>
					<option value="+1100">Lord Howe Summer Time  -- LHST</option>
					<option value="+1100">Macquarie Island Station Time  -- MIST</option>
					<option value="+1100">New Caledonia Time  -- NCT</option>
					<option value="+1100">Pohnpei Standard Time  -- PONT</option>
					<option value="+1100">Sakhalin Island time  -- SAKT</option>
					<option value="+1100">Solomon Islands Time  -- SBT</option>
					<option value="+1100">Vanuatu Time  -- VUT</option>
					<option value="+1130">Norfolk Time  -- NFT</option>
					<option value="+1200">Fiji Time  -- FJT</option>
					<option value="+1200">Gilbert Island Time  -- GILT</option>
					<option value="+1200">Magadan Time  -- MAGT</option>
					<option value="+1200">Marshall Islands  -- MHT</option>
					<option value="+1200">New Zealand Standard Time  -- NZST</option>
					<option value="+1200">Kamchatka Time  -- PETT</option>
					<option value="+1200">Tuvalu Time  -- TVT</option>
					<option value="+1200">Wake Island Time  -- WAKT</option>
					<option value="+1245">Chatham Standard Time  -- CHAST</option>
					<option value="+1300">New Zealand Daylight Time  -- NZDT</option>
					<option value="+1300">Phoenix Island Time  -- PHOT</option>
					<option value="+1300">Tonga Time  -- TOT</option>
					<option value="+1345">Chatham Daylight Time  -- CHADT</option>
					<option value="+1400">Line Islands Time  -- LINT</option>
					<option value="+1400">Tokelau Time  -- TKT</option>
				</select>"""
		
		out << sb as String
	}
	
	/* EXAMPLE:		<meta:tooltip domain="dataset" prop="summary" /> */
	def tooltip = { attrs ->
		def sb = new StringBuilder()
		def helptipInstance = Tooltip.findByDomainAndProperty(attrs.domain, attrs.prop)
		
		/*
		 	<a id="116_Helptip_icon" rel="116" class="empty helptips_icon helptips_icon_tip" 
		 		onclick="$('#116_Helptip').slideToggle()" title="" bt-xtitle="[object Object]"></a>
	 		<div id="116_Helptip" style="position: relative; display: none;" class="helptips_div">
	 			<div class="ui-dialog ui-widget">
	 				<div class="ui-dialog-titlebar ui-widget-header">
	 					${helptipInstance?.title}
	 					<br>
	 					<span class="normal">(${helptipInstance?.domain}.${helptipInstance?.property})</span>
	 				</div>
	 				<div class="ui-dialog-content ui-widget-content">
	 					<p>${helptipInstance?.description}</p>
	 				</div>
	 			</div>
	 		</div>
	 		
	 		-----------------------------------
	 		CSS WAY:
	 		-----------------------------------
	 		<a class="helptip" href="#">
	 			<img src="/metaarch/static/images/skin/help.png" alt="Help" height="16" width="16">
		 		<span class="custom help">
		 			<img src="/metaarch/static/images/skin/help.png" alt="Help" height="32" width="32">
		 			<em>${helptipInstance?.title}</em>
		 			${helptipInstance?.description}
		 		</span>
	 		</a>
	 	 */
		
		if (helptipInstance != null) {
			sb = """
				<a class="helptip" href="#">
		 			<img src="${resource(dir: 'images/skin', file: 'help.png')}" alt="Help" height="16" width="16">
			 		<span class="custom help">
			 			<img src="${resource(dir: 'images/skin', file: 'help.png')}" alt="Help" height="32" width="32">
			 			<em>${helptipInstance.title}</em>
			 			${helptipInstance.description}
			 		</span>
		 		</a>"""
		} else {
			sb = ""
		}
		
		out << sb as String
	}
}
