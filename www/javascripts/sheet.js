
function checkboxClick(element) {
    var tr = getParent(element,'tr');

    if (tr.className.indexOf("selected") < 0) {
	tr.className += " selected";
	checkChildCheckboxes(tr, true);
	return true;	
    }
    else {
	tr.className = tr.className.replace("selected", ""); 
	checkChildCheckboxes(tr, false);
	return true;
    }
}

function doLink(element) {
    var tr = getParent(element,'tr');
    var aes = tr.getElementsByTagName("a");
    document.location.href = aes[0].href;
}

	function toggleBlock(blockID) {
		var blockID = document.getElementById(blockID);

		if(blockID.style.display == '') {
			blockID.style.display = 'none';
		} else {
			blockID.style.display = '';
		}
	}

/**
	* Cross-browser event handling for IE5+,  NS6 and Mozilla
	* By Scott Andrew
	*
	*/
	function addEvent(elm, evType, fn, useCapture)	{
	  if (elm.addEventListener){
		elm.addEventListener(evType, fn, useCapture);
		return true;
	  } else if (elm.attachEvent){
		var r = elm.attachEvent("on"+evType, fn);
		return r;
	  } else {
		alert("Handler could not be removed");
	  }
	} 
	
	/**
	* Gets nearest parent (grandparent, etc.) with tagname pTagName
	*/
	function getParent(el, pTagName) {
		if (el == null) return null;
		else if (el.nodeType == 1 && el.tagName.toLowerCase() == pTagName.toLowerCase())	// Gecko bug, supposed to be uppercase
			return el;
		else
			return getParent(el.parentNode, pTagName);
	}
	
	
	
	/**
	* BEGIN TABLE SORTING FUNCTIONALITY
	*/
	
	var SORT_COLUMN_INDEX;
	
	/**
	* Goes through all document tables and calls ts_makeSortable
	* on any table with a class of "sortable".
	*/
	function sortables_init() {
		// Find all tables with class sortable and make them sortable
		if (!document.getElementsByTagName) return;
		tbls = document.getElementsByTagName("table");
		for (ti=0;ti<tbls.length;ti++) {
			thisTbl = tbls[ti];
			if (((' '+thisTbl.className+' ').indexOf("sortable") != -1) && (thisTbl.id)) {
				//initTable(thisTbl.id);
				ts_makeSortable(thisTbl);
			}
		}
	}
	
	/**
	* Adds events and styles to handle sorting behavior.
	*/
	function ts_makeSortable(table) {
		if (table.rows && table.rows.length > 0) {
			var firstRow = table.rows[0];
		}
		if (!firstRow) return;
		
		// We have a first row: assume it's the header, and make its contents clickable links
		for (var i=0;i<firstRow.cells.length;i++) {
			var cell = firstRow.cells[i];
			if (cell.className.indexOf("sortable") < 0) {
				continue;
			}
			var txt = ts_getInnerText(cell);
			cell.innerHTML = '<a href="#" class="sortheader" '+ 
			'onclick="ts_resortTable(this, '+i+');stripe();return false;">' + 
			txt+'<span class="sortarrow">&nbsp;&nbsp;&nbsp;</span></a>';
		}
	}
	
	/**
	* Returns the inner text of an element.
	*/
	function ts_getInnerText(el) {
		if (typeof el == "string") return el;
		if (typeof el == "undefined") { return el };
		if (el.innerText) return el.innerText;	//Not needed but it is faster
		var str = "";
		
		var cs = el.childNodes;
		var l = cs.length;
		for (var i = 0; i < l; i++) {
			switch (cs[i].nodeType) {
				case 1: //ELEMENT_NODE
					str += ts_getInnerText(cs[i]);
					break;
				case 3:	//TEXT_NODE
					str += cs[i].nodeValue;
					break;
			}
		}
		return str;
	}
	
	
	/**
	* Calls a sort function a table column for each table body.
	*/
	function ts_resortTable(lnk,clid) {
		// get the span
		var span;
		for (var ci=0;ci<lnk.childNodes.length;ci++) {
			if (lnk.childNodes[ci].tagName && lnk.childNodes[ci].tagName.toLowerCase() == 'span') {
				span = lnk.childNodes[ci];
			}
		}
		
		var spantext = ts_getInnerText(span);
		var td = lnk.parentNode;
		var column = clid || td.cellIndex;
		var table = getParent(td,'table');
		
		// Work out a type for the column
		if (table.rows.length <= 1) return;
		var itm = ts_getInnerText(table.rows[1].cells[column]);
		sortfn = ts_sort_caseinsensitive;
	
		//ts_sort_date currently only handles these two formats
		if (itm.match(/^\d\d[\/-]\d\d[\/-]\d\d\d\d$/)) sortfn = ts_sort_date;
		if (itm.match(/^\d\d[\/-]\d\d[\/-]\d\d$/)) sortfn = ts_sort_date;
		
		if (itm.match(/^[£$]/)) sortfn = ts_sort_currency;
		if (itm.match(/^[\d\.]+$/)) sortfn = ts_sort_numeric;
		if (itm.match(/^(-)?[\d\.]+$/)) sortfn = ts_sort_numeric;
		SORT_COLUMN_INDEX = column;
		
		
		var tbodies = table.getElementsByTagName("tbody");
		for (var h = 0; h < tbodies.length; h++) {
			var tableBody = tbodies[h];
			var newRows = new Array();
			
		
			var trs = tableBody.getElementsByTagName("tr");
	
			for (var i = 0; i < trs.length; i++) {
				newRows[i] = trs[i]; 
			}
			newRows.sort(sortfn);
	
	
			if (span.getAttribute("sortdir") == 'down') {
				newRows.reverse();
			}
			
			// We appendChild rows that already exist to the tbody, so it moves them rather than creating new ones
			// don't do sortbottom rows
			for (var i=0;i<newRows.length;i++) { 
				if (!newRows[i].className || (newRows[i].className && (newRows[i].className.indexOf('sortbottom') == -1))) {
					tableBody.appendChild(newRows[i]);
				}
			}
		
			// do sortbottom rows only
			for (var i=0;i<newRows.length;i++) { 
				if (newRows[i].className && (newRows[i].className.indexOf('sortbottom') != -1)) { 
					tableBody.appendChild(newRows[i]);
				}
			}
	
	
		}

	
		if (span.getAttribute("sortdir") == 'down') {
			ARROW = '&nbsp;&nbsp;&uarr;';
			span.setAttribute('sortdir','up');
		} else {
			ARROW = '&nbsp;&nbsp;&darr;';
			span.setAttribute('sortdir','down');
		}
		
		// Delete any other arrows there may be showing
		var allspans = document.getElementsByTagName("span");
		for (var ci=0;ci<allspans.length;ci++) {
			if (allspans[ci].className == 'sortarrow') {
				if (getParent(allspans[ci],"table") == getParent(lnk,"table")) { // in the same table as us?
					allspans[ci].innerHTML = '&nbsp;&nbsp;&nbsp;';
				}
			}
		}
			
		span.innerHTML = ARROW;
	}
	
	
	/**
	* Sorts strings as dates.
	*/
	function ts_sort_date(a,b) {
		// y2k notes: two digit years less than 50 are treated as 20XX, greater than 50 are treated as 19XX
		aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]);
		bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]);
		if (aa.length == 10) {
			dt1 = aa.substr(6,4)+aa.substr(3,2)+aa.substr(0,2);
		} else {
			yr = aa.substr(6,2);
			if (parseInt(yr) < 50) { yr = '20'+yr; } else { yr = '19'+yr; }
			dt1 = yr+aa.substr(3,2)+aa.substr(0,2);
		}
		if (bb.length == 10) {
			dt2 = bb.substr(6,4)+bb.substr(3,2)+bb.substr(0,2);
		} else {
			yr = bb.substr(6,2);
			if (parseInt(yr) < 50) { yr = '20'+yr; } else { yr = '19'+yr; }
			dt2 = yr+bb.substr(3,2)+bb.substr(0,2);
		}
		if (dt1==dt2) return 0;
		if (dt1<dt2) return -1;
		return 1;
	}
	
	/**
	* Sorts strings as an amount of currency.
	*/
	function ts_sort_currency(a,b) { 
		aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
		bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
		return parseFloat(aa) - parseFloat(bb);
	}
	
	/**
	* Sorts strings numerically.
	*/
	function ts_sort_numeric(a,b) { 
		aa = parseFloat(ts_getInnerText(a.cells[SORT_COLUMN_INDEX]));
		if (isNaN(aa)) aa = 0;
		bb = parseFloat(ts_getInnerText(b.cells[SORT_COLUMN_INDEX])); 
		if (isNaN(bb)) bb = 0;
		return aa-bb;
	}
	
	/**
	* Sorts strings case insensitively.
	*/
	function ts_sort_caseinsensitive(a,b) {
		aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]).toLowerCase();
		bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]).toLowerCase();
		if (aa==bb) return 0;
		if (aa<bb) return -1;
		return 1;
	}
	
	/**
	* Sorts strings by the default sort behavior.
	*/
	function ts_sort_default(a,b) {
		aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]);
		bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]);
		if (aa==bb) return 0;
		if (aa<bb) return -1;
		return 1;
	}



	/**
	* BEGIN ZEBRA STRIPING AND SELECTION/SUBMISSION CODE
	*/


	/**
	* Performs a "zebra striping" of all tables with the class name of "striped".
	*/
	var stripe = function() {
		var tables = document.getElementsByTagName("table");	

		for(var x=0;x!=tables.length;x++){
			var table = tables[x];
			if (! table) { return; }
			
			if (table.className.indexOf("striped") < 0) {
				continue;
			}
			
			var tbodies = table.getElementsByTagName("tbody");
			var even = true;
			for (var h = 0; h < tbodies.length; h++) {
				
				var trs = tbodies[h].getElementsByTagName("tr");
						
				
				for (var i = 0; i < trs.length; i++) {
					if(even) {
						if (trs[i].className.indexOf("even") < 0) {
							trs[i].className += " even";
						}
					}
					else {
						trs[i].className = trs[i].className.replace("even", "");
					}
					
					even = !even;
				}
			}
		}
	}
	
	
	/**
	* Gets all rows within the table body of a table with class name "selectable"
	* and allows them to be selected.  They highlight on mouseover and are selected on click.
	*
	* Since these rows are selectable, the function also tries to make the table
	* submittable.
	* SEE: makeTableSubmittable(table);
	*/
	var makeSelectable = function() {
		var tables = document.getElementsByTagName("table");	



		for(var x=0;x!=tables.length;x++){
			var table = tables[x];
			if (! table) { return; }
	
			hideElements(table.getElementsByTagName("td"));			
	
			if (table.className.indexOf("selectable") < 0) {
				continue;
			}
			
			var tbodies = table.getElementsByTagName("tbody");
			
			for (var h = 0; h < tbodies.length; h++) {

				var trs = tbodies[h].getElementsByTagName("tr");
				
				for (var i = 0; i < trs.length; i++) {
					if (trs[i].className.indexOf("disabled") >= 0) {
						//disableChildCheckBoxes(this);
						continue;
					}
				
							
					if(isChecked(trs[i])) {
						if (trs[i].className.indexOf("selected") < 0) {
							trs[i].className += " selected";
						}
					}
				
					trs[i].onmouseover=function(){
						this.className += " ruled"; return false
					}
					trs[i].onmouseout=function(){
						this.className = this.className.replace("ruled", ""); return false
					}

				}
			}

			makeTableSubmittable(table);

		}
	}


	
	/**
	* Takes a list of elements and an optional input type (for 'input' elements)
	* and sets their display to none.  If the inputType parameter is set, then it
	* will check to make sure that the type matches.
	*/
	var hideElements = function(els, inputType) {
		for (var i=0; i<els.length; i++) {
			var isRightType = true;
			if (inputType != 'undefined' && els[i].type != inputType) {
				isRightType = false;
			}
			if (isRightType && els[i].className.indexOf("hideme") >= 0) {
				els[i].style.display = 'none';
			}
		}
	}
	
	
	/**
	* Searches the element passed in (a tr in our case) for any child input elements 
	* which are checkboxes.  If it finds one that is checked, it returns true.
	*/
	var isChecked = function(el) {
		var inpts = el.getElementsByTagName("input");
		for (var n=0; n<inpts.length; n++) {
			if (inpts[n].getAttribute("type") == "checkbox") {
				return (inpts[n].checked);
			}
		}
		return false;
	}
	
	
	/**
	* Gets all checkboxes which are a child of the element passed in and sets
	* their "checked" value to the parameter chk.
	*/
	var checkChildCheckboxes = function(el, chk) {
		var inpts = el.getElementsByTagName("input");
		for (var n=0; n<inpts.length; n++) {
			if (inpts[n].getAttribute("type") == "checkbox") {
				inpts[n].checked = chk;
			}
		}
	}
	

	/**
	* Gets forms within the table passed in.
	* Makes any button with a class of "select_all" highlight all rows.
	* Makes any button with a class of "select_none" unhighlight all rows.
	*
	* SEE: selectAllRows(table, highlight);
	*/
	var makeTableSubmittable = function(table) {

		var inputs = table.getElementsByTagName("input");
		for (var h = 0; h < inputs.length; h++) {
			if (inputs[h].type == "button") {
				if (inputs[h].className.indexOf("select_all") >= 0) {
					inputs[h].onclick = function() {selectAllRows(table, true);}
				}
				if (inputs[h].className.indexOf("select_none") >= 0) {
					inputs[h].onclick = function() {selectAllRows(table, false);}
				}				
			}
		}
	}



	/**
	* Will select or deselect all of the rows in the table specified.
	*
	*/
	var selectAllRows = function(table, highlight) { 

			if (! table) { return; }
			
			if (table.className.indexOf("striped") < 0) {
				return;
			}
			
			var tbodies = table.getElementsByTagName("tbody");
			
			for (var h = 0; h < tbodies.length; h++) {
				var trs = tbodies[h].getElementsByTagName("tr");
					for (var i = 0; i < trs.length; i++) {
					
						if (trs[i].className.indexOf("disabled") >= 0) {
							continue;
						}

						if (highlight) {
								if (trs[i].className.indexOf("selected") < 0) {
									trs[i].onclick();
								}
						}
						else {
								if (trs[i].className.indexOf("selected") >= 0) {
									trs[i].onclick();
								}
						}
					}
			}			
	}




addEvent(window, "load", sortables_init);
addEvent(window, "load", stripe);
addEvent(window, "load", makeSelectable);

var datePickerDivID = "datepicker";
var iFrameDivID = "datepickeriframe";

var dayArrayShort = new Array('Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa');
var dayArrayMed = new Array('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
var dayArrayLong = new Array('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
var monthArrayShort = new Array('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var monthArrayMed = new Array('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec');
var monthArrayLong = new Array('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
 
// these variables define the date formatting we're expecting and outputting.
// If you want to use a different format by default, change the defaultDateSeparator
// and defaultDateFormat variables either here or on your HTML page.
var defaultDateSeparator = "/";        // common values would be "/" or "."
var defaultDateFormat = "mdy"    // valid values are "mdy", "dmy", and "ymd"
var dateSeparator = defaultDateSeparator;
var dateFormat = defaultDateFormat;

/**
This is the main function you'll call from the onClick event of a button.
Normally, you'll have something like this on your HTML page:

Start Date: <input name="StartDate">
<input type=button value="select" onclick="displayDatePicker('StartDate');">

That will cause the datepicker to be displayed beneath the StartDate field and
any date that is chosen will update the value of that field. If you'd rather have the
datepicker display beneath the button that was clicked, you can code the button
like this:

<input type=button value="select" onclick="displayDatePicker('StartDate', this);">

So, pretty much, the first argument (dateFieldName) is a string representing the
name of the field that will be modified if the user picks a date, and the second
argument (displayBelowThisObject) is optional and represents an actual node
on the HTML document that the datepicker should be displayed below.

In version 1.1 of this code, the dtFormat and dtSep variables were added, allowing
you to use a specific date format or date separator for a given call to this function.
Normally, you'll just want to set these defaults globally with the defaultDateSeparator
and defaultDateFormat variables, but it doesn't hurt anything to add them as optional
parameters here. An example of use is:

<input type=button value="select" onclick="displayDatePicker('StartDate', false, 'dmy', '.');">

This would display the datepicker beneath the StartDate field (because the
displayBelowThisObject parameter was false), and update the StartDate field with
the chosen value of the datepicker using a date format of dd.mm.yyyy
*/
function displayDatePicker(dateFieldName, displayBelowThisObject, dtFormat, dtSep)
{
  var targetDateField = document.getElementsByName (dateFieldName).item(0);
 
  // if we weren't told what node to display the datepicker beneath, just display it
  // beneath the date field we're updating
  if (!displayBelowThisObject)
    displayBelowThisObject = targetDateField;
 
  // if a date separator character was given, update the dateSeparator variable
  if (dtSep)
    dateSeparator = dtSep;
  else
    dateSeparator = defaultDateSeparator;
 
  // if a date format was given, update the dateFormat variable
  if (dtFormat)
    dateFormat = dtFormat;
  else
    dateFormat = defaultDateFormat;
 
  var x = displayBelowThisObject.offsetLeft;
  var y = displayBelowThisObject.offsetTop + displayBelowThisObject.offsetHeight ;
 
  // deal with elements inside tables and such
  var parent = displayBelowThisObject;
  while (parent.offsetParent) {
    parent = parent.offsetParent;
    x += parent.offsetLeft;
    y += parent.offsetTop ;
  }
 
  drawDatePicker(targetDateField, x, y);
}


/**
Draw the datepicker object (which is just a table with calendar elements) at the
specified x and y coordinates, using the targetDateField object as the input tag
that will ultimately be populated with a date.

This function will normally be called by the displayDatePicker function.
*/
function drawDatePicker(targetDateField, x, y)
{
  var dt = getFieldDate(targetDateField.value );
 
  // the datepicker table will be drawn inside of a <div> with an ID defined by the
  // global datePickerDivID variable. If such a div doesn't yet exist on the HTML
  // document we're working with, add one.
  if (!document.getElementById(datePickerDivID)) {
    // don't use innerHTML to update the body, because it can cause global variables
    // that are currently pointing to objects on the page to have bad references
    //document.body.innerHTML += "<div id='" + datePickerDivID + "' class='dpDiv'></div>";
    var newNode = document.createElement("div");
    newNode.setAttribute("id", datePickerDivID);
    newNode.setAttribute("class", "dpDiv");
    newNode.setAttribute("style", "visibility: hidden;");
    document.body.appendChild(newNode);
  }
 
  // move the datepicker div to the proper x,y coordinate and toggle the visiblity
  var pickerDiv = document.getElementById(datePickerDivID);
  pickerDiv.style.position = "absolute";
  pickerDiv.style.left = x + "px";
  pickerDiv.style.top = y + "px";
  pickerDiv.style.visibility = (pickerDiv.style.visibility == "visible" ? "hidden" : "visible");
  pickerDiv.style.display = (pickerDiv.style.display == "block" ? "none" : "block");
  pickerDiv.style.zIndex = 10000;
 
  // draw the datepicker table
  refreshDatePicker(targetDateField.name, dt.getFullYear(), dt.getMonth(), dt.getDate());
}


/**
This is the function that actually draws the datepicker calendar.
*/
function refreshDatePicker(dateFieldName, year, month, day)
{
  // if no arguments are passed, use today's date; otherwise, month and year
  // are required (if a day is passed, it will be highlighted later)
  var thisDay = new Date();
 
  if ((month >= 0) && (year > 0)) {
    thisDay = new Date(year, month, 1);
  } else {
    day = thisDay.getDate();
    thisDay.setDate(1);
  }
 
  // the calendar will be drawn as a table
  // you can customize the table elements with a global CSS style sheet,
  // or by hardcoding style and formatting elements below
  var crlf = "\r\n";
  var TABLE = "<table cols=7 class='dpTable'>" + crlf;
  var xTABLE = "</table>" + crlf;
  var TR = "<tr class='dpTR'>";
  var TR_title = "<tr class='dpTitleTR'>";
  var TR_days = "<tr class='dpDayTR'>";
  var TR_todaybutton = "<tr class='dpTodayButtonTR'>";
  var xTR = "</tr>" + crlf;
  var TD = "<td class='dpTD' onMouseOut='this.className=\"dpTD\";' onMouseOver=' this.className=\"dpTDHover\";' ";    // leave this tag open, because we'll be adding an onClick event
  var TD_title = "<td colspan=5 class='dpTitleTD'>";
  var TD_buttons = "<td class='dpButtonTD'>";
  var TD_todaybutton = "<td colspan=7 class='dpTodayButtonTD'>";
  var TD_days = "<td class='dpDayTD'>";
  var TD_selected = "<td class='dpDayHighlightTD' onMouseOut='this.className=\"dpDayHighlightTD\";' onMouseOver='this.className=\"dpTDHover\";' ";    // leave this tag open, because we'll be adding an onClick event
  var xTD = "</td>" + crlf;
  var DIV_title = "<div class='dpTitleText'>";
  var DIV_selected = "<div class='dpDayHighlight'>";
  var xDIV = "</div>";
 
  // start generating the code for the calendar table
  var html = TABLE;
 
  // this is the title bar, which displays the month and the buttons to
  // go back to a previous month or forward to the next month
  html += TR_title;
  html += TD_buttons + getButtonCode(dateFieldName, thisDay, -1, "&lt;") + xTD;
  html += TD_title + DIV_title + monthArrayLong[ thisDay.getMonth()] + " " + thisDay.getFullYear() + xDIV + xTD;
  html += TD_buttons + getButtonCode(dateFieldName, thisDay, 1, "&gt;") + xTD;
  html += xTR;
 
  // this is the row that indicates which day of the week we're on
  html += TR_days;
  for(i = 0; i < dayArrayShort.length; i++)
    html += TD_days + dayArrayShort[i] + xTD;
  html += xTR;
 
  // now we'll start populating the table with days of the month
  html += TR;
 
  // first, the leading blanks
  for (i = 0; i < thisDay.getDay(); i++)
    html += TD + "&nbsp;" + xTD;
 
  // now, the days of the month
  do {
    dayNum = thisDay.getDate();
    TD_onclick = " onclick=\"updateDateField('" + dateFieldName + "', '" + getDateString(thisDay) + "');\">";
    
    if (dayNum == day)
      html += TD_selected + TD_onclick + DIV_selected + dayNum + xDIV + xTD;
    else
      html += TD + TD_onclick + dayNum + xTD;
    
    // if this is a Saturday, start a new row
    if (thisDay.getDay() == 6)
      html += xTR + TR;
    
    // increment the day
    thisDay.setDate(thisDay.getDate() + 1);
  } while (thisDay.getDate() > 1)
 
  // fill in any trailing blanks
  if (thisDay.getDay() > 0) {
    for (i = 6; i > thisDay.getDay(); i--)
      html += TD + "&nbsp;" + xTD;
  }
  html += xTR;
 
  // add a button to allow the user to easily return to today, or close the calendar
  var today = new Date();
  var todayString = "Today is " + dayArrayMed[today.getDay()] + ", " + monthArrayMed[ today.getMonth()] + " " + today.getDate();
  html += TR_todaybutton + TD_todaybutton;
  html += "<button class='dpTodayButton' onClick='refreshDatePicker(\"" + dateFieldName + "\");'>this month</button> ";
  html += "<button class='dpTodayButton' onClick='updateDateField(\"" + dateFieldName + "\");'>close</button>";
  html += xTD + xTR;
 
  // and finally, close the table
  html += xTABLE;
 
  document.getElementById(datePickerDivID).innerHTML = html;
  // add an "iFrame shim" to allow the datepicker to display above selection lists
  adjustiFrame();
}


/**
Convenience function for writing the code for the buttons that bring us back or forward
a month.
*/
function getButtonCode(dateFieldName, dateVal, adjust, label)
{
  var newMonth = (dateVal.getMonth () + adjust) % 12;
  var newYear = dateVal.getFullYear() + parseInt((dateVal.getMonth() + adjust) / 12);
  if (newMonth < 0) {
    newMonth += 12;
    newYear += -1;
  }
 
  return "<button class='dpButton' onClick='refreshDatePicker(\"" + dateFieldName + "\", " + newYear + ", " + newMonth + ");'>" + label + "</button>";
}


/**
Convert a JavaScript Date object to a string, based on the dateFormat and dateSeparator
variables at the beginning of this script library.
*/
function getDateString(dateVal)
{
  var dayString = "00" + dateVal.getDate();
  var monthString = "00" + (dateVal.getMonth()+1);
  dayString = dayString.substring(dayString.length - 2);
  monthString = monthString.substring(monthString.length - 2);
 
  switch (dateFormat) {
    case "dmy" :
      return dayString + dateSeparator + monthString + dateSeparator + dateVal.getFullYear();
    case "ymd" :
      return dateVal.getFullYear() + dateSeparator + monthString + dateSeparator + dayString;
    case "mdy" :
    default :
      return monthString + dateSeparator + dayString + dateSeparator + dateVal.getFullYear();
  }
}


/**
Convert a string to a JavaScript Date object.
*/
function getFieldDate(dateString)
{
  var dateVal;
  var dArray;
  var d, m, y;
 
  try {
    dArray = splitDateString(dateString);
    if (dArray) {
      switch (dateFormat) {
        case "dmy" :
          d = parseInt(dArray[0], 10);
          m = parseInt(dArray[1], 10) - 1;
          y = parseInt(dArray[2], 10);
          break;
        case "ymd" :
          d = parseInt(dArray[2], 10);
          m = parseInt(dArray[1], 10) - 1;
          y = parseInt(dArray[0], 10);
          break;
        case "mdy" :
        default :
          d = parseInt(dArray[1], 10);
          m = parseInt(dArray[0], 10) - 1;
          y = parseInt(dArray[2], 10);
          break;
      }
      dateVal = new Date(y, m, d);
    } else if (dateString) {
      dateVal = new Date(dateString);
    } else {
      dateVal = new Date();
    }
  } catch(e) {
    dateVal = new Date();
  }
 
  return dateVal;
}


/**
Try to split a date string into an array of elements, using common date separators.
If the date is split, an array is returned; otherwise, we just return false.
*/
function splitDateString(dateString)
{
  var dArray;
  if (dateString.indexOf("/") >= 0)
    dArray = dateString.split("/");
  else if (dateString.indexOf(".") >= 0)
    dArray = dateString.split(".");
  else if (dateString.indexOf("-") >= 0)
    dArray = dateString.split("-");
  else if (dateString.indexOf("\\") >= 0)
    dArray = dateString.split("\\");
  else
    dArray = false;
 
  return dArray;
}

/**
Update the field with the given dateFieldName with the dateString that has been passed,
and hide the datepicker. If no dateString is passed, just close the datepicker without
changing the field value.

Also, if the page developer has defined a function called datePickerClosed anywhere on
the page or in an imported library, we will attempt to run that function with the updated
field as a parameter. This can be used for such things as date validation, setting default
values for related fields, etc. For example, you might have a function like this to validate
a start date field:

function datePickerClosed(dateField)
{
  var dateObj = getFieldDate(dateField.value);
  var today = new Date();
  today = new Date(today.getFullYear(), today.getMonth(), today.getDate());
 
  if (dateField.name == "StartDate") {
    if (dateObj < today) {
      // if the date is before today, alert the user and display the datepicker again
      alert("Please enter a date that is today or later");
      dateField.value = "";
      document.getElementById(datePickerDivID).style.visibility = "visible";
      adjustiFrame();
    } else {
      // if the date is okay, set the EndDate field to 7 days after the StartDate
      dateObj.setTime(dateObj.getTime() + (7 * 24 * 60 * 60 * 1000));
      var endDateField = document.getElementsByName ("EndDate").item(0);
      endDateField.value = getDateString(dateObj);
    }
  }
}

*/
function updateDateField(dateFieldName, dateString)
{
  var targetDateField = document.getElementsByName (dateFieldName).item(0);
  if (dateString)
    targetDateField.value = dateString;
 
  var pickerDiv = document.getElementById(datePickerDivID);
  pickerDiv.style.visibility = "hidden";
  pickerDiv.style.display = "none";
 
  adjustiFrame();
  targetDateField.focus();
 
  // after the datepicker has closed, optionally run a user-defined function called
  // datePickerClosed, passing the field that was just updated as a parameter
  // (note that this will only run if the user actually selected a date from the datepicker)
  if ((dateString) && (typeof(datePickerClosed) == "function"))
    datePickerClosed(targetDateField);
}


/**
Use an "iFrame shim" to deal with problems where the datepicker shows up behind
selection list elements, if they're below the datepicker. The problem and solution are
described at:

http://dotnetjunkies.com/WebLog/jking/archive/2003/07/21/488.aspx
http://dotnetjunkies.com/WebLog/jking/archive/2003/10/30/2975.aspx
*/
function adjustiFrame(pickerDiv, iFrameDiv)
{
  // we know that Opera doesn't like something about this, so if we
  // think we're using Opera, don't even try
  var is_opera = (navigator.userAgent.toLowerCase().indexOf("opera") != -1);
  if (is_opera)
    return;
  
  // put a try/catch block around the whole thing, just in case
  try {
    if (!document.getElementById(iFrameDivID)) {
      // don't use innerHTML to update the body, because it can cause global variables
      // that are currently pointing to objects on the page to have bad references
      //document.body.innerHTML += "<iframe id='" + iFrameDivID + "' src='javascript:false;' scrolling='no' frameborder='0'>";
      var newNode = document.createElement("iFrame");
      newNode.setAttribute("id", iFrameDivID);
      newNode.setAttribute("src", "javascript:false;");
      newNode.setAttribute("scrolling", "no");
      newNode.setAttribute ("frameborder", "0");
      document.body.appendChild(newNode);
    }
    
    if (!pickerDiv)
      pickerDiv = document.getElementById(datePickerDivID);
    if (!iFrameDiv)
      iFrameDiv = document.getElementById(iFrameDivID);
    
    try {
      iFrameDiv.style.position = "absolute";
      iFrameDiv.style.width = pickerDiv.offsetWidth;
      iFrameDiv.style.height = pickerDiv.offsetHeight ;
      iFrameDiv.style.top = pickerDiv.style.top;
      iFrameDiv.style.left = pickerDiv.style.left;
      iFrameDiv.style.zIndex = pickerDiv.style.zIndex - 1;
      iFrameDiv.style.visibility = pickerDiv.style.visibility ;
      iFrameDiv.style.display = pickerDiv.style.display;
    } catch(e) {
    }
 
  } catch (ee) {
  }
 
}
