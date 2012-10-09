
//utility functions

  function $(id) { 
      return document.getElementById(id);
  }

  function createXMLHttpRequest( ) {
    try { return new ActiveXObject("Msxml2.XMLHTTP"); } catch (e) {}
    try { return new ActiveXObject("Microsoft.XMLHTTP"); } catch (e) {}
    try { return new XMLHttpRequest( ); } catch(e) {}
    alert("XMLHttpRequest not supported");
    return null;
  }

//presentation
var tomorrow = new Date(((new Date).getTime()) + 24 * 60 * 60 * 1000);

function singlefaresurl(from, to) {
    var day = tomorrow.getDate();
    var month = ["January","February","March","April","May","June","July",
		 "August","September","October","November","December"][tomorrow.getMonth()];
    return "single-times-fares.ucw?from=" + from + "&to=" + to + "&day=" + day + "&month=" + month + "&hour=";
}

function getSingledayfares (from, to) {
    var url = singlefaresurl(from,to);
      singleFares(url + "05");
      singleFares(url + "08");
    /*
      singleFares(url + "11");
      singleFares(url + "14");
      singleFares(url + "17");
    */
}

      function singleFares(request) {
	  var xhr = createXMLHttpRequest( );
	  xhr.onreadystatechange = function( ) {
	      if (xhr.readyState==4) { // Request is finished
		  if (xhr.status==200) {
		      processResults(xhr.responseText);
		      notWaiting(request);
		  } else {
		      alert("Message returned, but with error status.");
		  }
	      }
	  }
	  xhr.open("GET", request, true);
	  waiting(request);
	  xhr.send(null);
      }

function processResults(result) {
    var response = eval("(" + result + ")");
    assembleResult(response);
}

var results = [];

function leavingOrder(a,b) {
    return a.train.leavingnum - b.train.leavingnum;
}

function deleteDuplicates(a, test) {
	tmp = new Array(0);
	for(i=0;i<a.length;i++){
	    if(!member(tmp, a[i], test)){
		tmp.length+=1;
		tmp[tmp.length-1]=a[i];
	    }
	}
	return tmp;
}

function member(a, e, test) {
	for(j=0;j<a.length;j++)
	    if(a[j] != null && test(a[j],e))
		return true;
	return false;
}

function extractFares(a) {
    var tmp = new Array(0);
    for (i=0;i<a.length;i++) {
	for (j=0; j<a[i].fares.length; j++) {
	    if(!member(tmp, 
		       a[i].fares[j], 
		       function(a,e) { return a.type == e.type && a.price == e.price;})) {
		if (a[i].fares[j] != null) {
		    tmp.length+=1;
		    tmp[tmp.length-1]=a[i].fares[j];
		}
	    }
	}
    }
    return tmp;
}

function doTable (results) {

    var table = '<div class="container"><table><tr>';

    var fares = extractFares(results);

    for(i=0;i < fares.length; i++) {
	var endTag = "";
	if (fares[i].type == "OPEN") 
	    endTag = 'checked="true">';
	else
	    endTag = '>';
	table += '<td><input type="radio" name="fares" value=' + i + ' ' + endTag +
	    fares[i].type + ' ' + fares[i].price + '</input></td>';
    }

    table += '</tr></table><table class="view">';
    alt = 'view';
    for(i=0;i < results.length; i++) {
	table += '<tr class="' + alt + '">' +
	    '<td>' + results[i].train.from + '</td>' +
	    '<td>' + results[i].train.to + '</td>' +
	    '<td>' + results[i].train.leaving + '</td>' +
	    '<td>' + results[i].train.arriving + '</td>' +
	    '<td>' + results[i].train.duration + '</td>' +
	    '<td>' + results[i].train.changes + '</td>';

	for(j=0;j < results[i].fares.length; j++) {
	    table += '<td>' + results[i].fares[j].type + '</td>' +
		'<td>' + results[i].fares[j].price + '</td>';
	}
	table += '</tr>';
    }
    table += '</table></div>';
    $("sandbox").innerHTML = table;
}

function assembleResult(response) {
    results = results.concat(response.results);
    results = deleteDuplicates(results, function(a,e) {	return a.train.leaving==e.train.leaving
							&& a.train.arriving == e.train.arriving
							&& a.train.changes == e.train.changes;});
    results = results.sort(leavingOrder);
    doTable(results);
    doGraph(results);
    //    doTimeline(results);
}

function doGraph (results) {
    var timeline="";
    for(i=0;i<results.length; i++) {
	timeline += 
	    '<div id="train" style="background: rgb(88, 160, 220) none repeat scroll 0%; overflow: hidden; position: absolute; left: ' + (1 * parseInt(results[i].train.startem)).toString() + 'px; top: ' +  (1.5 * (i + 1)) + 'em; height :1.5em; width: ' + results[i].train.durem + 'px;">' +
	    '<img src="bluebar1px.png" alt="' + results[i].train.leaving + '" height="15" width="' + (results[i].train.durem) 
	    + '" style="background-repeat: repeat-x"/>'
	    + '</div>';
    }

    $("trains").innerHTML = timeline;
	
}

var gEventSource;
var alreadyAdded = [];

function doTimeline(results) {
    var timelinerEntries = [];
    for (var i = 0; i < results.length; ++i) {
	if (!member(alreadyAdded, results[i].train, 
		    function(a,e) { return a !=null &&
			    a.leaving==e.leaving
			    && a.arriving == e.arriving
			    && a.changes == e.changes;}))
	    {
		var train = results[i].train;
		var start = convertFromGDataDate(train.leaving);
		var end = convertFromGDataDate(train.arriving);
		var changes = "";
		for (var j = 0; j < train.breaks.length; j++) 
		    changes += train.breaks[j].start + ' ' + train.breaks[j].type + ' from ' + train.breaks[j].leaving + '\r';
		timelinerEntries.push(new Timeline.DefaultEventSource.Event(
									    start,
									    end, // end - when not set, event displayed with icon (looks better)
									    null, // latestStart
									    null, // latestEnd
									    false, // not isDuration
									    'leaving ' + train.leaving + ' arriving ' + train.arriving,
									    changes,
									    'bluebar.png', // image
									    null, // link - destination when clicking on title
									    null, //icon
									    undefined, // color
									    undefined // textColor
									    ));
		alreadyAdded.length+=1;
		alreadyAdded[alreadyAdded.length-1]=results[i].train;
	    }
    }
    gEventSource.addMany(timelinerEntries);
};

function convertToGDataDate(/*Date*/ date) {
    return date.getFullYear() + '-' +
	zeroPad(date.getMonth() + 1) + '-' +
	zeroPad(date.getDate());
}

function convertFromGDataDate(/*string<HH:MM>*/ date) {
    var match = date.match(/(\d{2}):(\d{2})/);
    var myDate = new Date(tomorrow.getFullYear(), tomorrow.getMonth(), tomorrow.getDate(), parseInt(match[1], 10) + 1, parseInt(match[2], 10), 00, 00);
    return myDate;
}


var progress = 0;

function waiting(request) {
    progress++;
    if (progress!=0)
	$("progress").className = "Waiting";
}

function notWaiting(request) {
    progress--;
    fade(0);
    if (progress==0)
	$("progress").className = "notWaiting";
}

function fade() {
    fadeLoop(0);
}

function fadeLoop(nextBluePercent) {
    $("sandbox").style.backgroundColor = "rgb(100%, 100%, "+nextBluePercent+"%)";
    nextBluePercent += 10;
    if (nextBluePercent<=100) {
	setTimeout("fadeLoop("+nextBluePercent+")", 100);
    }
}

//--------------graphical interface-----------

// START:dragstuff
// used to control moving the map div
var dragging = false;
var top;
var left;
var dragStartTop;
var dragStartLeft;
function init() {
// make inner div big enough to display the map
setInnerDivSize('800px', '600px');
// wire up the mouse listeners to do dragging
var outerDiv = document.getElementById("outerDiv");
outerDiv.onmousedown = startMove;
outerDiv.onmousemove = processMove;
outerDiv.onmouseup = stopMove;
// necessary to enable dragging on IE
outerDiv.ondragstart = function() { return false; }
}
function startMove(event) {
// necessary for IE
if (!event) event = window.event;
dragStartLeft = event.clientX;
dragStartTop = event.clientY;
var innerDiv = document.getElementById("innerDiv");
innerDiv.style.cursor = "-moz-grab";
top = stripPx(innerDiv.style.top);
left = stripPx(innerDiv.style.left);
dragging = true;
return false;
}
function processMove(event) {
if (!event) event = window.event; // for IE
var innerDiv = document.getElementById("innerDiv");
if (dragging) {
innerDiv.style.top = top + (event.clientY - dragStartTop);
innerDiv.style.left = left + (event.clientX - dragStartLeft);
}
}
function stopMove() {
var innerDiv = document.getElementById("innerDiv");
innerDiv.style.cursor = "";
dragging = false;
}
function stripPx(value) {
if (value == "") return 0;
return parseFloat(value.substring(0, value.length - 2));
}
// END:dragstuff
function setInnerDivSize(width, height) {
var innerDiv = document.getElementById("innerDiv");
innerDiv.style.width = width;
innerDiv.style.height = height;
}

//--------

    function zeroPad(n) {
	if (n < 0) throw new Error('n is negative');
	return (n < 10) ? '0' + n : n;
    }

function onLoadTL(from,to) {

    getSingledayfares(from,to);
    init();
    /*
    gEventSource = new Timeline.DefaultEventSource();

    var theme = Timeline.ClassicTheme.create();
    theme.event.bubble.width = 250;
    theme.event.bubble.height = 180;
    theme.ether.backgroundColors.unshift("white");

    // centering the timeline three months previous makes it look nicer on load
    var fourInTheMorning = new Date(tomorrow.getFullYear(), tomorrow.getMonth(), tomorrow.getDate(), 17, 00, 00, 00);

    var bandInfos = [
		     Timeline.createBandInfo({
			     eventSource: gEventSource,
			     width: "90%",
			     date: fourInTheMorning,
			     intervalUnit: Timeline.DateTime.HOUR,
			     intervalPixels: 60,
			     timeZone: 0,
			     theme: theme
			 }),
		     Timeline.createBandInfo({
			     showEventText: false,
			     trackHeight: 0.5,
			     trackGap: 0.2,
			     eventSource: gEventSource,
			     width: "10%",
			     date: fourInTheMorning,
			     intervalUnit: Timeline.DateTime.DAY,
			     intervalPixels: 800,
			     timeZone: 0,
			     theme: theme
			 })
		     ];
    bandInfos[1].syncWith = 0;
    bandInfos[1].highlight = true;
    bandInfos[1].eventPainter.setLayout(bandInfos[0].eventPainter.getLayout());
    tl = Timeline.create(document.getElementById("my-timeline"), bandInfos);
    */
}

var resizeTimerID = null;
function onResize() {
    if (resizeTimerID == null) {
	resizeTimerID = window.setTimeout(function() {
		resizeTimerID = null;
		tl.layout();
	    }, 500);
    }
}
