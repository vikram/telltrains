// START:dragstuff
// used to control moving the map div
var dragging = false;
var top;
var left;
var lleft;
var ltop;
var mleft;
var nleft;
var wleft;
var dragStartTop;
var dragStartLeft;

function init() {
    // make inner div big enough to display the map
    //setInnerDivSize('2000px', '1400px');
    // wire up the mouse listeners to do dragging
    dragThis(document.getElementById("timeline"));
    dragThis(document.getElementById("markers"));
    //showhideOptions();
    filterJourneys();
}

function dragThis(outerDiv) {
    if (outerDiv !== null)
	{
	    outerDiv.onmousedown = startMove;
	    outerDiv.onmousemove = processMove;
	    outerDiv.onmouseup = stopMove;
	    // necessary to enable dragging on IE
	    outerDiv.ondragstart = function() { return false; };
	}
}

function startMove(event) {
    // necessary for IE
    if (!event) { event = window.event;}
    dragStartLeft = event.clientX;
    dragStartTop = event.clientY;
    var innerDiv = document.getElementById("timeline");
    var style = innerDiv.style;
    innerDiv.style.cursor = "";
    top = stripPx(innerDiv.style.top);
    left = stripPx(innerDiv.style.left);
    mleft = stripPx(document.getElementById("markers").style.left);
    lleft = stripPx(document.getElementById("lines").style.left);
    ltop = stripPx(document.getElementById("lines").style.top);
    nleft = stripPx(document.getElementById("now").style.left);
    dragging = true;
    return false;
}
function processMove(event) {
    if (!event) { event = window.event;} // for IE
    var innerDiv = document.getElementById("timeline");
    var markers = document.getElementById("markers");
    var lines = document.getElementById("lines");
    var now = document.getElementById("now");
    if (dragging) {
	innerDiv.style.left = left + (event.clientX - dragStartLeft) + 'px';
	markers.style.left = mleft + (event.clientX - dragStartLeft) + 'px';
	lines.style.left = lleft + (event.clientX - dragStartLeft) + 'px';
        now.style.left = nleft + (event.clientX - dragStartLeft) + 'px';
	lines.style.top = ltop + (event.clientX - dragStartLeft)/xFactor + 'px';
	innerDiv.style.top = top + (event.clientX - dragStartLeft)/xFactor + 'px';
    }
}
function stopMove() {
    var innerDiv = document.getElementById("timeline");
    innerDiv.style.cursor = "";
    dragging = false;
}
function stripPx(value) {
    if (value === "") {return 0;}
    return parseFloat(value.substring(0, value.length - 2));
}
// END:dragstuff
function setInnerDivSize(width, height) {
    var innerDiv = document.getElementById("timeline");
    innerDiv.style.width = width;
    innerDiv.style.height = height;
}

function filterJourneys(){
    var doneSomething = false;
    var journey;
    var x;
    for (var i=0; i< numberOfJourneys; i++) {
	journey = $(journeys[i].id);
	if (shouldFilter(journeys[i])) {
	    if (journey.style.opacity != "0.2") {
		x = new Effect.Fade(journey, {to:0.2});
		doneSomething = true;
	    }
	} else if (journey.style.opacity == "0.2") {
	    x = new Effect.Appear (journey);
	    doneSomething = true;
	}
    }
    if (doneSomething) {
	x = new Effect.Highlight('timeline');
	x = new Effect.Highlight('now');
    }
}

function shouldFilter(journey) {

    var stopping = $('stopping');
    var result = (stopping.checked === 0 && journey.stopping === true);

    var peak = $('peak');
    result = result || (peak.checked === 0 && journey.peak === true);

    var changes = $('changesSelect');
    var numChanges = parseFloat(changes.options[changes.selectedIndex].value);

    result = result || (journey.changes>numChanges);

    var routes = $('route');
    var routeNumber = parseFloat(routes.options[routes.selectedIndex].value);
    
    result = result || (routeNumber!=journey.route && 9999!=routeNumber);

    var departingStart=parseFloat($('Departing_start').options[$('Departing_start').selectedIndex].value);
    var departingEnd=parseFloat($('Departing_end').options[$('Departing_end').selectedIndex].value);
    var arrivingStart=parseFloat($('Arriving_start').options[$('Arriving_start').selectedIndex].value);
    var arrivingEnd=parseFloat($('Arriving_end').options[$('Arriving_end').selectedIndex].value);

    result = result || (!(journey.end>=arrivingStart && journey.end<=arrivingEnd &&
			   journey.start>=departingStart && journey.start<=departingEnd));

    return result;
}


function toggleLink(id, orig, repl, fn){
    fn();
    if ($(id).innerHTML == orig) {
	$(id).innerHTML = repl;
    }
    else {
	$(id).innerHTML = orig;
    }
}

function showhideOptions() {
    var x;
    if ($('routeli')) {
	x = new Effect.toggle('routeli','appear',{duration:0.0}); 
	x = new Effect.toggle('changeli','appear',{duration:0.0}); 
    }
}

function resetOptions() {
    $('stopping').checked = 1;
    $('peak').checked = 1;

    $('changesSelect').selectedIndex = 0;
    $('route').selectedIndex = 0;
    $('Departing_start').selectedIndex = 0;
    $('Departing_end').selectedIndex = $('Departing_end').length - 1;
    $('Arriving_start').selectedIndex = 0;
    $('Arriving_end').selectedIndex = $('Arriving_end').length - 1;
    
    filterJourneys();
}
