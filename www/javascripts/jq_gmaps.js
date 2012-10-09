/**
 * Plugin for jQuery 1.1 to manage Google Maps
 * @author SeViR
 *
 * the method wait locations data as JS Object with this structure:
 *
 * data: {locations: [
 * 	{
 *  	simpleContent: String with the HTML content in the info window.
 * 		maximizedContent : URL of the HTML page (AJAX loaded) of the maximized info window.
 * 		latitude: latitude of the geographic poing (you can use infopanel for obtain that)
 * 		longitude: longitude of the geographic poing (you can use infopanel for obtain that)
 * 		zoom: zoom level for hotspot links
 * 		icon: url of the custom icon for this place
 *  },
 *  ...
 * ]}
 * 
 * The "infopanel" param is a DOM object to show the latitude and longitude of the
 * center of the map, also, the level zoom applied.
 * 
 * The "maptype" param is some of map type that admits Google MAPS like G_MAP_TYPE, G_SATELLITE_TYPE y G_HYBRID_TYPE.
 * 
 * The "center" param is a point [latitude, longitude of the default point of the map center
 * 
 * The "zoom" param is the default zoom applied
 * 
 * The "relativepath" param is the absolute url folder path of the document (used for relative paths
 * of icons and maximized content).
 * 
 * sample:
 * $("#mylayer").gmaps({
 * 	data: myplaces, 
 * 	infopanel: $("#infolayer").get(0), 
 * 	tipomapa: G_HYBRID_TYPE,
 *  centro: [1, -0.3],
 *  zoom: 10,
 *  relativepath: "http://mydomain.com/myproject/"
 * });
 */
jQuery.fn.gmaps = function(settings){
	return this.each(function(){
		new jQuery.gmaps(this, settings);
	});
}

jQuery.gmaps = function(obj, settings){
	var map = false;
	var gitems = new Array();
	
	settings = jQuery.extend({
		data: {locations:[]},
		infopanel: "none",
		maptype: G_MAP_TYPE,
		center: [37.98547493214612 , -1.1305618286132812],
		zoom: 11,
		relativepath: "http://192.168.0.6/"
	},settings);
		
	if (GBrowserIsCompatible()) {
	    map = new GMap2(obj);
		map.addControl(new GSmallMapControl());
		map.addControl(new GMapTypeControl());
				
		map.setCenter(new GLatLng(settings.center[0],settings.center[1]), settings.zoom, settings.maptype);

		if (typeof(settings.infopanel) != Boolean){
			GEvent.addListener(map, "moveend", function() {
				var center = map.getCenter();
				var zoom = map.getZoom();
				$(settings.infopanel).html(center.toString() + " zoom: " + zoom);
			});
		}
		
		// Create the text for the marker
        function createMarker(point, number) {
			// create custom icon
			var icon = new GIcon();
			icon.image = (settings.data.locations[number].icon.indexOf("http:") < 0)?
			    settings.relativepath + settings.data.locations[number].icon:settings.data.locations[number].icon;
			icon.iconSize = new GSize(18, 18);
			icon.iconAnchor = new GPoint(6, 18);
			icon.infoWindowAnchor = new GPoint(5, 1);

			var marker = new GMarker(point, icon);
		  	
         	GEvent.addListener(marker, "click", function() {
				if (settings.data.locations[number].maximizedContent != ""){
				    simpleContent = settings.data.locations[number].simpleContent + "<a href='#' style='font-size:10px;margin-top:20px;display: block;' onclick='$(\"#"+obj.id+"\").get(0).mapa.getInfoWindow().maximize();return false;'>Ver informaci&oacute;n expandida</a>"
					marker.openInfoWindowHtml(simpleContent, 
									{maxUrl:(settings.data.locations[number].maximizedContent.indexOf("http:") < 0)?
											settings.relativepath + settings.data.locations[number].maximizedContent:
											settings.data.locations[number].maximizedContent
									}
									);
				}else{
				    marker.openInfoWindow(settings.data.locations[number].simpleContent);
				}				
				    
			});
			
			gitems.push(marker);
          
          return marker;
        }
	var latslongs = new Array();

		//Inserts markers
		for (i=0;i<settings.data.locations.length;i++){
			var pos = new GLatLng(settings.data.locations[i].latitude,
						       settings.data.locations[i].longitude);
			map.addOverlay(createMarker(pos, i));
			latslongs[i]=pos;
		}	
		
		var polyLine = new GPolyline(latslongs, "#B9121B", 5, 0.8);

		map.addOverlay(polyLine);

		//Loads the events of the link of the hotspots
		$("." +obj.id).each(function(){
			this.associated_map = obj.id;
			$(this).click(function(){
				$("#"+this.associated_map).get(0).gmap.jq_maps.goTo($("#"+this.associated_map).get(0), this.tabIndex - 1);
				return false;
			});
		});
		
		//save the properties in gmap property, and set the new options for goTo point and maximize Window
		map.jq_maps = {
			"settings": settings,
			"gitems": gitems,
			"goTo": function(div_gmap, indice){
				div_gmap.gmap.setZoom(div_gmap.gmap.jq_maps.settings.data.locations[indice].zoom);
				div_gmap.gmap.panTo(new GLatLng(div_gmap.gmap.jq_maps.settings.data.locations[indice].latitude, div_gmap.gmap.jq_maps.settings.data.locations[indice].longitude));
				if (div_gmap.gmap.jq_maps.settings.data.locations[indice].maximizedContent != ""){
					simpleContent = div_gmap.gmap.jq_maps.settings.data.locations[indice].simpleContent + "<a href='#' style='font-size:10px;margin-top:20px;display: block;' onclick='$(\"#"+div_gmap.id+"\").get(0).gmap.getInfoWindow().maximize();return false;'>Show extended info</a>"
            		div_gmap.gmap.jq_maps.gitems[indice].openInfoWindowHtml(simpleContent, 
									{maxUrl:(div_gmap.gmap.jq_maps.settings.data.locations[indice].maximizedContent.indexOf("http:") < 0)?
											div_gmap.gmap.jq_maps.settings.relativepath + div_gmap.gmap.jq_maps.settings.data.locations[indice].maximizedContent:
											div_gmap.gmap.jq_maps.settings.data.locations[indice].maximizedContent
									}
					);
				}else{
					div_gmap.gmap.jq_maps.gitems[indice].openInfoWindowHtml(div_gmap.gmap.jq_maps.settings.data.locations[indice].simpleContent);
				}	
			}
		}
	}
	
	obj.gmap = map;
}
