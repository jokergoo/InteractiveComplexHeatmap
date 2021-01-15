
$(function() {
	$("#@{heatmap_id}_heatmap_resize").resizable({
		stop: function( event, ui ) {
			document.getElementById("@{heatmap_id}_mask").remove();
			$("#@{heatmap_id}_heatmap_brush").remove();
			$("#@{heatmap_id}_heatmap").height(ui.size.height-4);
			$("#@{heatmap_id}_heatmap").width(ui.size.width-4);
		},
		start: function(event, ui) {
			var mask = document.createElement("div");
			mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
			mask.setAttribute("id", "@{heatmap_id}_mask");
			$("#@{heatmap_id}_heatmap_resize").append(mask);
		},
		resize: function(event, ui) {
			$("#@{heatmap_id}_mask").width(ui.size.width);
			$("#@{heatmap_id}_mask").height(ui.size.height);
			$("#@{heatmap_id}_heatmap").width(ui.size.width);
			$("#@{heatmap_id}_heatmap").height(ui.size.height);

			$('#@{heatmap_id}_heatmap_input_width').val(ui.size.width);
			$('#@{heatmap_id}_heatmap_input_height').val(ui.size.height);

			if(parseInt($('#@{heatmap_id}_heatmap_download_format').find('input').filter(':checked').val()) == 2) {
				width_in_inch = Math.round(ui.size.width*10/100*4/3)/10;
				height_in_inch = Math.round(ui.size.height*10/100*4/3)/10;
				$('#@{heatmap_id}_heatmap_download_image_width').val(width_in_inch);
				$('#@{heatmap_id}_heatmap_download_image_height').val(height_in_inch);
			} else {
				$('#@{heatmap_id}_heatmap_download_image_width').val(ui.size.width);
				$('#@{heatmap_id}_heatmap_download_image_height').val(ui.size.height);
			}
		}
	});

	$("#@{heatmap_id}_sub_heatmap_resize").resizable({
		stop: function( event, ui ) {
			document.getElementById("@{heatmap_id}_mask2").remove();
			$("#@{heatmap_id}_sub_heatmap").height(ui.size.height-4);
			$("#@{heatmap_id}_sub_heatmap").width(ui.size.width-4);
		},
		start: function(event, ui) {
			var mask = document.createElement("div");
			mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
			mask.setAttribute("id", "@{heatmap_id}_mask2");
			$("#@{heatmap_id}_sub_heatmap_resize").append(mask);
		},
		resize: function(event, ui) {
			$("#@{heatmap_id}_mask2").width(ui.size.width);
			$("#@{heatmap_id}_mask2").height(ui.size.height);
			$("#@{heatmap_id}_sub_heatmap").width(ui.size.width);
			$("#@{heatmap_id}_sub_heatmap").height(ui.size.height);
		}
	});

	$( '#@{heatmap_id}_tabs' ).tabs({
		collapsible: true,
		active: false
	});

	$('#@{heatmap_id}_tabs a').tooltip({
		position: {
			my: 'center bottom-4', 
			at: 'center top'
		}
	});

	$('#@{heatmap_id}_keyword').click(function() {
		$('#@{heatmap_id}_heatmap_brush').remove();
	});

	$('#@{heatmap_id}_search_regexpr').change(function() {
		if(this.checked) {
			$('#@{heatmap_id}_keyword').attr('placeholder', 'A single regular expression');
		} else {
			$('#@{heatmap_id}_keyword').attr('placeholder', "Multiple keywords separated by ','");
		}
	});

	$('#@{heatmap_id}_heatmap_input_size_button').click( function() {
		var width = $('#@{heatmap_id}_heatmap_input_width').val();
		width = parseInt(width);
		var height = $('#@{heatmap_id}_heatmap_input_height').val();
		height = parseInt(height);
		$('#@{heatmap_id}_heatmap_resize').width(width+4);
		$('#@{heatmap_id}_heatmap').width(width);
		$('#@{heatmap_id}_heatmap img').width(width);
		$('#@{heatmap_id}_heatmap_resize').height(height+4);
		$('#@{heatmap_id}_heatmap').height(height);
		$('#@{heatmap_id}_heatmap img').height(height);
		$('#@{heatmap_id}_heatmap_download_image_width').val(width);
		$('#@{heatmap_id}_heatmap_download_image_height').val(height);
	});

	$('#@{heatmap_id}_heatmap_download_format').change( function() {
		var width = $('#@{heatmap_id}_heatmap_input_width').val();
		width = parseInt(width);
		var height = $('#@{heatmap_id}_heatmap_input_height').val();
		height = parseInt(height);
		if(parseInt($(this).find('input').filter(':checked').val()) == 2) {
			width_in_inch = Math.round(width*10/100*4/3)/10;
			height_in_inch = Math.round(height*10/100*4/3)/10;
			$('#@{heatmap_id}_heatmap_download_image_width').val(width_in_inch);
			$('#@{heatmap_id}_heatmap_download_image_height').val(height_in_inch);
			$('#@{heatmap_id}_heatmap_download_image_width').prev().text('Image width (in inch)');
			$('#@{heatmap_id}_heatmap_download_image_height').prev().text('Image height (in inch)');
		} else {
			$('#@{heatmap_id}_heatmap_download_image_width').val(width);
			$('#@{heatmap_id}_heatmap_download_image_height').val(height);
			$('#@{heatmap_id}_heatmap_download_image_width').prev().text('Image width (in px)');
			$('#@{heatmap_id}_heatmap_download_image_height').prev().text('Image height (in px)');
		}
	});

	var @{heatmap_id}_pickr1 = Pickr.create({
	    el: '#@{heatmap_id}_color_pickers_border',
	    default: '#003366',
	    theme: 'nano',
	    comparison: false,
	    components: {
	    	preview: true, 
	    	opacity: true,
	    	hue: true
	    }
	});	

	@{heatmap_id}_pickr1.on('change', (color, source, instance) => {
		$('#@{heatmap_id}_heatmap_brush').css('border-color', color.toRGBA().toString());
		$('#@{heatmap_id}_heatmap').mousedown(function() {
			if($('#@{heatmap_id}_heatmap_brush').length > 0) {
				$('#@{heatmap_id}_heatmap_brush').css('border-color', color.toRGBA().toString());
			}
		});
	});

	var @{heatmap_id}_pickr2 = Pickr.create({
	    el: '#@{heatmap_id}_color_pickers_fill',
	    default: '#99ccff',
	    theme: 'nano',
	    comparison: false,
	    components: {
	    	preview: true, 
	    	opacity: true,
	    	hue: true
	    }
	});	

	@{heatmap_id}_pickr2.on('change', (color, source, instance) => {
		$('#@{heatmap_id}_heatmap_brush').css('background-color', color.toRGBA().toString());
		$('#@{heatmap_id}_heatmap').mousedown(function() {
			if($('#@{heatmap_id}_heatmap_brush').length > 0) {
				$('#@{heatmap_id}_heatmap_brush').css('background-color', color.toRGBA().toString());
			}
		});
	});

	$('#@{heatmap_id}_color_pickers_border_width').change(function() {
		var val = $(this).val();
		$('#@{heatmap_id}_heatmap_brush').css('border-width', val);
		$('#@{heatmap_id}_heatmap').mousedown(function() {
			if($('#@{heatmap_id}_heatmap_brush').length > 0) {
				$('#@{heatmap_id}_heatmap_brush').css('border-width', val);
			}
		});
	});

	$('#@{heatmap_id}_color_pickers_opacity').change(function() {
		var val = $(this).val();
		$('#@{heatmap_id}_heatmap_brush').css('opacity', val);
		$('#@{heatmap_id}_heatmap').mousedown(function() {
			if($('#@{heatmap_id}_heatmap_brush').length > 0) {
				$('#@{heatmap_id}_heatmap_brush').css('opacity', val);
			}
		});
	});


	// sub heamtaps
	$( '#@{heatmap_id}_sub_tabs' ).tabs({
		collapsible: true,
		active: false
	});

	$('#@{heatmap_id}_sub_tabs a').tooltip({
		position: {
			my: 'center bottom-4', 
			at: 'center top'
		}
	});

	$('#@{heatmap_id}_sub_heatmap_input_size_button').click(function(){
		var width = $('#@{heatmap_id}_sub_heatmap_input_width').val();
		width = parseInt(width);
		var height = $('#@{heatmap_id}_sub_heatmap_input_height').val();
		height = parseInt(height);
		$('#@{heatmap_id}_sub_heatmap_resize').width(width+4);
		$('#@{heatmap_id}_sub_heatmap').width(width);
		$('#@{heatmap_id}_sub_heatmap img').width(width);
		$('#@{heatmap_id}_sub_heatmap_resize').height(height+4);
		$('#@{heatmap_id}_sub_heatmap').height(height);
		$('#@{heatmap_id}_sub_heatmap img').height(height);
		$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width);
		$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height);
	});

	$('#@{heatmap_id}_sub_heatmap_download_format').change(function() {
		var width = $('#@{heatmap_id}_sub_heatmap_input_width').val();
		width = parseInt(width);
		var height = $('#@{heatmap_id}_sub_heatmap_input_height').val();
		height = parseInt(height);
		if(parseInt($(this).find('input').filter(':checked').val()) == 2) {
			width_in_inch = Math.round(width*10/100*4/3)/10;
			height_in_inch = Math.round(height*10/100*4/3)/10;
			$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width_in_inch);
			$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height_in_inch);
			$('#@{heatmap_id}_sub_heatmap_download_image_width').prev().text('Image width (in inch)');
			$('#@{heatmap_id}_sub_heatmap_download_image_height').prev().text('Image height (in inch)');
		} else {
			$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width);
			$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height);
			$('#@{heatmap_id}_sub_heatmap_download_image_width').prev().text('Image width (in px)');
			$('#@{heatmap_id}_sub_heatmap_download_image_height').prev().text('Image height (in px)');
		}
	});

	Shiny.addCustomMessageHandler('@{heatmap_id}_initialized', function(message) {
		$('#@{heatmap_id}_heatmap_control').css("display", "block");
		Shiny.setInputValue('@{heatmap_id}_heatmap_download_trigger', Math.random());
	});

	Shiny.addCustomMessageHandler('@{heatmap_id}_empty_search', function(message) {
		$('#@{heatmap_id}_tabs-search').html("<p>Search is turned off because of no row/column labels.</p>");
	});

	Shiny.addCustomMessageHandler('@{heatmap_id}_sub_initialized', function(message) {
		if(message == "on") {
			$('#@{heatmap_id}_sub_heatmap_control').css("display", "block");
		} else {
			$('#@{heatmap_id}_sub_heatmap_control').css("display", "none");
		}
		Shiny.setInputValue('@{heatmap_id}_sub_heatmap_download_trigger', Math.random());
	});

	// on shinyapp.io, jqueryui tabs are not properly processed
	var divs = $("#@{heatmap_id}_tabs ul").nextAll();
	if(divs.length == 8) {
		for(var iv = 0; iv < 4; iv ++) {
			var all_attrs = divs[iv].attributes;
			for(var i = 0; i < all_attrs.length; i ++) {
				if(all_attrs[i].name != "id") {
					$(divs[iv + 4]).attr(all_attrs[i].name, all_attrs[i].value);
				}
			}
		}
		divs[0].remove();
		divs[1].remove();
		divs[2].remove();
		divs[3].remove();
	}

	var divs = $("#@{heatmap_id}_sub_tabs ul").nextAll();
	if(divs.length == 8) {
		for(var iv = 0; iv < 4; iv ++) {
			var all_attrs = divs[iv].attributes;
			for(var i = 0; i < all_attrs.length; i ++) {
				if(all_attrs[i].name != "id") {
					$(divs[iv + 4]).attr(all_attrs[i].name, all_attrs[i].value);
				}
			}
		}
		divs[0].remove();
		divs[1].remove();
		divs[2].remove();
		divs[3].remove();
	}

});
