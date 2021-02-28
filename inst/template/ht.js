
$(function() {

	$("#@{heatmap_id}_heatmap_control").width($("#@{heatmap_id}_heatmap").width());
	$("#@{heatmap_id}_sub_heatmap_control").width($("#@{heatmap_id}_sub_heatmap").width());

	$("#@{heatmap_id}_heatmap_resize").resizable({
		stop: function( event, ui ) {
			document.getElementById("@{heatmap_id}_mask").remove();
			$("#@{heatmap_id}_heatmap_brush").remove();
			$("#@{heatmap_id}_heatmap").height(ui.size.height - 4);
			$("#@{heatmap_id}_heatmap").width(ui.size.width - 4);
			$("#@{heatmap_id}_heatmap img").width(ui.size.width - 4);  // img has 4px margin
			$("#@{heatmap_id}_heatmap img").height(ui.size.height - 4);
			
			Shiny.setInputValue("@{heatmap_id}_heatmap_resize_width", ui.size.width - 4);
			Shiny.setInputValue("@{heatmap_id}_heatmap_resize_height", ui.size.height - 4);
			Shiny.setInputValue("@{heatmap_id}_heatmap_do_resize", Math.random());
			
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
			$("#@{heatmap_id}_heatmap").width(ui.size.width - 4);
			$("#@{heatmap_id}_heatmap").height(ui.size.height - 4);
			$("#@{heatmap_id}_heatmap img").width(ui.size.width - 4);  // img has 4px margin
			$("#@{heatmap_id}_heatmap img").height(ui.size.height - 4);

			$("#@{heatmap_id}_heatmap_control").width(ui.size.width);

			$('#@{heatmap_id}_heatmap_input_width').val(ui.size.width - 4);
			$('#@{heatmap_id}_heatmap_input_height').val(ui.size.height - 4);

			if(parseInt($('#@{heatmap_id}_heatmap_download_format').find('input').filter(':checked').val()) == 2) {
				width_in_inch = Math.round((ui.size.width - 4)*10/100*4/3)/10;
				height_in_inch = Math.round((ui.size.height - 4)*10/100*4/3)/10;
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
			$("#@{heatmap_id}_sub_heatmap").height(ui.size.height - 4);
			$("#@{heatmap_id}_sub_heatmap").width(ui.size.width - 4);
			$("#@{heatmap_id}_sub_heatmap img").width(ui.size.width - 4);
			$("#@{heatmap_id}_sub_heatmap img").height(ui.size.height - 4);
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
			$("#@{heatmap_id}_sub_heatmap").width(ui.size.width - 4);
			$("#@{heatmap_id}_sub_heatmap").height(ui.size.height - 4);

			$("#@{heatmap_id}_sub_heatmap img").width(ui.size.width - 4);
			$("#@{heatmap_id}_sub_heatmap img").height(ui.size.height - 4);

			$("#@{heatmap_id}_sub_heatmap_control").width(ui.size.width);
		}
	});

	$('.@{heatmap_id}_widget .ui-resizable-handle').css("z-index", "auto");

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
		$('#@{heatmap_id}_heatmap_resize').width(width + 4);
		$('#@{heatmap_id}_heatmap').width(width);
		$('#@{heatmap_id}_heatmap img').width(width);
		$('#@{heatmap_id}_heatmap_resize').height(height + 4);
		$('#@{heatmap_id}_heatmap').height(height);
		$('#@{heatmap_id}_heatmap img').height(height);
		$('#@{heatmap_id}_heatmap_download_image_width').val(width);
		$('#@{heatmap_id}_heatmap_download_image_height').val(height);

		$("#@{heatmap_id}_heatmap_control").width(width);

		Shiny.resetBrush("@{heatmap_id}_heatmap_brush");

		Shiny.setInputValue("@{heatmap_id}_heatmap_resize_button", Math.random());
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
	$('#@{heatmap_id}_sub_heatmap_input_size_button').click(function(){
		var width = $('#@{heatmap_id}_sub_heatmap_input_width').val();
		width = parseInt(width);
		var height = $('#@{heatmap_id}_sub_heatmap_input_height').val();
		height = parseInt(height);
		$('#@{heatmap_id}_sub_heatmap_resize').width(width + 4);
		$('#@{heatmap_id}_sub_heatmap').width(width);
		$('#@{heatmap_id}_sub_heatmap img').width(width);
		$('#@{heatmap_id}_sub_heatmap_resize').height(height + 4);
		$('#@{heatmap_id}_sub_heatmap').height(height);
		$('#@{heatmap_id}_sub_heatmap img').height(height);
		$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width);
		$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height);
		$("#@{heatmap_id}_sub_heatmap_control").width(width);
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

	@{heatmap_id}_create_color_picker();
	
	Shiny.addCustomMessageHandler('@{heatmap_id}_initialized', function(message) {
		$('#@{heatmap_id}_heatmap_control').css("display", "block");
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
	});

	Shiny.addCustomMessageHandler('@{heatmap_id}_remove_brush', function(message) {
		$("#@{heatmap_id}_heatmap_brush").remove();
		Shiny.setInputValue("@{heatmap_id}_heatmap_brush", null);
	})

	// similar function as "jquery ui tabs"
	var objs = $('#@{heatmap_id}_heatmap_control li a');
	$(objs[0]).attr("title", "Search heatmap");
	$(objs[1]).attr("title", "Configure brush");
	$(objs[2]).attr("title", "Save image");
	$(objs[3]).attr("title", "Resize heatmap");

	var href = $(objs[0]).attr("href");
	$(href).attr("visible", "0")
	$(objs[0]).css("background-color", "white");
	$(href).parent().css("display", "none");

	for(var i = 0; i < objs.length; i ++) {
		$(objs[i]).hover(function() {
			var tooltip = $("<span class='ui-widget-shadow'>" + $(this).attr("title") + "</span>");
			tooltip.css("position", "absolute").
			        css("z-index", "100").
			        css("bottom", "120%").
			        css("background-color", "white").
			        css("border", "1px solid #dddddd").
			        css("border-radius", "4px").
			        css("padding", "4px 12px").
			        css("color", "black").
			        css("white-space", "nowrap");
			tooltip.css("left", "-50%");
			$(this).append(tooltip);
		}, function() {
			$(this).find("span").last().remove();
		});

		$(objs[i]).click(function() {
			var href = $(this).attr("href");
			if($(href).attr("visible") == undefined) {
				if($(href).hasClass("active")) {
					$(href).attr("visible", "1")
				}
			}
			if($(href).attr("visible") == "1") {
				$(href).attr("visible", "0");
				$(href).parent().css("display", "none");
				$(this).css("background-color", "white");
				
			} else {
				$(objs[0]).css("background-color", "white");
				$(objs[1]).css("background-color", "white");
				$(objs[2]).css("background-color", "white");
				$(objs[3]).css("background-color", "white");

				$($(objs[0]).attr("href")).attr("visible", 0);
				$($(objs[1]).attr("href")).attr("visible", 0);
				$($(objs[2]).attr("href")).attr("visible", 0);
				$($(objs[3]).attr("href")).attr("visible", 0);

				$(href).attr("visible", "1");
				$(href).parent().css("display", "block");
				$(this).css("background-color", "#ddd");
			}
			false;
		})
	}

	var objs2 = $('#@{heatmap_id}_sub_heatmap_control li a');
	$(objs2[0]).attr("title", "Configure sub-heatmap");
	$(objs2[1]).attr("title", "Export to table");
	$(objs2[2]).attr("title", "Save sub-heatmap as an image");
	$(objs2[3]).attr("title", "Resize sub-heatmap");

	var href = $(objs2[0]).attr("href");
	$(href).attr("visible", "0")
	$(objs2[0]).css("background-color", "white");
	$(href).parent().css("display", "none");

	for(var i = 0; i < objs2.length; i ++) {
		$(objs2[i]).hover(function() {
			var tooltip = $("<span class='ui-widget-shadow'>" + $(this).attr("title") + "</span>");
			tooltip.css("position", "absolute").
			        css("z-index", "100").
			        css("bottom", "120%").
			        css("background-color", "white").
			        css("border", "1px solid #dddddd").
			        css("border-radius", "4px").
			        css("padding", "4px 12px").
			        css("color", "black").
			        css("white-space", "nowrap");
			tooltip.css("left", "-50%");
			$(this).append(tooltip);
		}, function() {
			$(this).find("span").last().remove();
		});

		$(objs2[i]).click(function() {
			var href = $(this).attr("href");
			if($(href).attr("visible") == undefined) {
				if($(href).hasClass("active")) {
					$(href).attr("visible", "1")
				}
			}
			if($(href).attr("visible") == "1") {
				$(href).attr("visible", "0");
				$(href).parent().css("display", "none");
				$(this).css("background-color", "white");
				
			} else {
				$(objs2[0]).css("background-color", "white");
				$(objs2[1]).css("background-color", "white");
				$(objs2[2]).css("background-color", "white");
				$(objs2[3]).css("background-color", "white");

				$($(objs2[0]).attr("href")).attr("visible", 0);
				$($(objs2[1]).attr("href")).attr("visible", 0);
				$($(objs2[2]).attr("href")).attr("visible", 0);
				$($(objs2[3]).attr("href")).attr("visible", 0);

				$(href).attr("visible", "1");
				$(href).parent().css("display", "block");
				$(this).css("background-color", "#ddd");
			}
			false;
		})
	}

	$('#@{heatmap_id}_show_cell_fun_checkbox').next().hover(function() {
		var tooltip = $("<span class='ui-widget-shadow'>" + "Graphics defined by <code>cell_fun</code> or <code>layer_fun</code>." + "</span>");
		tooltip.css("position", "absolute").
		        css("z-index", "100").
		        css("bottom", "110%").
		        css("background-color", "white").
		        css("border", "1px solid #dddddd").
		        css("border-radius", "4px").
		        css("padding", "4px 12px").
		        css("color", "black").
		        css("max-width", "600px");
		tooltip.css("left", "0");
		$(this).append(tooltip);
	}, function() {
		$(this).find("span").last().remove();
	});

	$('#@{heatmap_id}_fill_figure_checkbox').next().hover(function() {
		var tooltip = $("<span class='ui-widget-shadow'>" + "When the sub-heatmap has width or height in absolute units, it controls whether to ignore them and fill the sub-heatmap 100% to the figure region." + "</span>");
		tooltip.css("position", "absolute").
		        css("z-index", "100").
		        css("bottom", "110%").
		        css("background-color", "white").
		        css("border", "1px solid #dddddd").
		        css("border-radius", "4px").
		        css("padding", "4px 12px").
		        css("color", "black").
		        css("max-width", "600px");
		tooltip.css("left", "0");
		$(this).append(tooltip);
	}, function() {
		$(this).find("span").last().remove();
	});
});

function @{heatmap_id}_create_color_picker() {
	
	var @{heatmap_id}_pickr1 = Pickr.create({
	    el: '#@{heatmap_id}_color_pickers_border',
	    default: '@{pickr_fill}',
	    theme: 'nano',
	    comparison: false,
	    position: 'bottom-start',
	    container: "#@{heatmap_id}_tabs-brush",
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
	    default: '@{pickr_border}',
	    theme: 'nano',
	    comparison: false,
	    position: 'bottom-start',
	    container: "#@{heatmap_id}_tabs-brush",
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

	if('@{shiny_env[[heatmap_id]]$action}' == 'click') {
		var @{heatmap_id}_brush_x1;
		var @{heatmap_id}_brush_x2;
		var @{heatmap_id}_brush_y1;
		var @{heatmap_id}_brush_y2;
		$('#@{heatmap_id}_heatmap').mousedown(function(e) {
			var parentOffset = $(this).offset();
			@{heatmap_id}_brush_x1 = e.pageX - parentOffset.left;
			@{heatmap_id}_brush_y1 = e.pageY - parentOffset.top;
		}).mouseup(function(e) {
			var parentOffset = $(this).offset();
			@{heatmap_id}_brush_x2 = e.pageX - parentOffset.left;
			@{heatmap_id}_brush_y2 = e.pageY - parentOffset.top;
			if(@{heatmap_id}_brush_x1 == @{heatmap_id}_brush_x2 && @{heatmap_id}_brush_y1 == @{heatmap_id}_brush_y2) {
				Shiny.setInputValue('@{heatmap_id}_heatmap_mouse_action', Math.random());
			}
		});
	}

	if('@{shiny_env[[heatmap_id]]$action}' == 'hover') {
		var @{heatmap_id}_x;
        var @{heatmap_id}_y;
        $('#@{heatmap_id}_heatmap').mousemove(function(e) {
            var parentOffset = $(this).offset();
            @{heatmap_id}_x = e.pageX - parentOffset.left;
            @{heatmap_id}_y = e.pageY - parentOffset.top;
        }).mousestop(function() {
        	var h = $(this).height();
            if($('#@{heatmap_id}_heatmap_brush').length == 0) {
                Shiny.setInputValue('@{heatmap_id}_heatmap_hover', {x: @{heatmap_id}_x, y: h - @{heatmap_id}_y});
                Shiny.setInputValue('@{heatmap_id}_heatmap_mouse_action', Math.random());
            } 
        })
	}

	if('@{output_ui_float}' == 'TRUE') {
		var default_message = "Retrieving from server... Don't move mouse.";
		$('#@{heatmap_id}_output_wrapper').css({'position':'absolute','right':-10000,'left':'auto'});
	    if('@{shiny_env[[heatmap_id]]$action}' == "hover") {
	    	var @{heatmap_id}_absX;
			var @{heatmap_id}_absY;
		    $('#@{heatmap_id}_heatmap').mousemove(function(e) {
		    	@{heatmap_id}_absX = e.pageX;
		        @{heatmap_id}_absY = e.pageY;
		    })
	    	$('#@{heatmap_id}_heatmap').mousestop(function() {
	    		if($('#@{heatmap_id}_heatmap_brush').length == 0) {
		    		$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':@{heatmap_id}_absX, 'right':'auto', 'top':@{heatmap_id}_absY});
		    	}
		    });

		    // brush also turns output visible
		    var @{heatmap_id}_brush_x1;
			var @{heatmap_id}_brush_x2;
			var @{heatmap_id}_brush_y1;
			var @{heatmap_id}_brush_y2;
			$('#@{heatmap_id}_heatmap').mousedown(function(e) {
				var parentOffset = $(this).offset();
				@{heatmap_id}_brush_x1 = e.pageX - parentOffset.left;
				@{heatmap_id}_brush_y1 = e.pageY - parentOffset.top;
			}).mouseup(function(e) {
				var parentOffset = $(this).offset();
				@{heatmap_id}_brush_x2 = e.pageX - parentOffset.left;
				@{heatmap_id}_brush_y2 = e.pageY - parentOffset.top;
				if(@{heatmap_id}_brush_x1 != @{heatmap_id}_brush_x2 || @{heatmap_id}_brush_y1 != @{heatmap_id}_brush_y2) {
					$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
				}
			});	
		}
		if('@{shiny_env[[heatmap_id]]$action}' == "click") {
	    	$('#@{heatmap_id}_heatmap').click(function(e) {
		    	$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
		    });
		}
		if('@{shiny_env[[heatmap_id]]$action}' == "dblclick") {
	    	$('#@{heatmap_id}_heatmap').dblclick(function(e) {
		    	$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
		    });

		    // brush also turns output visible
		    var @{heatmap_id}_brush_x1;
			var @{heatmap_id}_brush_x2;
			var @{heatmap_id}_brush_y1;
			var @{heatmap_id}_brush_y2;
			$('#@{heatmap_id}_heatmap').mousedown(function(e) {
				var parentOffset = $(this).offset();
				@{heatmap_id}_brush_x1 = e.pageX - parentOffset.left;
				@{heatmap_id}_brush_y1 = e.pageY - parentOffset.top;
			}).mouseup(function(e) {
				var parentOffset = $(this).offset();
				@{heatmap_id}_brush_x2 = e.pageX - parentOffset.left;
				@{heatmap_id}_brush_y2 = e.pageY - parentOffset.top;
				if(@{heatmap_id}_brush_x1 != @{heatmap_id}_brush_x2 || @{heatmap_id}_brush_y1 != @{heatmap_id}_brush_y2) {
					$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
				}
			});	
		}
		$('#@{heatmap_id}_output_wrapper').mouseleave(function() {
			$('#@{heatmap_id}_output_wrapper div').html(default_message);
			$('#@{heatmap_id}_output_wrapper').css({'position':'absolute','right':-10000,'left':'auto'});
		})
	}
}
