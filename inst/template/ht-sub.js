
$(function() {

	$("#@{heatmap_id}_sub_heatmap_control").width($("#@{heatmap_id}_sub_heatmap").width());

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

			$('#@{heatmap_id}_sub_heatmap_input_width').val(ui.size.width - 4);
			$('#@{heatmap_id}_sub_heatmap_input_height').val(ui.size.height - 4);

			if(parseInt($('#@{heatmap_id}_sub_heatmap_download_format').find('input').filter(':checked').val()) == 2) {
				width_in_inch = Math.round((ui.size.width - 4)*10/100*4/3)/10;
				height_in_inch = Math.round((ui.size.height - 4)*10/100*4/3)/10;
				$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width_in_inch);
				$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height_in_inch);
			} else {
				$('#@{heatmap_id}_sub_heatmap_download_image_width').val(ui.size.width - 4);
				$('#@{heatmap_id}_sub_heatmap_download_image_height').val(ui.size.height - 4);
			}
		},
		zIndex: 0,
		containment: "parent"
	});
	
	
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
	

	Shiny.addCustomMessageHandler('@{heatmap_id}_sub_initialized', function(message) {
		if(message == "on") {
			$('#@{heatmap_id}_sub_heatmap_control').css("display", "block");
		} else {
			$('#@{heatmap_id}_sub_heatmap_control').css("display", "none");
		}
	});

	
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
