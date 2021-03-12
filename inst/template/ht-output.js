
$(function() {

	if('@{output_ui_float}' == 'TRUE') {
		var default_message = "Retrieving from server... Don't move mouse.";
		$('#@{heatmap_id}_output_wrapper').css({'position':'absolute','right':-10000,'left':'auto'});
	    if('@{action}' == "hover") {
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
		}
		if('@{action}' == "click") {
	    	$('#@{heatmap_id}_heatmap').click(function(e) {
		    	$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
		    	
		    });
		}
		if('@{action}' == "dblclick") {
	    	$('#@{heatmap_id}_heatmap').dblclick(function(e) {
		    	$('#@{heatmap_id}_output_wrapper').css({'position':'absolute', 'left':e.pageX, 'right':'auto', 'top':e.pageY});
		    });
		}

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

		$('#@{heatmap_id}_output_wrapper').mouseleave(function() {
			$('#@{heatmap_id}_output_wrapper div').html(default_message);
			$('#@{heatmap_id}_output_wrapper').css({'position':'absolute','right':-10000,'left':'auto'});
		})
	}
	
});


