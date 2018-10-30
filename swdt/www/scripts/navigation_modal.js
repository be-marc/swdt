$(document).ready(function() {
	document.querySelector("#navbar li a[data-value=processing]").addEventListener("click", function(){
		Shiny.onInputChange("nav_processing", Math.random())
	});
});

$(document).ready(function() {
	document.querySelector("#navbar li a[data-value=water_extent_minimum]").addEventListener("click", function(){
		Shiny.onInputChange("nav_water_extent_minimum", Math.random())
	});
});

$(document).ready(function() {
	document.querySelector("#navbar li a[data-value=water_extent_maximum]").addEventListener("click", function(){
		Shiny.onInputChange("nav_water_extent_maximum", Math.random())
	});
});

$(document).ready(function() {
	document.querySelector("#navbar li a[data-value=water_dynamic]").addEventListener("click", function(){
		Shiny.onInputChange("nav_water_dynamic", Math.random())
	});
});