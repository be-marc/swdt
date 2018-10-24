$.get("nav_right.html", function(data) {
    $(".navbar > .container-fluid").append(data);
  
});

$(document).ready(function() {
  $("body").on("click", "#about", function() {
  Shiny.setInputValue("nav_about", Math.random());
  });
});

$(document).ready(function() {
  $("body").on("click", "#imprint", function() {
  Shiny.setInputValue("nav_imprint", Math.random());
  });
});

$(document).ready(function() {
  $("body").on("click", "#data_protection", function() {
  Shiny.setInputValue("nav_data_protection", Math.random());
  });
});

$(document).ready(function() {
  $("body").on("click", "#restart_session", function() {
  Shiny.setInputValue("restart_session", Math.random());
  });
});








                   