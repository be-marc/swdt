Shiny.addCustomMessageHandler('close', function(id) {
  console.log('Hello');
  document.getElementById('help_text_' + id + '0-collapse').classList.remove('in');
});