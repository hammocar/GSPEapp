Shiny.addCustomMessageHandler('addbutton', function(message) {
  var button = "<li class='list_button'><button type='button' class='btn btn-success' onclick='trigger_shiny(\"" + message.trigger + "\")'><i class='fa fa-plus'></i></button></li>";
  $("#" + message.id).first().prepend(button);
});

function trigger_shiny(trigger, value = 1) {
  Shiny.setInputValue(trigger, value, {priority: "event"});
}

$(document).on('shiny:value', function(event) {
  // Scroll down after model update
  if (event.target.id === 'model_record') {
    window.scrollTo(0,document.body.scrollHeight);
  }
});
