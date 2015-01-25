function htmlEncode(val) {
  return $('<div/>').text(val).html();
}

$(function() {
  var socket = eio.Socket();

  socket.on('open', function() {
    socket.send('connected');
  });

  socket.on('message', function(msg) {
    var parsedMessage = $.parseJSON(msg);
    if ("clients" in parsedMessage) {
      var clients = parsedMessage.clients;
      var names = Object.keys(clients).map(function (key) {
        return clients[key].name;
      });
      $clients = $("#clients");
      $clients.empty();
      $.each(names, function(i) {
        var li = $('<li/>').text(names[i]).appendTo($clients);
      });
    }
    else if ("log" in parsedMessage) {
      var log = parsedMessage.log;
      var date = new Date();
      var name = Object.keys(log)[0];
      $('#log').append(htmlEncode(sprintf("[%s] %s: `%s`", moment().format(), name, log[name])) + "<br/>");
    }
  });

});