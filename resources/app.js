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
      clients = parsedMessage.clients;
      names = Object.keys(clients).map(function (key) {
        return clients[key].name;
      });
      $("#clients").text(names.join(', '));
    }
    if ("log" in parsedMessage) {
      log = parsedMessage.log;
      date = new Date();
      name = Object.keys(log)[0];
      console.log(name);
      $('#log').append(htmlEncode(sprintf("[%s] %s: `%s`", moment().format(), name, log[name])) + "<br/>");
    }
  });

});