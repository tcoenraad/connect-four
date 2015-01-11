$(function() {
  var socket = eio.Socket();

  socket.on('open', function() {
    socket.send('echo! echo!');
  });

  socket.on('message', function(msg) {
    console.log('De server retourneerde: ' + msg);
    $('body').append('De server retourneerde: ' + msg);
  });

});