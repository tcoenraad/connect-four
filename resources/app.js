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
      var $clients = $("#clients");
      $clients.empty();
      $.each(names, function(i) {
        $('<li/>').text(names[i]).appendTo($clients);
      });
    }

    else if ("log" in parsedMessage) {
      var log = parsedMessage.log;
      var date = new Date();
      var name = Object.keys(log)[0];
      var $log = $('#log');
      $log.append(htmlEncode(sprintf("[%s] %s: `%s`", moment().format(), name, log[name])) + "<br/>");
      $log.stop().animate({scrollTop: $log[0].scrollHeight});
    }

    else if ("serverGames" in parsedMessage) {
      var serverGames = parsedMessage.serverGames;

      var $games = $('#games');
      $games.empty();
      serverGames.forEach(function(serverGame) {
        var $game = $('<div>').addClass('game');
        $game.append($('<h2/>').text('Player list'));
        var $playerList = $('<ul>').addClass('player-list').appendTo($game);

        var clients = serverGame[0];
        Object.keys(clients).map(function (key) {
          $('<li/>').text(clients[key].name).appendTo($playerList);
        });

        var game = serverGame[1];
        var board = game.board;
        var $board = $('<div>').addClass('board').appendTo($game);
        board.forEach(function(column) {
          var $column = $('<div>').appendTo($board);
          column.forEach(function(field) {
            $('<span/>').addClass(field.toLowerCase()).appendTo($column);
          });
        });
        $games.append($game);
      });
    }
  });

});