$(function () {
    "use strict";

    var content = $('#content');
    var socket = $.atmosphere;

    // We are now ready to cut the request
    var request = { url: 'http://127.0.0.1:8080/chat',
        contentType : "application/json",
        transport : 'websocket'};

    request.onOpen = function(response) {
        content.html($('<p>', { text: 'Atmosphere connected using ' + response.transport }));
    };

    request.onMessage = function (response) {
        var message = response.responseBody;
        try {
            var json = jQuery.parseJSON(message);
        } catch (e) {
            console.log('This doesn\'t look like a valid JSON: ', message);
        }

        content.html($('<p>', { text: message }))
    };

    request.onClose = function(response) {
        logged = false;
    }

    request.onError = function(response) {
        content.html($('<p>', { text: 'Sorry, but there\'s some problem with your '
            + 'socket or the server is down' }));
    };

    var subSocket = socket.subscribe(request);

    // subSocket.push(jQuery.stringifyJSON({ author: author, message: msg }));
});