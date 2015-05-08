$(function () {
    "use strict";

    var content = $('#content');
    var input = $('#input');
    var socket = $.atmosphere;

    // We are now ready to cut the request
    var request = {
        url: '/chat-socket',
        contentType : 'application/text',
        transport : 'websocket',
        fallbackTransport: 'long-polling'
    };

    request.onOpen = function(response) {
        console.log("on open: "+response)

        content.html($('<p>', { text: 'connected using ' + response.transport }));

        subSocket.push(jQuery.stringifyJSON({ status: 'ready' }));
    };

    request.onMessage = function (response) {
        console.log("on message: "+response)

        var message = response.responseBody;
        try {
            var json = jQuery.parseJSON(message);
            if (json.message) {
                addMessage("server", json.message, "red", new Date());
            }
        } catch (e) {
            console.log('This doesn\'t look like a valid JSON: ', message);
        }
    };

    request.onClose = function(response) {
        console.log("closed connection: "+response)
    };

    request.onError = function(response) {
        console.log("on error: "+response)

        content.html($('<p>', { text: 'Sorry, but there\'s some problem with your '
            + 'socket or the server is down' }));
    };

    input.keydown(function(e) {
        if (e.keyCode === 13) {
            var msg = $(this).val();

            var json = {
                message: msg
            };

            addMessage("me", msg, "blue", new Date());

            subSocket.push(jQuery.stringifyJSON(json));
            $(this).val('');
        }
    });

    function addMessage(author, message, color, datetime) {
        content.append(
            '<p><span style="color:' + color + '">' + author + '</span> @ ' + +(datetime.getHours() < 10 ? '0' + datetime.getHours() : datetime.getHours()) + ':' + (datetime.getMinutes() < 10 ? '0' + datetime.getMinutes() : datetime.getMinutes()) + ': ' + message + '</p>');
    }

    var subSocket = socket.subscribe(request);
});