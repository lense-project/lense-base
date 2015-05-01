$(function () {
    "use strict";

    var content = $('#content');
    var socket = $.atmosphere;

    // We are now ready to cut the request
    var request = {
        url: '/work-socket',
        contentType : 'application/text',
        transport : 'websocket',
        fallbackTransport: 'long-polling'
    };

    request.onOpen = function(response) {
        console.log("on open: ")
        console.log(response)

        content.html($('<p>', { text: 'connected using ' + response.transport }));
    };

    request.onMessage = function (response) {
        console.log("on message: ")
        console.log(response)

        var message = response.responseBody;
        try {
            var json = jQuery.parseJSON(message);
            console.log(json);
            if (json.type === "multiclass") {
                // We need to create a multiclass question here
                console.log("Handling a multiclass query");
            }
        } catch (e) {
            console.log('This doesn\'t look like a valid JSON: ', message);
        }

        content.html($('<p>', { text: message }))
    };

    request.onClose = function(response) {
        console.log("closed connection: ")
        console.log(response)
        logged = false;
    };

    request.onError = function(response) {
        console.log("on error: ")
        console.log(response)

        content.html($('<p>', { text: 'Sorry, but there\'s some problem with your '
            + 'socket or the server is down' }));
    };

    var subSocket = socket.subscribe(request);

    // subSocket.push(jQuery.stringifyJSON({ author: author, message: msg }));
});