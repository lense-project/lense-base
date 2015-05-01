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
        console.log("on open: "+response)

        content.html($('<p>', { text: 'connected using ' + response.transport }));

        subSocket.push(jQuery.stringifyJSON({ status: 'ready' }));
    };

    request.onMessage = function (response) {
        console.log("on message: "+response)

        var message = response.responseBody;
        try {
            var json = jQuery.parseJSON(message);
            if (json.type === "multiclass") {
                // We need to create a multiclass question here
                content.html(json.html+"<br>");

                for (var choiceID in json.choices) {
                    var choice = json.choices[choiceID];
                    var b = $('<button/>', {class: 'choice'});
                    b.html(choice);
                    content.append(b);

                    // I hate Javascript so much. I just can't even describe it.
                    b.click((function(closureChoice) {
                        return function() {
                            console.log("Choosing "+closureChoice);
                            subSocket.push(jQuery.stringifyJSON({ answer: closureChoice }));
                            content.html("Congratulations! You've finished everything. Waiting for next question from the server...");
                        }
                    })(choice));
                }
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

    var subSocket = socket.subscribe(request);
});