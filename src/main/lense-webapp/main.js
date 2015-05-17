$.urlParam = function(name){
    var results = new RegExp('[\?&]' + name + '=([^&#]*)').exec(window.location.href);
    if (results==null){
       return null;
    }
    else{
       return decodeURIComponent(results[1]) || 0;
    }
}

$(function () {
    "use strict";

    var assignmentId = $.urlParam("assignmentId");
    var hitId = $.urlParam("hitId");
    var turkSubmitTo = $.urlParam("turkSubmitTo");
    var workerId = $.urlParam("workerId");

    var content = $('#content');
    var bonus = $('#bonus');
    var retainer = $('#retainer');
    var form = $("#successForm")
    form.attr("action", turkSubmitTo+"/mturk/externalSubmit");
    $("#assignmentId").val(assignmentId);
    $("#hitId").val(hitId);

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

        subSocket.push(jQuery.stringifyJSON({
            status: 'ready',
            assignmentId: assignmentId,
            hitId: hitId,
            workerId: workerId
        }));
    };

    request.onMessage = function (response) {
        console.log("on message: "+response)

        var message = response.responseBody;
        try {
            var json = jQuery.parseJSON(message);
            if (json['bonus'] !== undefined) {
                var roundedBonus = Math.round(json.bonus * 100) / 100;
                bonus.html("Your current bonus: $"+roundedBonus);
            }
            if (json['on-call-duration'] !== undefined) {
                var onCallDuration = json['on-call-duration'];
                var startTimeMillis = (new Date()).getTime();
                var interval = setInterval(function() {
                    var currentTimeMillis = (new Date()).getTime();
                    var elapsedMillis = currentTimeMillis - startTimeMillis;
                    var remainingMillis = onCallDuration - elapsedMillis;
                    if (remainingMillis < 0) {
                        retainer.html('');
                        var input = $('<button>Collect your earnings!</button>');
                        input.click(function() {
                            console.log("Turning in results");
                            subSocket.push(jQuery.stringifyJSON({ request: 'turn-in' }));
                        });
                        input.appendTo(retainer);
                        window.clearInterval(interval);
                    }
                    else {
                        var totalSeconds = Math.ceil(remainingMillis / 1000);
                        var totalMinutes = Math.floor(totalSeconds / 60);
                        var hours = Math.floor(totalMinutes / 60);
                        var seconds = totalSeconds % 60;
                        var minutes = totalMinutes % 60;
                        retainer.html("Remaining retainer: "+hours+":"+minutes+":"+seconds);
                    }
                }, 1000);
            }
            if (json['completion-code'] !== undefined) {
                retainer.html('');
                content.html("Thanks for participating! Your bonus will be approved within 30 seconds. This page will refresh in 3 seconds. DO NOT NAVIGATE AWAY FROM THIS PAGE BEFORE IT REFRESHES, OR YOU WILL NOT GET PAID.");
                setTimeout(function() {
                    var code = json['completion-code'];
                    console.log("Turning in final work! Using code "+code);
                    workComplete(code);
                }, 3000);
            }
            if (json['type'] !== undefined && json.type === "multiclass") {
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
                            content.html("Waiting for next question from the server...");
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

    function workComplete(code) {
        $("#completionCode").val(code);
        form.submit();
    }

    if (assignmentId != "ASSIGNMENT_ID_NOT_AVAILABLE") {
        var subSocket = socket.subscribe(request);
    }
});
