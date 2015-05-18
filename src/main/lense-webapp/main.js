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
    var ready = $('#ready');
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
        console.log('connected using ' + response.transport);

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
                var roundedBonus = Math.round((json.bonus+0.1) * 100) / 100;
                if (roundedBonus % 0.1 == 0) {
                    bonus.html("$"+roundedBonus+"0");
                }
                else {
                    bonus.html("$"+roundedBonus);
                }

                // Create animating bonus
                var bonusDiv = $('<div/>', {class: "bonus-ping"});
                bonusDiv.html("+$0.01");
                $("body").append(bonusDiv);
                bonusDiv.animate({
                    top: "0px",
                    "font-size": "7em",
                    opacity: "0"
                }, 1200, "swing", function() {
                    bonusDiv.remove();
                });
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
                        function to2Tokens(number) {
                            if (number < 10) return "0"+number;
                            else return ""+number;
                        }
                        retainer.html(to2Tokens(hours)+":"+to2Tokens(minutes)+":"+to2Tokens(seconds));
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
                content.css({
                    height: 'auto'
                });

                content.html(json.html+"<br>");

                var keys = [];
                var refKeys = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];

                for (var choiceID in json.choices) {
                    var choice = json.choices[choiceID];
                    var b = $('<button/>', {class: 'choice'});
                    b.html(choice);

                    var shortcut = $('<span/>', {class: 'key'});

                    var key = choice.toLowerCase().charAt(0);
                    if (!$.inArray(key, keys)) {
                        for (var i in refKeys) {
                            key = refKeys[i];
                            if (!$.inArray(key, keys)) break;
                        }
                    }
                    keys.push(key);

                    b.append(shortcut);
                    shortcut.html(key);

                    content.append(b);

                    function makeChoice(closureChoice) {
                        console.log("Choosing "+closureChoice);
                        subSocket.push(jQuery.stringifyJSON({ answer: closureChoice }));
                        content.css({
                            height: content.height()
                        });

                        content.html("Waiting for next question from the server...");


                        $(document).unbind("keypress");
                        $(document).unbind("keyup");
                    }

                    // I hate Javascript so much. I just can't even describe it.
                    var handler = $(document).keyup((function(closureChoice, closureKey) {
                        return function(e) {
                            console.log("Released "+e.which);
                            var index = 65+refKeys.indexOf(closureKey)
                            if (e.which == index) {
                                makeChoice(closureChoice);
                            }
                        }
                    })(choice, key));

                    // I hate Javascript so much. I just can't even describe it.
                    b.click((function(closureChoice) {
                        return function() {
                            makeChoice(closureChoice);
                        }
                    })(choice));

                    var handler = $(document).keydown((function(closureChoice, closureKey, closureButton) {
                        return function(e) {
                            console.log("Pressed "+e.which);
                            var index = 65+refKeys.indexOf(closureKey)
                            if (e.which == index) {
                                closureButton.addClass("hover");
                                $(document).unbind("keypress");
                            }
                        }
                    })(choice, key, b));
                }
            }
        } catch (e) {
            console.log(e);
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

    var subSocket;

    if (assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE") {
        ready.html("Accept the HIT to get started!");
        ready.addClass("disabled");
    }
    else {
        ready.click(function() {
            bonus.html("$0.10");

            var instructions = $("#instructions");
            var instructionsHeader = $("#instructions-header");
            instructionsHeader.animate({
                height: 0,
            }, 200, "swing", function() {
                instructionsHeader.remove();
            });
            ready.animate({
                height: 0,
            }, 200, "swing", function() {
                ready.remove();
            });

            setTimeout(function() {
                subSocket = socket.subscribe(request);
            }, 200);
        });
    }
});
