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
    // "use strict";

    // Get Worker information

    var assignmentId = $.urlParam("assignmentId");
    var hitId = $.urlParam("hitId");
    var turkSubmitTo = $.urlParam("turkSubmitTo");
    var workerId = $.urlParam("workerId");

    // Get DOM elements

    var content = $('#content');
    var bonus = $('#bonus');
    var ready = $('#ready');
    var retainer = $('#retainer');
    var form = $("#successForm")
    var returnForm = $("#returnForm")
    var trainingComments = $("#training-comments")
    var cheatSheet = $("#cheat-sheet")

    // Setup state to handle whether or not we're in a query right now

    var inQuery = false
    var timeouts = 0
    var use3Digits = false

    // Setup submit form

    form.attr("action", turkSubmitTo+"/mturk/externalSubmit");
    $("#assignmentId").val(assignmentId);
    $("#hitId").val(hitId);
    returnForm.attr("action", turkSubmitTo+"/mturk/return");
    $("#hitIdReturn").val(hitId);

    // Build socket

    var socket = $.atmosphere;

    // Build atmosphere request

    var request = new $.atmosphere.AtmosphereRequest();
    request.url = '/work-socket';
    request.contentType = 'application/text';
    request.transport = 'websocket';
    request.fallbackTransport = 'long-polling';
    request.shared = true; // make sure this is shared across windows / tabs

    // Setup on-open callback

    request.onOpen = function(response) {
        console.log('connected using ' + response.transport);

        subSocket.push(jQuery.stringifyJSON({
            status: 'ready',
            assignmentId: assignmentId,
            hitId: hitId,
            workerId: workerId
        }));
    };

    // Setup on-message callback

    request.onMessage = function (response) {
        var message = response.responseBody;

        console.log("on message: "+$.stringifyJSON(message))
        try {
            var json = jQuery.parseJSON(message);
            if (json['status'] !== undefined) {
                if (json['status'] === 'failure') {
                    content.html(json['display']);
                    socket.unsubscribe();
                }
                if (json['status'] === 'timeout') {
                    content.html(json['display']);
                    socket.unsubscribe();
                }
                if (json['status'] === 'waiting') {
                    var numHere = json['here'];
                    var numNeeded = json['needed'];
                    var waitingDiv = $('<div/>', {class: "waiting"});
                    waitingDiv.html("We're <b>waiting for "+numNeeded+" people</b> to accept this HIT before we start.<br>"+
                    "So far <b>there are "+numHere+"/"+numNeeded+" here.</b><br>"+
                    "<b>You'll still get paid</b> your retainer even if we never get "+numNeeded+" people.<br>"+
                    "Feel free to do something else for a bit, <b>we'll alert you</b> when there are tasks available.");
                    content.html("");
                    content.append(waitingDiv);
                }
            }
            // The current query was cancelled
            if (json['cancelled'] !== undefined) {
                $(document).unbind("keyup");
                $(document).unbind("keydown");
                content.html("Took too long answering query. Server revoked. Waiting for next question to answer.");
            }
            if (json['bonus'] !== undefined) {
                if (use3Digits) {
                    var roundedBonus = Math.round(json.bonus * 1000) / 1000;
                    var bonusString = "$"+roundedBonus;
                    if (bonusString.length === 3) {
                        bonus.html(bonusString+"000");
                    }
                    else if (bonusString.length === 4) {
                        bonus.html(bonusString+"00");
                    }
                    else if (bonusString.length === 5) {
                        bonus.html(bonusString+"0");
                    }
                    else {
                        bonus.html(bonusString);
                    }
                }
                else {
                    var roundedBonus = Math.round(json.bonus * 100) / 100;
                    var bonusString = "$"+roundedBonus;
                    if (bonusString.length === 3) {
                        bonus.html(bonusString+"00");
                    }
                    else if (bonusString.length === 4) {
                        bonus.html(bonusString+"0");
                    }
                    else {
                        bonus.html(bonusString);
                    }
                }

                // Create animating bonus
                var bonusDiv = $('<div/>', {class: "bonus-ping"});
                // bonusDiv.html("+0.25&#162;");
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
                        /*
                        var retainerContainer = $("#retainer-container");
                        retainerContainer.html('');
                        var input = $('<button class="collect">Collect your earnings!</button>');
                        input.css({
                            "z-index": 100,
                            "position": "relative"
                        });
                        input.click(function() {
                            console.log("Turning in results");
                            subSocket.push(jQuery.stringifyJSON({ request: 'turn-in' }));
                        });
                        */

                        console.log("Turning in results");
                        subSocket.push(jQuery.stringifyJSON({ request: 'turn-in' }));

                        input.appendTo(retainerContainer);
                        window.clearInterval(interval);
                    }
                    else {
                        var totalSeconds = Math.ceil(remainingMillis / 1000);
                        var totalMinutes = Math.floor(totalSeconds / 60);
                        var hours = Math.floor(totalMinutes / 60);
                        var seconds = totalSeconds % 60;
                        var minutes = totalMinutes % 60;
                        var to2Tokens = function(number) {
                            if (number < 10) return "0"+number;
                            else return ""+number;
                        }
                        retainer.html(to2Tokens(hours)+":"+to2Tokens(minutes)+":"+to2Tokens(seconds));
                    }
                }, 1000);
            }
            if (json['completion-code'] !== undefined) {
                retainer.html('');
                var code = json['completion-code'];
                console.log("Turning in final work! Using code "+code);
                workComplete(code);
            }
            if (json['type'] !== undefined && json.type === "training") {
                var examples = json.examples;
                var filteredExamples = [];
                for (var j in examples) {
                    if (examples[j].type == "cheat-sheet") {
                        cheatSheet.html("<h3>Cheat Sheet:</h3>"+examples[j].html);
                    }
                    else {
                        filteredExamples.push(examples[j]);
                    }
                }
                runThroughExamples(filteredExamples, 0, null);
            }
            if (json['type'] !== undefined && json.type === "multiclass") {
                // This only happens if we're overwriting the current query
                if (inQuery) {
                    timeouts = timeouts + 1;
                    console.log("Overwriting query");
                    // Create animating punishment
                    var timeoutDiv = $('<div/>', {class: "timeout-ping"});
                    timeoutDiv.html("Took too long, missed "+timeouts+"/3 in a row");
                    $("body").append(timeoutDiv);
                    timeoutDiv.animate({
                        top: "0px",
                        "font-size": "5em",
                        opacity: "0"
                    }, 4000, "swing", function() {
                        timeoutDiv.remove();
                    });
                }

                inQuery = true;
                renderMulticlassQuery(json, function(closureChoice) {
                    console.log("Choosing "+closureChoice);
                    subSocket.push(jQuery.stringifyJSON({ answer: closureChoice }));
                    content.css({
                        height: content.height()
                    });

                    timeouts = 0;

                    inQuery = false;
                    content.html("...");
                    setTimeout(function() {
                        if (!inQuery) {
                            content.html("Waiting for the other Turkers participating in this HIT to catch up with you...<br><br>Good work on finishing so quickly!<br><br>(We'll alert you when your co-workers are all caught up. Feel free to go do something else for a few seconds.)");
                        }
                    }, 1000);
                });
            }
        } catch (e) {
            console.log(e);
            console.log('This doesn\'t look like a valid JSON: ', message);
        }
    };

    request.onClose = function(response) {
        console.log("closed connection: "+response)

        trainingComments.removeClass("comments");
        trainingComments.html("");
    };

    request.onError = function(response) {
        console.log("on error: "+response)

        content.html($('<p>', { text: 'Sorry, but there\'s some problem with your '
            + 'socket or the server is down. We\'re going to pay you for the work you did so far, minus the retainer. Turning in HIT in 6 seconds.' }));

        trainingComments.removeClass("comments");
        trainingComments.html("");

        setTimeout(function() {
            workComplete('timeout');
        }, 6000);
    };

    // This will run through examples in a non-blocking way

    function runThroughExamples(examples, i, lastAnswer) {
        var header = $("<div/>");
        header.html("<b>TUTORIAL "+(i+1)+"/"+(examples.length+1)+":</b><br>");

        // Display a welcome banner

        if (i < examples.length && examples[i].type === "introduction") {
            content.html("");
            trainingComments.addClass("comments");
            trainingComments.html("<br><b>Task Description:</b><div>"+examples[i].html+"</div><br>We're going to run through "+examples.length+" examples to warm up.<br>"+
            "<b>Press any key</b> to get started, or click ");
            var b = $('<button/>', {class: 'choice'});
            b.html("get started");
            trainingComments.append(b);
            trainingComments.prepend(header);

            $(document).keyup(function() {
                $(document).unbind("keyup");
                runThroughExamples(examples, i+1, null);
            });
            b.click(function() {
                $(document).unbind("keyup");
                runThroughExamples(examples, i+1, null);
            });
        }

        // Display actual examples

        else if (i < examples.length && examples[i].type === "multiclass") {
            var displayComments = examples[i].comments;
            if (lastAnswer != null) {
                displayComments = "<span class='incorrect'>The answer \""+lastAnswer+"\" is incorrect.</span> Please try again. The hint was: <br>"+displayComments;
            }
            trainingComments.html(displayComments);
            trainingComments.prepend(header);

            renderMulticlassQuery(examples[i], function(closureChoice) {
                if (closureChoice == examples[i].answer) {
                    runThroughExamples(examples, i+1, null);
                }
                else {
                    runThroughExamples(examples, i, closureChoice);
                }
            });
        }

        // We're finished with examples, do the real thing!

        else {
            content.html("");
            trainingComments.html("<b>Congratulations!</b>. You're done with the tutorial!<br>"+
            "<b>Press any key</b> to get started earning <b>real money</b>, or click ");
            trainingComments.addClass("comments");
            var b = $('<button/>', {class: 'choice'});
            b.html("get started");
            trainingComments.append(b);
            trainingComments.prepend(header);

            var start = function() {
                $(document).unbind("keyup");
                trainingComments.removeClass("comments");
                trainingComments.html("");
                subSocket.push(jQuery.stringifyJSON({ status: "completed-training"}));
            }

            $(document).keyup(function() {
                start();
            });
            b.click(function() {
                start();
            });
        }
    }

    // This will handle creating, rendering, and setting up input-hooks for multiclass questions

    function renderMulticlassQuery(json, callback) {
        if (!document.hasFocus()) {
            alert("There's a task available for you now");
        }

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

            var makeChoice = function(closureChoice) {
                $(document).unbind("keydown");
                $(document).unbind("keyup");

                callback(closureChoice);
            }

            // I hate Javascript so much. I just can't even describe it.
            var handler = $(document).keyup((function(closureChoice, closureKey) {
                return function(e) {
                    var index = 65+refKeys.indexOf(closureKey)
                    if (e.which == index) {
                        makeChoice(closureChoice);
                    }
                }
            })(choice, key));

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

    function workComplete(code) {
        content.html($('<p>', { text: 'Received a completion code from the server! Thanks for all your hard work. Turning in HIT in 3 seconds.' }));
        $("#completionCode").val(code);
        setTimeout(function() {
            form.submit();
        }, 3000);
    }

    var subSocket;

    if (assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE") {
        ready.html("Accept the HIT to get started!");
        ready.addClass("disabled");
    }
    else {
        ready.click(function() {
            bonus.html("$1.00");

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
