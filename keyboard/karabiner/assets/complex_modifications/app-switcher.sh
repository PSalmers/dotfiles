osascript -l JavaScript << EndOfScript
    var appChoice = [
            undefined,
            "Emacs",
            "Terminal",
            "Code",
            "Slack",
            "Chrome meet.google",
            "Chrome mail.google",
            "Chrome calendar.google",
	    "Chrome shopify.workplace",
        ][$1].split(" ")
    console.log("app choice " + appChoice)
    var appName = appChoice[0]
    var appTab = appChoice[1]

    if (!appName) throw "No app for hotkey $1";

    var app = Application(appName)

    app.activate();

 if(appTab) {
        for (win of app.windows()) {
            var tabIndex = win.tabs().findIndex(tab => tab.url().match(new RegExp(appTab)));
            if (tabIndex != -1) {
                win.activeTabIndex = (tabIndex + 1);
                win.index = 1;
            }
        }
    }
EndOfScript
