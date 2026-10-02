// Cloud Run only (loaded when R_CONFIG_ACTIVE=cloudrun).
// Tells the server someone is using the page, so it can close idle sessions
// and let the service scale to zero. Sends at most one update every 30 seconds;
// activity inside that window is sent when the window ends, so none is lost.
(function () {
  var REPORT_EVERY_MS = 30 * 1000;
  var lastReport = 0;
  var pending = null;
  var events = ["pointerdown", "pointermove", "keydown", "wheel", "touchstart", "scroll"];

  function send() {
    pending = null;
    if (!window.Shiny || !Shiny.setInputValue) {
      return;
    }
    lastReport = Date.now();
    Shiny.setInputValue("cloud_activity", lastReport, { priority: "event" });
  }

  function report() {
    if (pending) {
      return;
    }
    var wait = REPORT_EVERY_MS - (Date.now() - lastReport);
    if (wait <= 0) {
      send();
    } else {
      pending = setTimeout(send, wait);
    }
  }

  events.forEach(function (name) {
    window.addEventListener(name, report, { capture: true, passive: true });
  });
})();
