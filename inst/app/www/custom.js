(function () {
  function scrollHelpToAnchor(anchorId) {
    if (!anchorId) return;
    var container = document.querySelector(".help-content");
    var target = document.getElementById(anchorId);
    if (!container || !target) return;

    var containerTop = container.getBoundingClientRect().top;
    var targetTop = target.getBoundingClientRect().top;
    var scrollTop = container.scrollTop + (targetTop - containerTop) - 8;
    container.scrollTo({ top: scrollTop, behavior: "smooth" });
  }

  document.addEventListener("click", function (e) {
    var link = e.target && e.target.closest ? e.target.closest("a.toc-link") : null;
    if (!link) return;
    var href = link.getAttribute("href") || "";
    if (!href.startsWith("#")) return;
    e.preventDefault();
    scrollHelpToAnchor(href.slice(1));
  });

  function registerHandlers() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return false;
    window.Shiny.addCustomMessageHandler("ocs-scroll", function (msg) {
      if (!msg) return;
      scrollHelpToAnchor(msg.anchor);
    });
    return true;
  }

  if (!registerHandlers()) {
    document.addEventListener("shiny:connected", registerHandlers);
  }
})();

