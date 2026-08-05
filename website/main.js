/* Progressive enhancement only. The site is fully usable without JavaScript:
   navigation, expandable notes (<details>), and all content work unaided.
   This script adds three conveniences. */
(function () {
  "use strict";

  /* 1. Close the mobile menu after a link is chosen, and on outside click. */
  var mobile = document.querySelector(".nav-mobile");
  if (mobile) {
    mobile.querySelectorAll("a").forEach(function (a) {
      a.addEventListener("click", function () { mobile.removeAttribute("open"); });
    });
    document.addEventListener("click", function (e) {
      if (mobile.hasAttribute("open") && !mobile.contains(e.target)) {
        mobile.removeAttribute("open");
      }
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") { mobile.removeAttribute("open"); }
    });
  }

  /* 2. Highlight the nav item for the section currently in view. */
  var links = Array.prototype.slice.call(document.querySelectorAll(".nav-desktop a[href^='#']"));
  var byId = {};
  links.forEach(function (l) { byId[l.getAttribute("href").slice(1)] = l; });
  var sections = Object.keys(byId)
    .map(function (id) { return document.getElementById(id); })
    .filter(Boolean);

  if ("IntersectionObserver" in window && sections.length) {
    var obs = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        var link = byId[entry.target.id];
        if (!link) return;
        if (entry.isIntersecting) {
          links.forEach(function (l) { l.classList.remove("active"); l.removeAttribute("aria-current"); });
          link.classList.add("active");
          link.setAttribute("aria-current", "true");
        }
      });
    }, { rootMargin: "-45% 0px -50% 0px", threshold: 0 });
    sections.forEach(function (s) { obs.observe(s); });
  }

  /* 3. Copy-citation button. The citation text is always visible, so this is
        purely a convenience; the button is hidden unless clipboard is available. */
  var btn = document.querySelector(".copy-btn");
  if (btn && navigator.clipboard) {
    btn.hidden = false;
    btn.addEventListener("click", function () {
      var target = document.querySelector(btn.getAttribute("data-copy-target"));
      if (!target) return;
      var text = target.textContent.replace(/\s+/g, " ").trim();
      navigator.clipboard.writeText(text).then(function () {
        var original = btn.textContent;
        btn.textContent = "Copied ✓";
        btn.classList.add("copied");
        setTimeout(function () { btn.textContent = original; btn.classList.remove("copied"); }, 2000);
      });
    });
  }
})();
