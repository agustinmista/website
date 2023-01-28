"use strict";

window.onload = function () {

  // Burger menu
  // Links are shown by default on wide windows
  const minWidth = 992;
  const burger = document.getElementById("burger");
  const links = document.getElementById("links");

  function showLinks() {
    links.style.display = "grid";
    burger.style.color = "var(--primary)"
  }

  function hideLinks() {
    links.style.display = "none";
    burger.style.color = "var(--secondary)"
  }

  function showBurgerButton() {
    burger.style.display = "block";
  }

  function hideBurgerButton() {
    burger.style.display = "none";
  }

  function updateLayout() {
    if (window.innerWidth >= minWidth) {
      hideBurgerButton();
      showLinks();
    } else {
      showBurgerButton();
      if (links.style.display !== "grid") {
        hideLinks();
      }
    }
  }

  burger.onclick = function () {
    links.style.display === "none" ? showLinks() : hideLinks();
  }

  window.onresize = function () {
    updateLayout();
  }

  updateLayout();
}