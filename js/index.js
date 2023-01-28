"use strict";

window.onload = function () {

  // Burger menu
  // Links are shown by default on wide windows
  const wideWindow = window.innerWidth >= 992;
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

  burger.onclick = function () {
    links.style.display === "none" ? showLinks() : hideLinks();
  }

  // Set the default layout depending on the window width
  wideWindow ? showLinks() : hideLinks();

}