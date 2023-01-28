"use strict";

// Burger menu
// Links are shown by default on wide windows
const wideWindow = window.innerWidth >= 992;
const burger = document.getElementById("burger");
const links = document.getElementById("links");

hideLinks();

function showLinks() {
  burger.style.color = "var(--secondary)"
  links.style.display = "grid";
}

function hideLinks() {
  links.style.display = "none";
  burger.style.color = "var(--primary)"
}

function toggleLinks() {
  links.style.display === "none" ? showLinks() : hideLinks();
}

burger.onclick = function () {
  toggleLinks();
}

window.onload = function () {
  wideWindow ? showLinks() : hideLinks();
}