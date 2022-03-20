// Prevent default keyboard behavior from inteferring with the gameplay.
// E.g. if screen is too small, and the arrow keys would normally scroll.
let preventDefaults = function (e) {
  switch (e.code) {
    case "ArrowUp":
    case "ArrowDown":
    case "ArrowLeft":
    case "ArrowRight":
    case "Enter":
      e.preventDefault();
      break;
    default:
      break;
  }
};
window.addEventListener("keydown", preventDefaults, false);
