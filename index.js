import { Elm } from "./src/elm/Main.elm";
import "./src/static/style.css";

var app = Elm.Main.init({
  node: document.getElementById("app")
});


document.body.addEventListener('keydown', e => {
  if (e.metaKey && e.keyCode == 13) {
    app.ports.cmdEnter.send()
  }
})
