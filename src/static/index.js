const { Elm } = require('../elm/Main.elm');

import "./style.css";

var app = Elm.Main.init({
    node: document.getElementById("app")
});
