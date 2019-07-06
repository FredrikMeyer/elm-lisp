const { Elm } = require('../elm/Main.elm');

import "./style.css";

var app = Elm.Main.init({
    node: document.getElementById("app")
});

window.onblur = function () {
    app.ports.blurs.send()
}


document.body.addEventListener('keydown', e => {
    if (e.metaKey && e.keyCode == 13) {
        app.ports.cmdEnter.send()
    }
})
