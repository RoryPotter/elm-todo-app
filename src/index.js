import { Elm } from "./Main.elm";

export var app = Elm.Main.init({ flags: localStorage.getItem("model") });

app.ports.setStorage.subscribe(data =>
  localStorage.setItem("model", JSON.stringify(data))
);
