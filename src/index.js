import { Elm } from "./Main.elm";

export var app = Elm.Main.init({ flags: localStorage.getItem("model") });

app.ports.storeModel.subscribe(model =>
  localStorage.setItem("model", JSON.stringify(model))
);

app.ports.updateModel.send(localStorage.getItem("model"));
