import { Elm } from "./Main.elm";

export var app = Elm.Main.init({ flags: localStorage.getItem("model") });
