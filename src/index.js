import { Elm } from './Main.elm';

var environment = process.env.NODE_ENV;
var mealsUrl = process.env.ELM_APP_MEALS_URL;
var jsonString = "{ \"environment\": \"" + environment + "\", \"mealsUrl\": \"" + mealsUrl + "\" }";
var flags = JSON.parse(jsonString);

Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});
