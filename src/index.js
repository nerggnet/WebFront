import { Elm } from './Main.elm';

var environment = process.env.NODE_ENV;
var mealsUrl = process.env.ELM_APP_MEALS_URL;

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    environment: environment,
    mealsUrl: mealsUrl,
    width: window.innerWidth,
    height: window.innerHeight
}
});
