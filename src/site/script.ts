///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{}>;
}

window.onload = () => {
  Elm.embed(Elm.Main, document.getElementById('main'));
  SvgConnectors.manage();
}