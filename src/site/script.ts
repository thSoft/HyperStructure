///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{}>;
}

interface JQueryStatic {
  contextMenu(type: string): JQuery;
}

window.onload = () => {
  Elm.embed(Elm.Main, document.getElementById('main'));
  SvgConnectors.manage();
  $.contextMenu("html5");
}