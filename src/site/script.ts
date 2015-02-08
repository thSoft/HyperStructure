///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{}>;
}

window.onload = () => {
  Elm.fullscreen(Elm.Main);
  SvgConnectors.manage();
}