///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{
    charCodes: PortToElm<number>;
  }>;
}

window.onload = () => {
  var elm = Elm.embed(Elm.Main, document.getElementById('main'), {
    charCodes: 0
  });
  SvgConnectors.manage();
  $(document).bind("keypress", event => {
    elm.ports.charCodes.send(event.charCode);
  });
  $.contextMenu("html5");
}