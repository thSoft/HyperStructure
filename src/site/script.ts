///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{
    charCodes: PortToElm<number>;
  }>;
}

interface JQueryStatic {
  contextMenu(type: string): JQuery;
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

function setCaretPosition(ctrl, pos) {
  if (ctrl.setSelectionRange) {
    ctrl.focus();
    ctrl.setSelectionRange(pos, pos);
  } else if (ctrl.createTextRange) {
    var range = ctrl.createTextRange();
    range.collapse(true);
    range.moveEnd('character', pos);
    range.moveStart('character', pos);
    range.select();
  }
}