///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{
    focus: PortFromElm<string>;
  }>;
}

interface JQueryStatic {
  contextMenu(type: string): JQuery;
}

window.onload = () => {
  var elm = Elm.embed(Elm.Main, document.getElementById('main'));
  SvgConnectors.manage();
  $.contextMenu("html5");
  elm.ports.focus.subscribe((id) => {
    setTimeout(() => { // XXX
      var node = document.getElementById(id);
      if (node != null) {
        node.focus();
        setCaretPosition(node, 1);
      }
    }, 50);
  });
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