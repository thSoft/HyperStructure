///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{
    charCodes: PortToElm<number>;
  }>;
}

window.onload = () => {
  var mainElement = document.getElementById("main");
  var elm = Elm.embed(Elm.Main, mainElement, {
    charCodes: 0
  });
  SvgConnectors.manage();
  $(document).bind("keypress", event => {
    elm.ports.charCodes.send(event.charCode);
  });
  var observer = new MutationObserver(mutations => {
    mutations.forEach(mutation => {
      MiscUtils.toArray(mutation.addedNodes).forEach(handleAutofocus);
    });
  });
  observer.observe(mainElement, {
    childList: true,
    subtree: true
  });
  $.contextMenu("html5");
}

function handleAutofocus(node: Node) {
  if (node instanceof HTMLInputElement) {
    var element = <HTMLInputElement>node;
    if (element.autofocus) {
      element.focus();
      var position = 1;
      element.setSelectionRange(position, position);
    }
  } else {
    MiscUtils.toArray(node.childNodes).forEach(handleAutofocus);
  }
}