///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  Main: ElmModule<{
    charCodes: PortToElm<number>;
  }>;
}

window.onload = () => {
  const mainElement = document.getElementById("main");
  const elm = Elm.embed(Elm.Main, mainElement, {
    charCodes: 0
  });
  $(document).bind("keypress", event => {
    elm.ports.charCodes.send(event.charCode);
  });
  const observer = new MutationObserver(mutations => {
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
    const element = <HTMLInputElement>node;
    if (element.autofocus) {
      element.focus();
      const position = 1;
      element.setSelectionRange(position, position);
    }
  } else {
    MiscUtils.toArray(node.childNodes).forEach(handleAutofocus);
  }
}