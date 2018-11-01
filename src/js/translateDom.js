import walkDom from "./walkDom";
import translateToTraditional from "./translateToTraditional";

const translateDom = node =>
  walkDom(node, currentNode => {
    if (currentNode.nodeType === HTMLElement.TEXT_NODE) {
      currentNode.nodeValue = translateToTraditional(currentNode.nodeValue);
    }
  });

export default translateDom;
