import walkDom from "./walkDom";
import Dictionary from "./Dictionary";

const translateDom = node =>
  walkDom(node, currentNode => {
    const dictionary = new Dictionary();
    if (currentNode.nodeType === HTMLElement.TEXT_NODE) {
      currentNode.nodeValue = dictionary.translateToTraditional();
    }
  });

export default translateDom;
