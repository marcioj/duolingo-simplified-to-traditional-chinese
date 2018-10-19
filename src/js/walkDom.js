const slice = Array.prototype.slice;

function walkDom(nodes, cb) {
  if ("nodeType" in nodes) {
    nodes = [nodes];
  }
  nodes = slice.call(nodes);

  while (nodes.length) {
    const node = nodes.shift();

    cb(node);

    if (node.childNodes && node.childNodes.length) {
      nodes = slice.call(node.childNodes).concat(nodes);
    }
  }
}

export default walkDom;
