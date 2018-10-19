export default function observeMutations(node, cb) {
  const observer = new MutationObserver(mutations => {
    for (const mutation of mutations) {
      if (mutation.type == "childList") {
        for (const node of mutation.addedNodes) {
          cb(node);
        }
      }
    }
  });

  observer.observe(node, {
    childList: true,
    subtree: true
  });

  return observer;
}
