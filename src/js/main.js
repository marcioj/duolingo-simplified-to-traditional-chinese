import translateDom from "./translateDom";
import onPageLoad from "./onPageLoad";
import observeMutations from "./observeMutations";
import configuration from "./configuration";
import isLearningChinese from "./isLearningChinese";

let observer;

const install = () => {
  if (!isLearningChinese()) return;
  translateDom(document.body);
  observer = observeMutations(document.body, node => translateDom(node));
};

const uninstall = () => {
  if (observer) {
    observer.disconnect();
  }
};

onPageLoad(() => {
  configuration.getValue("traditional", traditional => {
    if (traditional) {
      install();
    }
  });
});

configuration.onChange("traditional", changes => {
  const traditional = changes.newValue;
  if (traditional) {
    install();
  } else {
    uninstall();
  }
});
