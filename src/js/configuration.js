const configuration = {
  setValue: (key, value, cb) => {
    chrome.storage.sync.set({ [key]: value }, cb);
  },
  getValue: (key, cb) => {
    chrome.storage.sync.get([key], settings => cb(settings[key]));
  },
  onChange: (key, cb) => {
    chrome.storage.onChanged.addListener(function(changes, namespace) {
      if (key in changes) {
        cb(changes[key], namespace);
      }
    });
  }
};

configuration.getValue("installed", installed => {
  if (!installed) {
    configuration.setValue("traditional", true);
    // set defaults
    configuration.setValue("installed", true);
  }
});

export default configuration;
