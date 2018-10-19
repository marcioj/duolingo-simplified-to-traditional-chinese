(function() {
  setTimeout(function() {
    document.body.classList.add("can-animate");
  }, 300);

  const settings = ["meanings", "traditional"];

  const configuration = (function() {
    function setValue(key, value) {
      let insert = {};
      insert[key] = value;
      chrome.storage.sync.set(insert, function() {});
    }
    function getValue(key, callback) {
      //  console.log(key)
      chrome.storage.sync.get(key, callback);
    }
    return { setValue: setValue, getValue: getValue };
  })();

  configuration.getValue("installed", result => {
    if (Object.keys(result).length == 0) {
      configuration.setValue("traditional", true);
      configuration.setValue("meanings", true);
      configuration.setValue("installed", true);
    } else {
    }
  });

  settings.forEach(setting => {
    const checkboxElement = document.getElementById(`setting-${setting}`);

    configuration.getValue(setting, function(value) {
      if (value[setting] == true) {
        checkboxElement.checked = "checked";
      }
    });

    checkboxElement.onchange = () => {
      configuration.setValue(setting, checkboxElement.checked);
    };
  });
})();
