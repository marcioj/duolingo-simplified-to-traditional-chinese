import configuration from "./configuration.js";

const settings = ["traditional"];
settings.forEach(setting => {
  const checkboxElement = document.getElementById(`setting-${setting}`);

  configuration.getValue(setting, function(value) {
    if (value === true) {
      checkboxElement.checked = "checked";
    }
  });

  checkboxElement.onchange = () => {
    configuration.setValue(setting, checkboxElement.checked);
  };
});
