import { characters } from "./characters.js";
import Dictionary from "./Dictionary";
import Sizzle from "sizzle";

let characterLookup = new Dictionary();

const configuration = {};

function settingsChanged() {
  if (configuration["meanings"] == true) {
    document.body.classList.add("show-meanings");
  } else {
    document.body.classList.remove("show-meanings");
  }
}

chrome.storage.sync.get("traditional", function(result) {
  configuration["traditional"] = result.traditional;
  settingsChanged();
});

chrome.storage.sync.get("meanings", function(result) {
  configuration["meanings"] = result.meanings;
  settingsChanged();
});

chrome.storage.onChanged.addListener(function(changes) {
  configuration[Object.keys(changes)[0]] =
    changes[Object.keys(changes)[0]].newValue;
  settingsChanged();
});

export class Duolingo {
  static isLearningChinese() {
    const state = JSON.parse(localStorage.getItem("duo.state"));
    return state.user.courseId.match(/ZH\-CN_EN/i) != null;
  }

  static insertCharacter(simplified, traditional) {
    let character = traditional;
    setTimeout(function() {
      let singleChalengeElement = document.querySelector(
        '[data-test="challenge-header"] + span > div > div'
      );
      if (singleChalengeElement !== null) {
        if (singleChalengeElement.childElementCount == 2) {
          const meaning = characterLookup.getMeaning(character);
          if (meaning !== undefined) {
            const div = document.createElement("div");
            const explaination =
              meaning.explaination == undefined ? " " : meaning.explaination;
            const type = meaning.type == undefined ? " " : meaning.type;
            const englishTranslation =
              meaning.meaning == undefined ? " " : meaning.meaning;
            div.innerHTML = `<div class="meaning"><div class="english">${englishTranslation}</div><div class="type">${type}</div><div class="explaination">${explaination}</div></div>`;
            singleChalengeElement.appendChild(div);
          }
        }
      } else {
      }
    }, 500);
    if (configuration["traditional"] == false) {
      return;
    }
    if (simplified == traditional) {
      return;
    }
    if (simplified == undefined || simplified == "undefined") {
      return;
    }

    const element = Sizzle(`:contains(${simplified})`).slice(-1)[0];

    if (element) {
      element.innerHTML = element.innerHTML.replace(simplified, traditional);
    }
  }

  static checkForChineseCharactersOnLoad() {
    if (!Duolingo.isLearningChinese()) {
      return;
    }
    setTimeout(function() {
      characters.forEach((x, y) => {
        let [simplified, traditional] = x.split("|");
        setTimeout(function() {
          const element = Sizzle(`:contains('${simplified}')`);
          if (element.length != 0) {
            Duolingo.insertCharacter(simplified, traditional);
          }
        }, 10);
      });
    }, 1000);
  }
}
