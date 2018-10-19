import { characters, charactersVersion } from "./characters.js";

export class Cache {
  static setCharacter(simplifiedCharacter, realCharacter) {
    localStorage.setItem(simplifiedCharacter, realCharacter);
  }

  static updateLocalStorage() {
    if (
      localStorage.getItem("version") == null ||
      parseFloat(localStorage.getItem("version")) < charactersVersion
    ) {
      localStorage.clear();
      console.log("clear cache");
      localStorage.setItem("version", charactersVersion);
      characters.forEach(x => {
        let [simplified, traditional] = x.split("|");
        localStorage.setItem(simplified, traditional);
      });
    }
  }
}
