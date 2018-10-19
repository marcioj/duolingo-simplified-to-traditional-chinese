import { characters } from "./characters.js";

export class CharacterLookup {
  constructor() {
    this.dictionary = {};
    for (let x in characters) {
      let [simplified, traditional, meaning, type, explaination] = characters[
        x
      ].split("|");
      this.dictionary[simplified] = {
        meaning: meaning,
        type: type,
        explaination: explaination
      };
      this.dictionary[traditional] = {
        meaning: meaning,
        type: type,
        explaination: explaination
      };
    }
  }

  getMeaning(character) {
    return this.dictionary[character];
  }
}
