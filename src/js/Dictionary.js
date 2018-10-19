import { characters } from "./characters.js";

let parsedData;

const parseDataIfNeeded = () => {
  if (parsedData) return;
  parsedData = Object.create(null);
  for (let char in characters) {
    let [simplified, traditional, meaning, type, explanation] = characters[
      char
    ].split("|");
    parsedData[simplified] = {
      traditional,
      meaning,
      type,
      explanation
    };
  }
};

export default class Dictionary {
  constructor() {
    parseDataIfNeeded();
  }

  translateToTraditional(simplified) {
    return parsedData[simplified].traditional;
  }
}
